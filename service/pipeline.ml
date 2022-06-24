open Current.Syntax
open Ocaml_ci

module Git = Current_git
module Github = Current_github
module Docker = Current_docker.Default
module Matrix = Matrix

let platforms =
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) () in
  let v { Conf.label; builder; pool; distro; ocaml_version; arch; opam_version } =
    let base = Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version ~opam_version in
    let host_base =
      match arch with
      | `X86_64 -> base
      | _ -> Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro ~ocaml_version ~opam_version
    in
    Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version ~host_base ~opam_version base
  in
  let v2_0 = Conf.platforms `V2_0 in
  let v2_1 = Conf.platforms `V2_1 in
  Current.list_seq (List.map v (v2_0 @ v2_1))

let opam_repository_commit =
  let repo = { Github.Repo_id.owner = "ocaml"; name = "opam-repository" } in
  Github.Api.Anonymous.head_of repo @@ `Ref "refs/heads/master"

let github_status_of_state ~head result forge_url forge_url_variant results =
  let+ head = head
  and+ result = result
  and+ results = results in
  let { Github.Repo_id.owner; name } = Github.Api.Commit.repo_id head in
  let hash = Github.Api.Commit.hash head in
  let url = forge_url ~owner ~name ~hash in
  let pp_status f = function (variant, (build, _job_id)) ->
    let job_url = forge_url_variant ~owner ~name ~hash ~variant in
    match build with
      | Ok `Checked | Ok `Built->
        Fmt.pf f "%s [%s (%s)](%s)" "✅" variant "passed" job_url
      | Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m ->
        Fmt.pf f "%s [%s (%s)](%s)" "¯\\_(ツ)_/¯" variant "skipped" job_url
      | Error `Msg m ->
        Fmt.pf f "%s [%s (%s)](%s)" "❌" variant ("failed: " ^ m) job_url
      | Error `Active _ ->
        Fmt.pf f "%s [%s (%s)](%s)" "🟠" variant "active" job_url in
  let summary = Fmt.str "@[<v>%a@]" (Fmt.list ~sep:Fmt.cut pp_status)
      (List.sort (fun (x, _) (y,_) -> String.compare x y) results) in
  match result with
  | Ok _ ->
     Github.Api.CheckRunStatus.v ~url (`Completed `Success) ~summary
  | Error (`Active _) ->
     Github.Api.CheckRunStatus.v ~url `Queued ~summary
  | Error (`Msg m) when Astring.String.is_prefix ~affix:"[SKIP]" m ->
     Github.Api.CheckRunStatus.v ~url (`Completed (`Skipped m)) ~summary
  | Error (`Msg m) ->
     Github.Api.CheckRunStatus.v ~url (`Completed (`Failure m)) ~summary

let set_active_installations installations =
  let+ installations = installations in
  
  installations
  |> List.fold_left (fun acc i -> 
         let owner_id = {Index.Owner_id.name = (Github.Installation.account i); git_forge = Repo_id.GitHub } in
      Index.Owner_set.add owner_id acc) Index.Owner_set.empty
  |> Index.Owner_set.union (Index.get_active_owners ())
  |> Index.set_active_owners;

  installations

let set_active_repos ~installation repos =
  let+ installation = installation
  and+ repos = repos in
  let owner = Github.Installation.account installation in
  let owner_id = {Index.Owner_id.name = owner; git_forge = Repo_id.GitHub} in
  repos
  |> List.fold_left (fun acc r -> 
      Index.Repo_set.add (Github.Api.Repo.id r).name acc) Index.Repo_set.empty
  |> Index.set_active_repos ~owner_id;
  repos

let set_active_refs ~repo xs =
  let+ repo = repo
  and+ xs = xs in
  let github_repo = Github.Api.Repo.id repo in
  let repo = { Repo_id.owner = github_repo.owner; name = github_repo.name; git_forge = Repo_id.GitHub} in
  Index.set_active_refs ~repo (
    xs |> List.fold_left (fun acc x ->
        let commit = Github.Api.Commit.id x in
        let gref = Git.Commit_id.gref commit in
        let hash = Git.Commit_id.hash commit in
        Index.Ref_map.add gref hash acc
      ) Index.Ref_map.empty
  );
  xs

let get_job_id x =
  let+ md = Current.Analysis.metadata x in
  match md with
  | Some { Current.Metadata.job_id; _ } -> job_id
  | None -> None

let build_with_docker ?ocluster ~(repo : Repo_id.t Current.t) ~analysis source =
  Current.with_context analysis @@ fun () ->
  let specs =
    let+ analysis = Current.state ~hidden:true analysis in
    match analysis with
    | Error _ ->
        (* If we don't have the analysis yet, just use the empty list. *)
        []
    | Ok analysis ->
      match Analyse.Analysis.selections analysis with
      | `Opam_monorepo builds ->
        let lint_selection = Opam_monorepo.selection_of_config (List.hd builds) in
        Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt)
        :: List.map (fun config -> Spec.opam_monorepo ~config) builds
      | `Opam_build selections ->
        let lint_selection = List.hd selections in
        let builds =
          selections
          |> Selection.filter_duplicate_opam_versions
          |> List.map (fun selection ->
               let label = Variant.to_string selection.Selection.variant in
               Spec.opam ~label ~selection ~analysis `Build
             )
        and lint =
          [
            Spec.opam ~label:"(lint-fmt)" ~selection:lint_selection ~analysis (`Lint `Fmt);
            Spec.opam ~label:"(lint-doc)" ~selection:lint_selection ~analysis (`Lint `Doc);
            Spec.opam ~label:"(lint-opam)" ~selection:lint_selection ~analysis (`Lint `Opam);
          ]
        in
        lint @ builds
  in
  let builds = specs |> Current.list_map (module Spec) (fun spec ->
      let+ result =
        match ocluster with
        | None -> Build.v ~platforms ~repo ~spec source
        | Some ocluster ->
          let src = Current.map Git.Commit.id source in
          Cluster_build.v ocluster ~platforms ~repo ~spec src
      and+ spec = spec in
      Spec.label spec, result
    ) in
  let+ builds = builds
  and+ analysis_result = Current.state ~hidden:true (Current.map (fun _ -> `Checked) analysis)
  and+ analysis_id = get_job_id analysis in
  builds @ [
    "(analysis)", (analysis_result, analysis_id);
  ]

let list_errors ~ok errs =
  let groups =  (* Group by error message *)
    List.sort compare errs |> List.fold_left (fun acc (msg, l) ->
        match acc with
        | (m2, ls) :: acc' when m2 = msg -> (m2, l :: ls) :: acc'
        | _ -> (msg, [l]) :: acc
      ) []
  in
  Error (`Msg (
      match groups with
      | [] -> "No builds at all!"
      | [ msg, _ ] when ok = 0 -> msg (* Everything failed with the same error *)
      | [ msg, ls ] -> Fmt.str "%a failed: %s" Fmt.(list ~sep:(any ", ") string) ls msg
      | _ ->
        (* Multiple error messages; just list everything that failed. *)
        let pp_label f (_, l) = Fmt.string f l in
        Fmt.str "%a failed" Fmt.(list ~sep:(any ", ") pp_label) errs
    ))

let summarise results =
  results |> List.fold_left (fun (ok, pending, err, skip) -> function
      | _, Ok `Checked -> (ok, pending, err, skip)  (* Don't count lint checks *)
      | _, Ok `Built -> (ok + 1, pending, err, skip)
      | l, Error `Msg m when Astring.String.is_prefix ~affix:"[SKIP]" m -> (ok, pending, err, (m, l) :: skip)
      | l, Error `Msg m -> (ok, pending, (m, l) :: err, skip)
      | _, Error `Active _ -> (ok, pending + 1, err, skip)
    ) (0, 0, [], [])
  |> fun (ok, pending, err, skip) ->
  if pending > 0 then Error (`Active `Running)
  else match ok, err, skip with
    | 0, [], skip -> list_errors ~ok:0 skip (* Everything was skipped - treat skips as errors *)
    | _, [], _ -> Ok ()                     (* No errors and at least one success *)
    | ok, err, _ -> list_errors ~ok err     (* Some errors found - report *)

let local_test ~solver repo () =
  let src = Git.Local.head_commit repo in
  let repo = Current.return { Repo_id.owner = "local"; name = "test"; git_forge = GitHub  }
  and analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  Current.component "summarise" |>
  let> results = build_with_docker ~repo ~analysis src in
  let result =
    results
    |> List.map (fun (variant, (build, _job)) -> variant, build)
    |> summarise
  in
  Current_incr.const (result, None)

let github_pipeline ?ocluster ?matrix ~app ~solver () =
  let installations = Github.App.installations app |> set_active_installations in
  installations |> Current.list_iter ~collapse_key:"org" (module Github.Installation) @@ fun installation ->
  let repos = Github.Installation.repositories installation |> set_active_repos ~installation in

  let matrix_org_room = Matrix.get_org_room ~installation matrix in

  repos |> Current.list_iter ~collapse_key:"repo" (module Github.Api.Repo) @@ fun repo ->
  let refs = Github.Api.Repo.ci_refs ~staleness:Conf.max_staleness repo |> set_active_refs ~repo in
  let matrix_room = Matrix.get_room ~repo matrix in
  refs |> Current.list_iter (module Github.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Github.Api.Commit.id head) in
  let repo = Current.map (fun (_, x) -> { Repo_id.owner = x.Github.Repo_id.owner; Repo_id.name = x.Github.Repo_id.name; git_forge = Repo_id.GitHub }) repo in

  let analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  let builds =
    build_with_docker ?ocluster ~repo ~analysis src in
  let summary =
    builds
    |> Current.map (List.map (fun (variant, (build, _job)) -> variant, build))
    |> Current.map summarise
  in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ builds = builds
    and+ status = status in
    let repo = Current_github.Api.Commit.repo_id commit in
    let repo' = {Ocaml_ci.Repo_id.owner = repo.owner; name = repo.name; git_forge = Repo_id.GitHub} in
    let hash = Current_github.Api.Commit.hash commit in
    let jobs = builds |> List.map (fun (variant, (_, job_id)) -> (variant, job_id)) in
    Index.record ~repo:repo' ~hash ~status jobs
  and set_github_status =
    builds
    |> github_status_of_state ~head summary Github_forge.url Github_forge.url_variant
    |> Github.Api.CheckRun.set_status head "ocaml-ci"
  and set_matrix_status =
    match matrix, matrix_room, matrix_org_room with
    | None, None, None -> Current.return ()
    | Some context, Some room, Some org_room ->
      let key =
        let+ head = head in
        Github.Api.Commit.id head
        |> Git.Commit_id.digest
      in
      let message = Matrix.message_of_state ~head summary builds
      in
      Current.all [
        Matrix_current.post context ~key ~room message;
        Matrix_current.post context ~key ~room:org_room message;
      ]
    | _ -> assert false
  in
  Current.all [index; set_github_status; set_matrix_status]

let gitlab_pipeline ?ocluster ~app ~solver () =
  let module Gitlab = Current_gitlab in
  let installations = Gitlab_forge.installations |> Gitlab_forge.set_active_installations  in
  installations |> Current.list_iter ~collapse_key:"org" (module Gitlab_forge.Installation) @@ fun installation ->
  let repos = Gitlab_forge.repositories installation |> Gitlab_forge.set_active_repos ~installation in

  repos |> Current.list_iter ~collapse_key:"repo" (module Gitlab.Repo_id) @@ fun repo ->
  let* repo_id = repo in                                                                             
  let refs = Gitlab.Api.ci_refs app ~staleness:Conf.max_staleness repo_id |> Gitlab_forge.set_active_refs ~repo in
  refs |> Current.list_iter (module Gitlab.Api.Commit) @@ fun head ->
  let src = Git.fetch (Current.map Gitlab.Api.Commit.id head) in
  let repo = Current.map (fun x -> {Repo_id.owner = x.Gitlab.Repo_id.owner; Repo_id.name = x.Gitlab.Repo_id.name; git_forge = Repo_id.GitLab}) repo in

  let analysis = Analyse.examine ~solver ~platforms ~opam_repository_commit src in
  let builds = build_with_docker ?ocluster ~repo ~analysis src in
  let summary =
    builds
    |> Current.map (List.map (fun (variant, (build, _job)) -> variant, build))
    |> Current.map summarise
  in
  let status =
    let+ summary = summary in
    match summary with
    | Ok () -> `Passed
    | Error (`Active `Running) -> `Pending
    | Error (`Msg _) -> `Failed
  in
  let index =
    let+ commit = head
    and+ builds = builds
    and+ status = status in
    let repo = Gitlab.Api.Commit.repo_id commit
               |> fun repo -> { Ocaml_ci.Repo_id.owner = repo.owner; name = repo.name; git_forge = Repo_id.GitLab } in

    let hash = Gitlab.Api.Commit.hash commit in
    let jobs = builds |> List.map (fun (variant, (_, job_id)) -> (variant, job_id)) in
    Index.record ~repo ~hash ~status jobs
  and set_gitlab_status =
    Gitlab_forge.gitlab_status_of_state head summary
    |> Gitlab.Api.Commit.set_status head "ocaml-ci-gitlab"
  in
  Current.all [index; set_gitlab_status]

let main ?ocluster ?matrix ~github ~gitlab ~solver () = 
  let ocluster = Option.map (Cluster_build.config ~timeout:(Duration.of_hour 1)) ocluster in
  Current.with_context opam_repository_commit @@ fun () ->
  Current.with_context platforms @@ fun () ->

  Current.all_labelled [ "installation", github_pipeline ?ocluster ?matrix ~app:github ~solver ()
                       ; "installation", gitlab_pipeline ?ocluster ~app:gitlab ~solver ()]