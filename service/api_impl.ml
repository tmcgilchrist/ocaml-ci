module Rpc = Current_rpc.Impl(Current)
module Raw = Ocaml_ci_api.Raw

module String_map = Map.Make(String)

module Index = Ocaml_ci.Index
module Owner_id = Ocaml_ci.Index.Owner_id

module Owner_map = Map.Make(Owner_id)

open Capnp_rpc_lwt

let make_commit ~engine ~owner_id ~name hash =
  let module Commit = Raw.Service.Commit in
  Commit.local @@ object
    inherit Commit.service

    method jobs_impl _params release_param_caps =
      let open Commit.Jobs in
      release_param_caps ();
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name ; name; git_forge = owner_id.Owner_id.git_forge } in
      let jobs = Index.get_jobs ~repo  hash in
      let response, results = Service.Response.create Results.init_pointer in
      let arr = Results.jobs_init results (List.length jobs) in
      jobs |> List.iteri (fun i (variant, outcome) ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.JobInfo.variant_set slot variant;
          let state = Raw.Builder.JobInfo.state_init slot in
          let module S = Raw.Builder.JobInfo.State in
          match outcome with
          | `Not_started -> S.not_started_set state
          | `Passed -> S.passed_set state
          | `Aborted -> S.aborted_set state
          | `Active -> S.active_set state
          | `Failed msg -> S.failed_set state msg
        );
      Service.return response

    method job_of_variant_impl params release_param_caps =
      let open Commit.JobOfVariant in
      let variant = Params.variant_get params in
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in

      release_param_caps ();
      match Index.get_job ~repo ~hash ~variant with
      | Error `No_such_variant -> Service.fail "No such variant %S" variant
      | Ok None -> Service.fail "No job for variant %S yet" variant
      | Ok (Some id) ->
        let job = Rpc.job ~engine id in
        let response, results = Service.Response.create Results.init_pointer in
        Results.job_set results (Some job);
        Capability.dec_ref job;
        Service.return response

    method refs_impl _params release_param_caps =
      let open Commit.Refs in
      release_param_caps ();
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in
      let refs =
        Index.get_active_refs repo
        |> Index.Ref_map.bindings
        |> List.filter_map (fun (name, h) -> if h = hash then Some name else None)
      in
      let response, results = Service.Response.create Results.init_pointer in
      Results.refs_set_list results refs |> ignore;
      Service.return response

    method status_impl _params release_param_caps =
      let open Commit.Status in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in
      Index.get_status ~repo ~hash
      |> (function
          | `Not_started -> Results.status_set results NotStarted
          | `Pending -> Results.status_set results Pending
          | `Failed -> Results.status_set results Failed
          | `Passed -> Results.status_set results  Passed
         );
      Service.return response
  end

let to_build_status =
  let open Raw.Builder.BuildStatus in function
  | `Not_started -> NotStarted
  | `Failed -> Failed
  | `Pending -> Pending
  | `Passed -> Passed

let make_repo ~engine ~owner_id ~name =
  let module Repo = Raw.Service.Repo in
  let commits = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_commit hash =
    match String_map.find_opt hash !commits with
    | Some x -> x
    | None ->
      let commit = make_commit ~engine ~owner_id ~name hash in
      commits := String_map.add hash commit !commits;
      commit
  in
  Repo.local @@ object
    inherit Repo.service

    method refs_impl _params release_param_caps =
      let open Repo.Refs in
      release_param_caps ();
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in
      let refs =
        Index.get_active_refs repo
        |> Index.Ref_map.bindings
      in
      let response, results = Service.Response.create Results.init_pointer in
      let arr = Results.refs_init results (List.length refs) in
      refs |> List.iteri (fun i (gref, hash) ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.RefInfo.ref_set slot gref;
          Raw.Builder.RefInfo.hash_set slot hash;
          let status = to_build_status (Index.get_status ~repo ~hash) in
          Raw.Builder.RefInfo.state_set slot status;
        );
      Service.return response

    method obsolete_refs_of_commit_impl _ release_param_caps =
      release_param_caps ();
      Service.fail "This method no longer exists"

    method commit_of_ref_impl params release_param_caps =
      let open Repo.CommitOfRef in
      let gref = Params.ref_get params in
      release_param_caps ();
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in
      let refs = Index.get_active_refs repo in
      match Index.Ref_map.find_opt gref refs with
      | None -> Service.fail "@[<v2>Unknown ref %S. Options are:@,%a@]" gref
                  Fmt.(Dump.list string) (List.map fst (Index.Ref_map.bindings refs))
      | Some hash ->
        let commit = get_commit hash in
        let response, results = Service.Response.create Results.init_pointer in
        Results.commit_set results (Some commit);
        Service.return response

    method commit_of_hash_impl params release_param_caps =
      let open Repo.CommitOfHash in
      let hash = Params.hash_get params in
      release_param_caps ();
      let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in
      match Index.get_full_hash ~repo hash with
      | Error `Ambiguous -> Service.fail "Ambiguous commit hash %S" hash
      | Error `Invalid -> Service.fail "Invalid Git hash %S" hash
      | Error `Unknown -> Service.fail "Unknown Git hash %S" hash
      | Ok hash ->
        let commit = get_commit hash in
        let response, results = Service.Response.create Results.init_pointer in
        Results.commit_set results (Some commit);
        Service.return response

    method obsolete_job_of_commit_impl _ release_param_caps =
      release_param_caps ();
      Service.fail "This method no longer exists"

    method obsolete_job_of_ref_impl _ release_param_caps =
      release_param_caps ();
      Service.fail "This method no longer exists"
  end

let make_org ~engine owner_id  =
  let module Org = Raw.Service.Org in
  let repos = ref String_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_repo name =
    match String_map.find_opt name !repos with
    | Some repo -> Some repo
    | None -> (
       let active_repos = Index.get_active_repos ~owner_id in
       if Index.Repo_set.mem name active_repos then (
         let repo = make_repo ~engine ~owner_id ~name in
         repos := String_map.add name repo !repos;
         Some repo
       ) else None
    )
  in
  Org.local @@ object
    inherit Org.service

    method repo_impl params release_param_caps =
      let open Org.Repo in
      let name = Params.name_get params in
      release_param_caps ();
      match get_repo name with
      | None -> Service.fail "Invalid GitHub repo %a" Owner_id.pp owner_id
      | Some repo ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.repo_set results (Some repo);
        Service.return response

    method repos_impl _params release_param_caps =
      let open Org.Repos in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let repos = Index.get_active_repos ~owner_id |> Index.Repo_set.elements in
      let arr = Results.repos_init results (List.length repos) in
      repos |> List.iteri (fun i name ->
          let slot = Capnp.Array.get arr i in
          Raw.Builder.RepoInfo.name_set slot name;
          let repo = { Ocaml_ci.Repo_id.owner = owner_id.Owner_id.name; name; git_forge = owner_id.Owner_id.git_forge } in
          let refs = Index.get_active_refs repo in
          let status =
            match Index.Ref_map.find_opt "refs/heads/master" refs with
            | Some hash -> to_build_status (Index.get_status ~repo ~hash)
            | None -> NotStarted
          in
          Raw.Builder.RepoInfo.master_state_set slot status;
        );
      Service.return response
  end


let to_git_forge = let open Raw.Builder.GitForge in function
  | Github -> Ocaml_ci.Repo_id.GitHub
  | Gitlab -> Ocaml_ci.Repo_id.GitLab
  | Undefined _a -> Ocaml_ci.Repo_id.GitHub (* TODO Clearly wrong, how do we handle this? *)

let of_git_forge = 
  let open Raw.Builder.GitForge in 
  function
  | Ocaml_ci.Repo_id.GitHub -> Github
  | Ocaml_ci.Repo_id.GitLab -> Gitlab

let make_ci ~engine =
  let module CI = Raw.Service.CI in
  let orgs = ref Owner_map.empty in
  (* Returned reference is borrowed. Call [inc_ref] if you need to keep it. *)
  let get_org owner_id =
    match Owner_map.find_opt owner_id !orgs with
    | Some org -> Some org
    | None ->
      if Index.Owner_set.mem owner_id (Index.get_active_owners ()) then (
        let org = make_org ~engine owner_id in
        orgs := Owner_map.add owner_id org !orgs;
        Some org
      ) else None
  in
  CI.local @@ object
    inherit CI.service

    method org_impl params release_param_caps =
      let open CI.Org in
      let owner = Params.owner_get params in
      let git_forge = Params.git_forge_get params in
      let owner_id = { Index.Owner_id.name = owner; Index.Owner_id.git_forge = to_git_forge git_forge } in
      release_param_caps ();
      match get_org owner_id with
      | None -> Service.fail "Invalid Git Forge owner %S %a" owner Index.Owner_id.pp owner_id
      | Some org ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.org_set results (Some org);
        Service.return response

    method orgs_impl _params release_param_caps =
      let open CI.Orgs in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let owners = Index.get_active_owners () |> Index.Owner_set.elements |> List.map (fun x -> x.Owner_id.name) in
      Results.orgs_set_list results owners |> ignore;
      Service.return response

    method all_orgs_impl _params release_param_caps =
      let open CI.AllOrgs in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let owners = Index.get_owners () in
      let arr = Results.orgs_init results (List.length owners) in
      owners |> List.iteri (fun i owner_id -> 
                    let slot = Capnp.Array.get arr i in
                    Raw.Builder.OrgId.name_set slot owner_id.Owner_id.name;
                    let git_forge = of_git_forge owner_id.Owner_id.git_forge in
                    Raw.Builder.OrgId.git_forge_set slot git_forge;
                  );
      Service.return response
      
  end
