(* Access control policy. *)
let has_role user role =
  match user with
  | None -> role = `Viewer              (* Unauthenticated users can only look at things. *)
  | Some user ->
     match Current_web.User.id user, role with
     | "gitlab:tmcgilchrist", _ -> true  (* This user has all roles *)
     | _, (`Viewer | `Builder) -> true   (* Any GitLab user can cancel and rebuild *)
     | _ -> false

let webhook_route ~webhook_secret =
  Routes.(s "webhooks" / s "gitlab" /? nil @--> Current_gitlab.webhook ~webhook_secret)

let login_route gitlab_auth = Routes.(s "login" /? nil @--> Current_gitlab.Auth.login gitlab_auth)

let authn auth =  Option.map Current_gitlab.Auth.make_login_uri auth

let url ~owner ~name ~hash = 
  Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/gitlab/%s/%s/commit/%s" owner name hash)

module Gitlab = Current_gitlab
(* TODO Sometime later, grab these from Index/DB table. *)
let gitlab_repos : Gitlab.Repo_id.t list = [
    { Gitlab.Repo_id.owner = "tmcgilchrist"; Gitlab.Repo_id.name = "ocaml-gitlab"; project_id = 29798678 }
  ; { Gitlab.Repo_id.owner = "tmcgilchrist"; Gitlab.Repo_id.name = "ocaml-changes"; project_id = 30953712 }
  ; { Gitlab.Repo_id.owner = "talex5"; Gitlab.Repo_id.name = "gemini-eio"; project_id = 28169706 }
  ]

(* Fake Installation module, we don't have this in GitLab. *)
module Installation = struct
  type t = { name : string }
  let compare = compare
  let pp f t = Fmt.string f t.name
end

open Current.Syntax
open Ocaml_ci

let installations = List.map (fun x -> {Installation.name = x.Gitlab.Repo_id.owner}) gitlab_repos |> Current.return ~label:"installations"

let repositories (installation : Installation.t Current.t) : Gitlab.Repo_id.t list Current.t =
  let+ installation = installation in
  List.filter (fun repo -> repo.Gitlab.Repo_id.owner == installation.Installation.name) gitlab_repos

let set_active_installations (accounts : Installation.t list Current.t) =
  let+ accounts = accounts in
  accounts
  |> List.fold_left (fun acc i -> 
         let owner_id = { Index.Owner_id.name = i.Installation.name; git_forge = Repo_id.GitLab } in 
         Index.Owner_set.add owner_id acc) (Index.get_active_owners ())
  |> Index.Owner_set.union (Index.get_active_owners ())
  |> Index.set_active_owners;
  accounts

let set_active_repos ~(installation : Installation.t Current.t) (repos : Gitlab.Repo_id.t list Current.t) =
  let+ repos = repos
  and+ installation = installation in
  let owner_id = { Index.Owner_id.name = installation.Installation.name; git_forge = Repo_id.GitLab } in
  repos
  |> List.fold_left (fun acc r -> Index.Repo_set.add r.Gitlab.Repo_id.name acc) Index.Repo_set.empty
  |> Index.set_active_repos ~owner_id;
  repos

module Git = Current_git

let set_active_refs ~(repo : Gitlab.Repo_id.t Current.t) xs =
  let+ repo = repo
  and+ xs = xs in
  let repo' = { Repo_id.owner = repo.owner; name = repo.name; git_forge = Repo_id.GitLab} in
  Index.set_active_refs ~repo:repo' (
      xs |> List.fold_left (fun acc x ->
                let commit = Gitlab.Api.Commit.id x in
                let gref = Git.Commit_id.gref commit in
                let hash = Git.Commit_id.hash commit in
                Index.Ref_map.add gref hash acc
              ) Index.Ref_map.empty
    );
  xs


let gitlab_status_of_state head result =
  let+ head = head
  and+ result = result in
  let { Gitlab.Repo_id.owner; name; project_id=_} = Gitlab.Api.Commit.repo_id head in
  let hash = Gitlab.Api.Commit.hash head in
  let url = url ~owner ~name ~hash in
  match result with
  | Ok _              -> Gitlab.Api.Status.v ~url `Success ~description:"Passed" ~name:"ocaml-ci"
  | Error (`Active _) -> Gitlab.Api.Status.v ~url `Pending ~name:"ocaml-ci"
  | Error (`Msg m)    -> Gitlab.Api.Status.v ~url `Failure ~description:m ~name:"ocaml-ci"
