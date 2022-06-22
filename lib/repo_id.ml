(** Git Forge *)
type git_forge =
  | GitHub
  | GitLab 

let string_of_git_forge = function
  | GitHub -> "github"
  | GitLab -> "gitlab"

type t = {
  owner : string;
  name : string;
  git_forge : git_forge
}

let pp f { owner; name; git_forge=_ } = Fmt.pf f "%s/%s" owner name

let compare = compare