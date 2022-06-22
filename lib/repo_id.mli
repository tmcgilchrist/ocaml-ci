(** Git Forge *)
type git_forge =
  | GitHub
  | GitLab 

val string_of_git_forge : git_forge -> string

(** Git source repository *)
type t = {
  owner : string;
  name : string;
  git_forge : git_forge
}

val pp : t Fmt.t

val compare : t -> t -> int
