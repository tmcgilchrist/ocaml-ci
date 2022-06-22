(** The index is:
    - A map from active Git references to the Git commit at their heads.
    - A map from project builds ([owner * name * hash)] triples) to statuses.
    - A (persisted) map from each Git commit hash to its last known OCurrent job ID. *)

type job_state = [`Not_started | `Active | `Failed of string | `Passed | `Aborted ] [@@deriving show]

type build_status = [ `Not_started | `Pending | `Failed | `Passed ]

val init : unit -> unit
(** Ensure the database is initialised (for unit-tests). *)

val record :
  repo:Repo_id.t ->
  hash:string ->
  status:build_status ->
  (string * Current.job_id option) list ->
  unit
(** [record ~repo ~hash jobs] updates the entry for [repo, hash] to point at [jobs]. *)

val get_jobs : repo:Repo_id.t -> string -> (string * job_state) list
(** [get_jobs ~repo commit] is the last known set of OCurrent jobs for hash [commit] in repository [Repo_id.t]. *)

val get_job : repo:Repo_id.t -> hash:string -> variant:string -> (string option, [> `No_such_variant]) result
(** [get_job ~repo ~variant] is the last known job ID for this combination. *)

val get_status:
  repo:Repo_id.t ->
  hash:string ->
  build_status
(** [get_status ~repo ~hash] is the latest status for this combination. *)

val get_full_hash : repo:Repo_id.t -> string -> (string, [> `Ambiguous | `Unknown | `Invalid]) result
(** [get_full_hash ~repo short_hash] returns the full hash for [short_hash]. *)

type owner_id = { name: string; git_forge_prefix: string }
(* Owner_id as a combination of git forge and name
   e.g. github:tmcgilchrist or gitlab:tmcgilchrist 
*)
                
module Owner_set : Set.S with type elt = owner_id
module Repo_set : Set.S with type elt = string

val set_active_owners : Owner_set.t -> unit
(** [set_active_owners owners] records that [owners] is the set of accounts on which the CI is installed.
    This is displayed in the CI web interface. *)

val get_active_owners : unit -> Owner_set.t
(** [get_active_owners ()] is the last value passed to [set_active_owners], or [[]] if not known yet. *)

val set_active_repos : owner_id:owner_id -> Repo_set.t -> unit
(** [set_active_repos ~owner_id repos] records that [repos] is the set of active repositories under [owner_id]. *)

val get_active_repos : owner_id:owner_id -> Repo_set.t
(** [get_active_repos ~owner_id] is the last value passed to [set_active_repos] for [owner_id], or [empty] if not known yet. *)

module Ref_map : Map.S with type key = string

val set_active_refs : repo:Repo_id.t -> string Ref_map.t -> unit
(** [set_active_refs ~repo refs] records that [refs] is the current set of Git references that the CI
    is watching. There is one entry for each branch and PR. Each entry maps the Git reference name
    to the head commit's hash. *)

val get_active_refs : Repo_id.t -> string Ref_map.t
(** [get_active_refs repo] is the entries last set for [repo] with [set_active_refs], or
    [empty] if this repository isn't known. *)
