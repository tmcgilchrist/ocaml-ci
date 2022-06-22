let safe_char = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '-' | '_' -> true
  | _ -> false

let check_safe s =
  if not (Astring.String.for_all safe_char s) then
    Fmt.failwith "Unsafe characters in %S" s

let for_repo repo =
  let { Repo_id.owner; name; git_forge} = repo in
  let git_forge = (Repo_id.string_of_git_forge git_forge) in
  check_safe owner;
  check_safe name;
  check_safe git_forge;
  let name = Printf.sprintf "dune:%s:%s:%s" git_forge owner name in
  Obuilder_spec.Cache.v name ~target:"/src/_build" ~buildkit_options:["sharing", "private"]
