(* Access control policy. *)
let has_role user = function
  | `Viewer | `Monitor -> true
  | _ ->
    match Option.map Current_web.User.id user with
    | Some ( "github:talex5"
           | "github:avsm"
           | "github:kit-ty-kate"
           | "github:samoht"
           | "github:tmcgilchrist"
           | "github:dra27"
           ) -> true
    | _ -> false

let webhook_route ~engine ~webhook_secret ~has_role =
  Routes.(s "webhooks" / s "github" /? nil @--> Current_github.webhook ~engine ~webhook_secret ~has_role)

let login_route github_auth = Routes.(s "login" /? nil @--> Current_github.Auth.login github_auth) 

let authn github_auth = Option.map Current_github.Auth.make_login_uri github_auth

(* Link for GitHub statuses. *)
let url ~owner ~name ~hash = Uri.of_string (Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s" owner name hash)

(* Link for GitHub CheckRun details. *)
let url_variant ~owner ~name ~hash ~variant =
  Printf.sprintf "https://ci.ocamllabs.io/github/%s/%s/commit/%s/variant/%s" owner name hash variant
