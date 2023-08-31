(** Pipeline configuration. *)

let website_scheme_and_domain = "https://ocaml.ci.dev"

let ci_profile =
  match Sys.getenv_opt "CI_PROFILE" with
  | Some "production" -> `Production
  | Some "dev" | None -> `Dev
  | Some x -> Fmt.failwith "Unknown $CI_PROFILE setting %S." x

let cmdliner_envs =
  let values = [ "production"; "dev" ] in
  let doc =
    Printf.sprintf "CI profile settings, must be %s."
      (Cmdliner.Arg.doc_alts values)
  in
  [ Cmdliner.Cmd.Env.info "CI_PROFILE" ~doc ]

(* GitHub defines a stale branch as more than 3 months old.
   Don't bother testing these. *)
let max_staleness = Duration.of_day 93

module Capnp = struct
  (* Cap'n Proto RPC is enabled by passing --capnp-public-address. These values are hard-coded
     (because they're just internal to the Docker container). *)

  let cap_secrets =
    match ci_profile with
    | `Production -> "/capnp-secrets"
    | `Dev -> "./capnp-secrets"

  let secret_key = cap_secrets ^ "/secret-key.pem"
  let cap_file = cap_secrets ^ "/ocaml-ci-admin.cap"
  let internal_port = 9000
end

let dev_pool = Current.Pool.create ~label:"docker" 1

(** Maximum time for one Docker build. *)
let build_timeout = Duration.of_hour 1

module Builders = struct
  let v docker_context =
    let docker_context, pool =
      ( Some docker_context,
        Current.Pool.create ~label:("docker-" ^ docker_context) 20 )
    in
    { Ocaml_ci.Builder.docker_context; pool; build_timeout }

  let local =
    { Ocaml_ci.Builder.docker_context = None; pool = dev_pool; build_timeout }
end

module OV = Ocaml_version
module DD = Dockerfile_opam.Distro

let default_compilers =
  OV.(List.map with_just_major_and_minor Releases.[ v4_14; latest ])

let trunk_compiler = OV.(Sources.trunk |> without_patch)

type platform = {
  label : string;
  builder : Ocaml_ci.Builder.t;
  pool : Ocaml_ci.Platform.Pool_name.t;
  distro : string;
  ocaml_version : OV.t;
  arch : OV.arch;
  opam_version : Ocaml_ci.Opam_version.t;
  lower_bound : bool;
}

(* TODO Hardcoding the versions for now, this should expand to OV.Releases.recent.
   Currently we only have base images for these 2 compiler variants. See ocurrent/freebsd-infra playbook.yml.
*)
let freebsd_distros =
  [
    {
      label = "freebsd";
      builder = Builders.local;
      pool = `FreeBSD_x86_64;
      distro = "freebsd";
      ocaml_version = OV.Releases.v4_14;
      arch = `X86_64;
      opam_version = `V2_1;
      lower_bound = false;
    };
    {
      label = "freebsd";
      builder = Builders.local;
      pool = `FreeBSD_x86_64;
      distro = "freebsd";
      ocaml_version = OV.Releases.v5_0;
      arch = `X86_64;
      opam_version = `V2_1;
      lower_bound = false;
    };
  ]

(* TODO Hardcoding the versions for now, this should expand to OV.Releases.recent.
   Currently we only have base images for these 2 compiler variants. See ocurrent/macos-infra playbook.yml.
*)
let macos_distros =
  [
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = `Macos_x86_64;
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v4_14;
      arch = `X86_64;
      opam_version = `V2_1;
      lower_bound = false;
    };
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = `Macos_x86_64;
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v5_0;
      arch = `X86_64;
      opam_version = `V2_1;
      lower_bound = false;
    };
    (* Apple Silicon *)
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = `Macos_ARM64;
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v4_14;
      arch = `Aarch64;
      opam_version = `V2_1;
      lower_bound = false;
    };
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = `Macos_ARM64;
      distro = "macos-homebrew";
      ocaml_version = OV.Releases.v5_0;
      arch = `Aarch64;
      opam_version = `V2_1;
      lower_bound = false;
    };
  ]

let macos_distros_experimental =
  [
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = `Macos_x86_64;
      distro = "macos-homebrew";
      ocaml_version = OV.v 5 1 ~patch:0 ~prerelease:"rc2";
      arch = `X86_64;
      opam_version = `V2_1;
      lower_bound = false;
    };
    {
      label = "macos-homebrew";
      builder = Builders.local;
      pool = `Macos_x86_64;
      distro = "macos-homebrew";
      ocaml_version = OV.v 5 1 ~patch:0 ~prerelease:"rc2";
      arch = `Aarch64;
      opam_version = `V2_1;
      lower_bound = false;
    };
  ]

let pool_of_arch = function
  | `X86_64 | `I386 -> `Linux_x86_64
  | `Aarch32 | `Aarch64 -> `Linux_ARM64
  | `S390x -> `Linux_s390x
  | `Ppc64le -> `Linux_ppc64
  | `Riscv64 -> `Linux_riscv64

let platforms ~ci_profile ~include_macos opam_version =
  let v ?(arch = `X86_64) ?(lower_bound = false) label distro ocaml_version =
    {
      arch;
      label;
      builder = Builders.local;
      pool = pool_of_arch arch;
      distro;
      ocaml_version;
      opam_version;
      lower_bound;
    }
  in
  let master_distro = DD.resolve_alias DD.master_distro in
  (* Make platforms for all arches and desired variants using [distro] *)
  let make_platform distro =
    let distro = DD.resolve_alias distro in
    let label = DD.latest_tag_of_distro (distro :> DD.t) in
    let tag = DD.tag_of_distro (distro :> DD.t) in
    let f ov =
      if distro = master_distro then
        v label tag (OV.with_variant ov (Some "flambda"))
        :: List.map
             (fun arch -> v ~arch label tag ov)
             (DD.distro_arches ov (distro :> DD.t))
      else [ v label tag ov ]
    in
    List.fold_left (fun l ov -> f ov @ l) [] default_compilers
  in
  (* Make platform for OCaml version [ov] using [master_distro] *)
  let make_release ?arch ?(lower_bound = false) ov =
    let distro = DD.tag_of_distro (master_distro :> DD.t) in
    let ov = OV.with_just_major_and_minor ov in
    v ?arch ~lower_bound (OV.to_string ov) distro ov
  in
  match ci_profile with
  | `Production ->
      let distros =
        DD.active_tier1_distros `X86_64 @ DD.active_tier2_distros `X86_64
        |> List.filter (( <> ) (`OpenSUSE `Tumbleweed :> DD.t))
        (* Removing Tumbleweed due to bug in opam depext see https://github.com/ocaml/opam/issues/5565 *)
        |> List.concat_map make_platform
      in
      let distros =
        if include_macos then
          macos_distros @ macos_distros_experimental @ distros @ freebsd_distros
        else distros @ freebsd_distros
      in
      (* The first one in this list is used for lint actions *)
      let ovs = List.rev OV.Releases.recent @ OV.Releases.unreleased_betas in
      let releases = List.map make_release ovs in
      let lower_bounds = List.map (make_release ~lower_bound:true) ovs in
      releases @ lower_bounds @ distros
  | `Dev when Sys.win32 ->
      (* Assume we're building using native Windows images. *)
      let distro =
        DD.tag_of_distro (`Windows (`Mingw, DD.win10_latest_image) :> DD.t)
      in
      let ov = OV.with_just_major_and_minor OV.Releases.latest in
      [ v (OV.to_string ov) distro ov ]
  | `Dev ->
      let[@warning "-8"] (latest :: previous :: _) =
        List.rev OV.Releases.recent
      in
      let macos_distros = if include_macos then macos_distros else [] in
      let ovs = [ latest; previous ] in
      let releases = List.map make_release ovs in
      releases @ macos_distros @ freebsd_distros

(** When we have the same platform differing only in [lower_bound], for the
    purposes of Docker pulls, take only the platform with [lower_bound = true].
    The non-lower-bound platform will be regenerated in the "opam-vars" step *)
let merge_lower_bound_platforms platforms =
  let eq_without_lower_bound
      {
        arch = arch0;
        label = _;
        builder = _;
        pool = pool0;
        distro = distro0;
        ocaml_version = ocaml_version0;
        opam_version = opam_version0;
        lower_bound = _;
      }
      {
        arch = arch1;
        label = _;
        builder = _;
        pool = pool1;
        distro = distro1;
        ocaml_version = ocaml_version1;
        opam_version = opam_version1;
        lower_bound = _;
      } =
    arch0 = arch1
    && pool0 = pool1
    && distro0 = distro1
    && OV.equal ocaml_version0 ocaml_version1
    && Ocaml_ci.Opam_version.equal opam_version0 opam_version1
  in
  let lower_bound, upper_bound =
    List.partition (fun p -> p.lower_bound) platforms
  in
  let upper_bound =
    List.filter
      (fun u ->
        not (List.exists (fun l -> eq_without_lower_bound u l) lower_bound))
      upper_bound
  in
  upper_bound @ lower_bound

let fetch_platforms ~include_macos () =
  let open Ocaml_ci in
  let schedule = Current_cache.Schedule.v ~valid_for:(Duration.of_day 30) () in
  let v
      {
        label;
        builder;
        pool;
        distro;
        ocaml_version;
        arch;
        opam_version;
        lower_bound;
      } =
    match distro with
    | "macos-homebrew" ->
        (* MacOS uses ZFS snapshots rather than docker images, hardcoding values here for now. *)
        let docker_image_name =
          Fmt.str "%s-ocaml-%d.%d" distro (OV.major ocaml_version)
            (OV.minor ocaml_version)
        in
        let label =
          Fmt.str "pull %s %s" docker_image_name (OV.string_of_arch arch)
        in
        let base = Current.return ~label (`MacOS docker_image_name) in
        Platform.get_macos ~arch ~label ~builder ~pool ~distro ~ocaml_version
          ~opam_version ~lower_bound base
    | "freebsd" ->
        (* FreeBSD uses ZFS snapshots rather than docker images, hardcoding values here for now. *)
        let docker_image_name =
          Fmt.str "%s-ocaml-%d.%d" distro (OV.major ocaml_version)
            (OV.minor ocaml_version)
        in
        let label =
          Fmt.str "pull %s %s" docker_image_name (OV.string_of_arch arch)
        in
        let base = Current.return ~label (`FreeBSD docker_image_name) in
        Platform.get_freebsd ~arch ~label ~builder ~pool ~distro ~ocaml_version
          ~opam_version ~lower_bound base
    | _ ->
        (* All Linux distros *)
        let base =
          Platform.pull ~arch ~schedule ~builder ~distro ~ocaml_version
            ~opam_version
        in
        let host_base =
          match arch with
          | `X86_64 -> base
          | _ ->
              Platform.pull ~arch:`X86_64 ~schedule ~builder ~distro
                ~ocaml_version ~opam_version
        in
        Platform.get ~arch ~label ~builder ~pool ~distro ~ocaml_version
          ~host_base ~opam_version ~lower_bound base
  in
  let v2_1 =
    platforms ~ci_profile `V2_1 ~include_macos |> merge_lower_bound_platforms
  in
  Current.list_seq (List.map v v2_1) |> Current.map List.flatten
