open Tyxml.Html

let render () =
  Template.instance [
    p [txt "Welcome to OCaml-CI!"];
    ul [
      li [a ~a:[a_href "/gitlab"] [txt "Registered GitLab organisations"]];
      li [a ~a:[a_href "/github"] [txt "Registered GitHub organisations"]];
    ]
  ]
