module RR = ReasonReact

module RD = ReactDOMRe

external s : string -> RR.reactElement = "%identity"

let empty () = Js.Obj.empty ()

let div ?class_name ?style children =
  let props = Obj.magic @@
    RD.props
      ?className:class_name
      ?style
      () in
  RR.createDomElement "div" ~props @@ Array.of_list children

let code children =
  RR.createDomElement "code" ~props:(empty ()) @@ Array.of_list children

let pre children =
  RR.createDomElement "pre" ~props:(empty ()) @@ Array.of_list children

let h1 children =
  RR.createDomElement "h1" ~props:(empty ()) @@ Array.of_list children

let h2 children =
  RR.createDomElement "h2" ~props:(empty ()) @@ Array.of_list children

let h3 children =
  RR.createDomElement "h3" ~props:(empty ()) @@ Array.of_list children

let p children =
  RR.createDomElement "p" ~props:(empty ()) @@ Array.of_list children

let b children =
  RR.createDomElement "b" ~props:(empty ()) @@ Array.of_list children