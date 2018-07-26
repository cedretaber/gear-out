module RR = ReasonReact

module RD = ReactDOMRe

module RE = ReactEventRe

external s : string -> RR.reactElement = "%identity"

let target_value e = (e |> RE.Form.target |> RD.domElementToObj)##value

let empty () = Js.Obj.empty ()

let div ?class_name ?style ?on_click children =
  let props = Obj.magic @@
    RD.props
      ?className:class_name
      ?onClick:on_click
      ?style
      () in
  RR.createDomElement "div" ~props @@ Array.of_list children

let code children =
  RR.createDomElement "code" ~props:(empty ()) @@ Array.of_list children

let pre children =
  RR.createDomElement "pre" ~props:(empty ()) @@ Array.of_list children

let header children =
  RR.createDomElement "header" ~props:(empty ()) @@ Array.of_list children

let nav children =
  RR.createDomElement "nav" ~props:(empty ()) @@ Array.of_list children

let ul children =
  RR.createDomElement "ul" ~props:(empty ()) @@ Array.of_list children

let li children =
  RR.createDomElement "li" ~props:(empty ()) @@ Array.of_list children

let h1 children =
  RR.createDomElement "h1" ~props:(empty ()) @@ Array.of_list children

let h2 children =
  RR.createDomElement "h2" ~props:(empty ()) @@ Array.of_list children

let h3 children =
  RR.createDomElement "h3" ~props:(empty ()) @@ Array.of_list children

let a ?href ?on_click children =
  let props = Obj.magic @@
    RD.props
      ?href
      ?onClick:on_click
      () in
  RR.createDomElement "a" ~props @@ Array.of_list children

let b children =
  RR.createDomElement "b" ~props:(empty ()) @@ Array.of_list children

let p children =
  RR.createDomElement "p" ~props:(empty ()) @@ Array.of_list children

let label ?for_ children =
  let props = Obj.magic @@
    RD.props
      ?htmlFor:for_
      () in
  RR.createDomElement "label" ~props @@ Array.of_list children

let input ?type_ ?value ?on_change children =
  let props = Obj.magic @@
    RD.props
      ?type_
      ?value
      ?onChange:on_change
      () in
  RR.createDomElement "input" ~props @@ Array.of_list children

let button ?class_name ?on_click children =
  let props = Obj.magic @@
    RD.props
      ?className:class_name
      ?onClick:on_click
      () in
  RR.createDomElement "button" ~props @@ Array.of_list children