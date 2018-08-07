module RR = ReasonReact

module RD = ReactDOMRe

module RE = ReactEvent

external s : string -> RR.reactElement = "%identity"

let target_value e = (RE.Form.target e)##value

let div ?class_name ?style ?on_click children =
  let props = RD.props
    ?className:class_name
    ?onClick:on_click
    ?style
    () in
  RD.createElementVariadic "div" ~props @@ Array.of_list children

let span ?class_name ?style ?on_click children =
  let props = RD.props
    ?className:class_name
    ?onClick:on_click
    ?style
    () in
  RD.createElementVariadic "span" ~props @@ Array.of_list children

let code children =
  RD.createElementVariadic "code" ~props:(RD.props ()) @@ Array.of_list children

let pre children =
  RD.createElementVariadic "pre" ~props:(RD.props ()) @@ Array.of_list children

let header ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "header" ~props @@ Array.of_list children

let nav children =
  RD.createElementVariadic "nav" ~props:(RD.props ()) @@ Array.of_list children

let ul children =
  RD.createElementVariadic "ul" ~props:(RD.props ()) @@ Array.of_list children

let li ?class_name ?on_click children =
  let props = RD.props
    ?className:class_name
    ?onClick:on_click
    () in
  RD.createElementVariadic "li" ~props @@ Array.of_list children

let h1 children =
  RD.createElementVariadic "h1" ~props:(RD.props ()) @@ Array.of_list children

let h2 children =
  RD.createElementVariadic "h2" ~props:(RD.props ()) @@ Array.of_list children

let h3 children =
  RD.createElementVariadic "h3" ~props:(RD.props ()) @@ Array.of_list children

let a ?href ?on_click children =
  let props = RD.props
    ?href
    ?onClick:on_click
    () in
  RD.createElementVariadic "a" ~props @@ Array.of_list children

let b children =
  RD.createElementVariadic "b" ~props:(RD.props ()) @@ Array.of_list children

let p children =
  RD.createElementVariadic "p" ~props:(RD.props ()) @@ Array.of_list children

let i ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "i" ~props @@ Array.of_list children

let label ?for_ children =
  let props = RD.props
    ?htmlFor:for_
    () in
  RD.createElementVariadic "label" ~props @@ Array.of_list children

let small ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "small" ~props @@ Array.of_list children

let input ?type_ ?value ?on_change children =
  let props = RD.props
    ?type_
    ?value
    ?onChange:on_change
    () in
  RD.createElementVariadic "input" ~props @@ Array.of_list children

let button ?class_name ?disabled ?on_click children =
  let props = RD.props
    ?className:class_name
    ?disabled
    ?onClick:on_click
    () in
  RD.createElementVariadic "button" ~props @@ Array.of_list children

let textarea ?class_name ?value ?read_only ?on_change children =
  let props = RD.props
    ?className:class_name
    ?value
    ?readOnly:read_only
    ?onChange:on_change
    () in
  RD.createElementVariadic "textarea" ~props @@ Array.of_list children
