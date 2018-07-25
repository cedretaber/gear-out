module RR = ReasonReact

type playground
  = ClickGear of Board.t

type t
  = ChangePage of Page.t
  | Playground of playground
  | NoAction