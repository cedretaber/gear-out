module RR = ReasonReact

type playground
  = ClickGear of Board.t
  | ChangeSize of int
  | ResetBoard

type t
  = ChangePage of Page.t
  | Playground of playground
  | NoAction