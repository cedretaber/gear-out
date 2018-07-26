module RR = ReasonReact

type submit
  = ToggleTestCase of int
  | ChangeInputStyle of State.Submit.input_style
  | AllToggle of bool
  | ChangeOutput of int * string
  | SubmitAnswer of int

type playground
  = ClickGear of Board.t
  | ChangeSize of int
  | ResetBoard

type t
  = ChangePage of Page.t
  | Submit of submit
  | Playground of playground
  | NoAction

let change_page page = ChangePage page

let toggle_test_case i = Submit (ToggleTestCase i)
let change_input_style style = Submit (ChangeInputStyle style)
let all_toggle is_open = Submit (AllToggle is_open)
let change_output i output = Submit (ChangeOutput (i, output))
let submit_answer i = Submit (SubmitAnswer i)

let click_gear board = Playground (ClickGear board)
let change_size size = Playground (ChangeSize size)
let reset_board = Playground ResetBoard