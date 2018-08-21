module RR = ReasonReact

type submit
  = ToggleTestCase of int
  | ChangeInputStyle of State.Submit.input_style
  | AllToggle of bool
  | ChangeOutput of int * string
  | SubmitAnswer of int
  | ChangeOutputAll of string
  | SubmitAnswerAll
  | RunBoard of (int * string) list
  | ResetSubmit

type playground
  = ClickGear of Board.t
  | ChangeSize of int
  | ResetBoard
  | ChangeBoardInput of string
  | ClickMakeBoard

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
let change_output_all output = Submit (ChangeOutputAll output)
let submit_answer_all = Submit SubmitAnswerAll
let run_board ops = Submit (RunBoard ops)
let reset_submit = Submit ResetSubmit

let click_gear board = Playground (ClickGear board)
let change_size size = Playground (ChangeSize size)
let reset_board = Playground ResetBoard
let change_board_input board_input = Playground (ChangeBoardInput board_input)
let click_make_board = Playground ClickMakeBoard