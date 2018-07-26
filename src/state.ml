
module Playground = struct
  type state
    = Playing
    | Cleared

  type t = {
    size: int;
    count: int;
    board: Board.t;
    state: state
  }

  let make size =
    { size; count= 0; board= Board.make size; state = Playing }
end

module Submit = struct
  module TestCase = struct
    type state
      = Waiting
      | Accepted
      | WrongAnswer

    type t = {
      state: state;
      board: Board.t;
      input: string;
      message: string option
    }

    let make size =
      { state= Waiting; board= Board.make size; input= ""; message= None}
  end

  type state
    = Ready
    | Testing

  type input_style
    = Competitive
    | Doukaku
    | JSON

  type t = {
    state: state;
    input_style: input_style;
    test_cases: TestCase.t list
  }

  let default_sizes =
    [2; 2; 2; 2; 2; 2; 2; 2;
     4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
     6; 6; 6; 6; 6; 6; 6; 6; 6; 6;
     8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
     10; 10; 10; 10]

  let make ?(sizes=default_sizes) () = {
    state= Ready;
    input_style= Competitive;
    test_cases= List.map TestCase.make sizes
  }
end

type t = {
  page: Page.t;
  submit: Submit.t;
  playground: Playground.t
}