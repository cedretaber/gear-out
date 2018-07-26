
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
      is_open: bool;
      board: Board.t;
      output: string;
      message: string option
    }

    let make size = {
      state= Waiting; 
      is_open= false; 
      board= Board.make size; 
      output= ""; 
      message= None
    }
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
    output: string;
    test_cases: TestCase.t array
  }

  let default_sizes =
    [|2; 2; 2; 2; 2; 2; 2; 2;
      4; 4; 4; 4; 4; 4; 4; 4; 4; 4;
      6; 6; 6; 6; 6; 6; 6; 6; 6; 6;
      8; 8; 8; 8; 8; 8; 8; 8; 8; 8;
      10; 10; 10; 10|]

  let make ?(sizes=default_sizes) () = {
    state= Ready;
    input_style= Competitive;
    output= "";
    test_cases= Array.map TestCase.make sizes
  }
end

type t = {
  page: Page.t;
  submit: Submit.t;
  playground: Playground.t
}