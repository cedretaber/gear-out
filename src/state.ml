
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

  let make size = { size; count= 0; board= Board.make size; state = Playing }
end

type t = {
  page: Page.t;
  playground: Playground.t
}