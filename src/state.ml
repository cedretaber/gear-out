
module Playground = struct
  type state
    = Playing
    | Cleared

  type t = {
    count: int;
    board: Board.t;
    state: state
  }
end

type t = {
  page: Page.t;
  playground: Playground.t
}