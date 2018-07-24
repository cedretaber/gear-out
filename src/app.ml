open BSReact

let initialState () = State.Problem

let reducer (_: Action.t) (_: State.t) = RR.NoUpdate

let component = RR.reducerComponent "App"

let make _children = {
  component with
  initialState;
  reducer;
  render= fun self ->
    match self.state with
      State.Problem -> Problem.c []
    | State.Submit -> div []
    | State.Playground -> div []
}

let c children =
  RR.element @@ make children