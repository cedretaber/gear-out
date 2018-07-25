open BSReact

module A = Action
module S = State
module P = Page

let initial_state page () =
  Random.self_init ();
  let playground = {
    S.Playground.count= 0;
    S.Playground.board= Board.make 4;
    S.Playground.state = S.Playground.Playing
  } in
  S.{ page; playground }

let reducer action state = match action, state with
    A.ChangePage page, _ -> RR.Update S.{ state with page }
  | A.Playground (A.ClickGear board), {S.playground= {state= Playing} as playground} ->
    if Board.is_cleared board then
      RR.Update S.{ state with playground= Playground.{ playground with state= Cleared; board } }
    else
      RR.Update S.{ state with playground= Playground.{ playground with board } }
  | _ -> RR.NoUpdate

let component = RR.reducerComponent "App"

let make ?(initial_page=P.Problem) _children = {
  component with
  initialState= initial_state initial_page;
  reducer;
  render= fun self ->
    let content = match self.state with
        {S.page= P.Problem} -> Pages.Problem.c []
      | {S.page= P.Submit} -> div []
      | {S.page= P.Playground; playground= {board= {size} as board}} ->
        let click_gear i =
          let x, y = i mod size, i / size in
          match Board.touch x y board with
            Some board -> self.send (A.Playground (A.ClickGear board))
          | None -> () in
        Pages.Playground.c ~board ~on_gear_click:(fun i _ -> click_gear i) [] in
    let move_page page event =
      RE.Mouse.preventDefault event;
      self.send (A.ChangePage page) in
    div [
      header [
        nav [
          ul [
            li [ a ~href:"#problem" ~on_click:(move_page P.Problem) [ s {j|問題|j} ] ];
            li [ a ~href:"#submit" ~on_click:(move_page P.Submit) [ s {j|提出|j} ] ];
            li [ a ~href:"#playground" ~on_click:(move_page P.Playground) [ s {j|サンプル|j} ] ]
          ]
        ]
      ];
      div ~class_name:"content" [
        content
      ]
    ]
}

let c ?initial_page children =
  RR.element @@ make ?initial_page children