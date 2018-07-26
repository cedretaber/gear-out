open BSReact

module A = Action
module S = State
module P = Page

let initial_state page () =
  Random.self_init ();
  S.{ page; submit= Submit.make (); playground= Playground.make 4 }

let reducer action state = match action, state with
    A.ChangePage page, _ -> RR.Update S.{ state with page }
  | A.Playground (A.ClickGear board), {S.playground= {state= Playing; count} as playground} ->
    if Board.is_cleared board then
      RR.Update S.{ state with playground= Playground.{ playground with state= Cleared; count= count + 1; board } }
    else
      RR.Update S.{ state with playground= Playground.{ playground with board; count= count + 1 } }
  | A.Playground (A.ChangeSize size), {S.playground} ->
    RR.Update S.{ state with playground= { playground with size } }
  | A.Playground (A.ResetBoard), {S.playground= {size}} ->
    RR.Update S.{ state with playground= Playground.make size }
  | _ -> RR.NoUpdate

let component = RR.reducerComponent "App"

let make ?(initial_page=P.Problem) _children = {
  component with
  initialState= initial_state initial_page;
  reducer;
  render= fun self ->
    let content = match self.state with
        {S.page= P.Problem} -> Pages.Problem.c []
      | {S.page= P.Submit} -> Pages.Submit.c []
      | {S.page= P.Playground; playground= {board= {size} as board} as playground} ->
        let dispatcher = Pages.Playground.{
            gear_click= (fun i _ ->
                let x, y = i mod size, i / size in
                match Board.touch x y board with
                  Some board -> self.send (A.Playground (A.ClickGear board))
                | None -> ());
            change_size= (fun e ->
                let size = e |> target_value |> int_of_string |> max 2 |> min 8 in
                self.send (A.Playground (A.ChangeSize size)));
            reset= (fun _ ->
                self.send (A.Playground (A.ResetBoard)))
          } in
        Pages.Playground.c ~playground ~dispatcher [] in
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