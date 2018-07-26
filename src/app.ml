open BSReact

module A = Action
module S = State
module P = Page

let rewrite_hash: string -> unit = [%bs.raw fun hash -> "location.hash = hash"]

module Result = Belt.Result
module TestCase = S.Submit.TestCase

module FromOutput = struct
  let gen_parse sep_of_point sep_of_xy str =
    let ops =
      str
      |> Js.String.split sep_of_point
      |> Array.map Result.(fun point ->
          match Js.String.split sep_of_xy point with
            [|x; y|] -> Ok (int_of_string x - 1, int_of_string y - 1)
          | err -> Error err) in
    Array.fold_right Result.(fun point -> function
          Ok acm -> (match point with
              Ok point -> Ok (point :: acm)
            | Error err -> Error err)
        | Error err -> Error err)
      ops
      (Result.Ok [])

  let parse str =
    (if String.contains str ',' then gen_parse "|" "," else gen_parse "\n" " ") str
end

module Judge = struct
  let run ops board =
    List.fold_left Result.(fun result (x, y) ->
        match result with
          Ok board -> (match Board.touch x y board with
              Some board -> Ok board
            | None -> Error (x, y))
        | Error err -> Error err)
      (Result.Ok board)
      ops
end

let initial_state page () =
  Random.self_init ();
  S.{ page; submit= Submit.make (); playground= Playground.make 4 }

let reducer action state = match action with
    A.ChangePage page -> RR.Update S.{ state with page }
  | A.Submit action -> (match action, state with
        A.ToggleTestCase idx, {S.submit= {test_cases} as submit} ->
        let test_case = test_cases.(idx) in
        let test_cases = Array.copy test_cases in
        test_cases.(idx) <- { test_case with is_open= not test_case.is_open };
        RR.Update { state with submit= { submit with test_cases } }
      | A.ChangeInputStyle input_style, {S.submit} ->
        RR.Update { state with submit= { submit with input_style } }
      | A.AllToggle is_open, {S.submit= {test_cases} as submit} ->
        let test_cases = Array.map (fun test_case -> S.Submit.TestCase.{ test_case with is_open }) test_cases in
        RR.Update { state with submit= { submit with test_cases } }
      | A.ChangeOutput (idx, output), {S.submit= {test_cases} as submit} ->
        let test_case = test_cases.(idx) in
        let test_cases = Array.copy test_cases in
        test_cases.(idx) <- { test_case with output };
        RR.Update { state with submit= { submit with test_cases } }
      | A.SubmitAnswer idx, {S.submit= {test_cases} as submit} ->
        let ({TestCase.board; output} as test_case) = test_cases.(idx) in
        let test_cases = Array.copy test_cases in
        (match FromOutput.parse output with
           Ok ops ->
           (match Judge.run ops board with
              Ok board -> if Board.is_cleared board then (
                test_cases.(idx) <- TestCase.{ test_case with state= Accepted; board };
                RR.Update { state with submit= { submit with test_cases } }
              ) else (
                test_cases.(idx) <- TestCase.{ test_case with state= WrongAnswer; board };
                RR.Update { state with submit= { submit with test_cases } })
            | Error (x, y) ->
              let message = Some {j|不正な座標です: ($(x), $(y))|j} in
              test_cases.(idx) <- TestCase.{ test_case with state= WrongAnswer; message };
              RR.Update { state with submit= { submit with test_cases } })
         | Error err ->
           let message = Some {j|不正な入力です: $(err)|j} in
           test_cases.(idx) <- TestCase.{ test_case with state= WrongAnswer; message };
           RR.Update { state with submit= { submit with test_cases } })
      | _ -> RR.NoUpdate)
  | A.Playground action -> (match action, state with
        A.ClickGear board, {S.playground= {state= Playing; count} as playground} ->
        if Board.is_cleared board then
          RR.Update S.{ state with playground= Playground.{ playground with state= Cleared; count= count + 1; board } }
        else
          RR.Update S.{ state with playground= Playground.{ playground with board; count= count + 1 } }
      | A.ChangeSize size, {S.playground} ->
        RR.Update S.{ state with playground= { playground with size } }
      | A.ResetBoard, {S.playground= {size}} ->
        RR.Update S.{ state with playground= Playground.make size }
      | _ -> RR.NoUpdate)
  | _ -> RR.NoUpdate

let component = RR.reducerComponent "App"

let make ?(initial_page=P.Problem) _children = {
  component with
  initialState= initial_state initial_page;
  reducer;
  render= fun self ->
    let content = match self.state with
        {S.page= P.Problem} -> Pages.Problem.c []
      | {S.page= P.Submit; submit} ->
        let dispatcher = Pages.Submit.{
            header_click= (fun i event ->
                RE.Mouse.preventDefault event;
                self.send @@ A.toggle_test_case i);
            change_input_style= (fun style event ->
                RE.Mouse.preventDefault event;
                self.send @@ A.change_input_style style);
            all_toggle= (fun is_open event ->
                RE.Mouse.preventDefault event;
                self.send @@ A.all_toggle is_open);
            change_output= (fun i event ->
                let output = target_value event in
                self.send @@ A.change_output i output);
            submit_answer= (fun i event ->
                RE.Mouse.preventDefault event;
                self.send @@ A.submit_answer i)
          } in
        Pages.Submit.c ~submit ~dispatcher []
      | {S.page= P.Playground; playground= {board= {size} as board} as playground} ->
        let dispatcher = Pages.Playground.{
            gear_click= (fun i _ ->
                let x, y = i mod size, i / size in
                match Board.touch x y board with
                  Some board -> self.send @@ A.click_gear board
                | None -> ());
            change_size= (fun event ->
                let size = event |> target_value |> int_of_string |> max 2 |> min 8 in
                self.send @@ A.change_size size);
            reset= (fun _ ->
                self.send A.reset_board)
          } in
        Pages.Playground.c ~playground ~dispatcher [] in
    let move_page page event =
      RE.Mouse.preventDefault event;
      rewrite_hash (match page with P.Problem -> "problem" | P.Submit -> "submit" | P.Playground -> "playground");
      self.send @@ A.change_page page in
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