open BSReact

module A = Action
module S = State
module P = Page

let rewrite_hash: string -> unit = [%raw fun hash -> "location.hash = hash"]

let extract_board_str': string -> string Js.Nullable.t = [%raw fun str -> {| return (str.match(/"([^"]+)"/) || [])[1]; |}]
let extract_board_str str = extract_board_str' str |> Js.Nullable.toOption

module Result = Belt.Result
module TestCase = S.Submit.TestCase

module FromInput = struct
  let gen_parse sep_of_line sep_of_gear str =
    try
      let lines = Js.String.split sep_of_line str in
      let size = Array.length lines in
      let gears =
        lines
        |> Array.map (Js.String.split sep_of_gear)
        |> Array.to_list
        |> Array.concat
        |> Array.map (fun n -> int_of_string n - 1) in
      if
        Belt.Array.every gears (fun n -> n >= 0 && n < 4) && (size * size) = Array.length gears
      then
        Some Board.{size; gears}
      else
        None
    with
      _ -> None
  
  let parse str =
    try
      if String.contains str ',' then
        str
        |. extract_board_str
        |. Belt.Option.flatMap (fun str ->
          match Js.String.split "|" str with
              [|size_str; board_str|] ->
              let size' = int_of_string size_str in
              Belt.Option.flatMap
                (gen_parse "," "" board_str)
                (function ({Board.size} as board) when size = size' -> Some board
                        | _ -> None)
            | _ -> None)
      else
        (match Js.String.split "\n" str |> Array.to_list with
            size_str :: rest ->
            let size' = int_of_string size_str in
            Belt.Option.flatMap
              (gen_parse "\n" " " @@ String.concat "\n" rest)
              (function ({Board.size} as board) when size = size' -> Some board
                      | _ -> None)
          | _ -> None)
    with
      _ -> None
end

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

  let split_testcases str =
    if String.contains str ',' then
      let cases = Js.String.split "\n" str in
      Array.fold_right
        (fun str acm -> match Js.String.split " " str with
             [|n; output|] -> (try (int_of_string n, output) :: acm with _ -> acm)
           | _ -> acm)
        cases
        []
    else
      let cases = Js.String.split "\n\n" str in
      Array.fold_right
        (fun str acm -> match Js.String.split "\n" str |> Array.to_list with
             n :: rest ->
             (try (int_of_string n, String.concat "\n" rest) :: acm
              with _ -> acm)
           | _ -> acm)
        cases
        []
end

module Judge = struct
  let exec ops board =
    List.fold_left Result.(fun result (x, y) ->
        match result with
          Ok board -> (match Board.touch x y board with
              Some board -> Ok board
            | None -> Error (x, y))
        | Error err -> Error err)
      (Result.Ok board)
      ops

  let run ({TestCase.board} as test_case) output =
    match FromOutput.parse @@ Js.String.trim output with
      Error err ->
      TestCase.{ test_case with state= WrongAnswer; message= Some {j|不正な入力です: $(err)|j} }
    | Ok ops -> match exec ops board with
        Error (x, y) ->
        let x, y = x + 1, y + 1 in
        TestCase.{ test_case with state= WrongAnswer; message= Some {j|不正な座標です: ($(x), $(y))|j} }
      | Ok board -> if Board.is_cleared board then
          TestCase.{ test_case with state= Accepted; board }
        else
          TestCase.{ test_case with state= WrongAnswer; board }
end

let initial_state page () =
  Random.self_init ();
  S.{ page; submit= Submit.make (); playground= Playground.make 4 }

let reducer action state = match action with
    A.ChangePage page -> RR.Update S.{ state with page }
  | A.Submit action -> (match action, state with
        A.ToggleTestCase idx, {S.submit= {state= Ready; test_cases} as submit} ->
        let test_case = test_cases.(idx) in
        let test_cases = Array.copy test_cases in
        test_cases.(idx) <- { test_case with is_open= not test_case.is_open };
        RR.Update { state with submit= { submit with test_cases } }
      | A.ChangeInputStyle input_style, {S.submit= {state= Ready} as submit} ->
        RR.Update { state with submit= { submit with input_style } }
      | A.AllToggle is_open, {S.submit= {state= Ready; test_cases} as submit} ->
        let test_cases = Array.map (fun test_case -> S.Submit.TestCase.{ test_case with is_open }) test_cases in
        RR.Update { state with submit= { submit with test_cases } }
      | A.ChangeOutput (idx, output), {S.submit= {state= Ready; test_cases} as submit} ->
        let test_case = test_cases.(idx) in
        let test_cases = Array.copy test_cases in
        test_cases.(idx) <- { test_case with output };
        RR.Update { state with submit= { submit with test_cases } }
      | A.SubmitAnswer idx, {S.submit= {state= Ready; test_cases} as submit} ->
        let {TestCase.output} = test_cases.(idx) in
        RR.UpdateWithSideEffects (
          { state with submit= {submit with state= Testing} },
          fun self ->
            self.send @@ A.run_board [idx, output])
      | A.ChangeOutputAll output, {S.submit= {state= Ready} as submit} ->
        RR.Update { state with submit= { submit with output } }
      | A.SubmitAnswerAll, {S.submit= {state= Ready; output} as submit} ->
        RR.UpdateWithSideEffects (
          { state with submit= {submit with state= Testing} },
          fun self ->
            self.send @@ A.run_board @@ FromOutput.split_testcases output)
      | A.RunBoard ((idx, output) :: rest), {S.submit= {state= Testing; test_cases} as submit} ->
        (match test_cases.(idx) with
           {TestCase.state= Waiting} as test_case ->
           test_cases.(idx) <- Judge.run test_case output;
           RR.UpdateWithSideEffects (
             { state with submit= { submit with test_cases } },
             fun self ->
               self.send @@ A.run_board rest)
         | _ -> RR.SideEffects (fun self -> self.send @@ A.run_board rest))
      | A.RunBoard [], {S.submit= {state= Testing} as submit} ->
        RR.Update { state with submit= { submit with state= Ready} }
      | A.ResetSubmit, {S.submit= {state= Ready}} ->
        RR.Update { state with submit= S.Submit.make () }
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
      | A.ChangeBoardInput board_input, {S.playground} ->
        RR.Update S.{ state with playground= { playground with board_input } }
      | A.ClickMakeBoard, {S.playground= {board_input}} ->
        (match FromInput.parse board_input with
            Some board -> RR.Update S.{ state with playground= S.Playground.from_board board }
          | None -> RR.NoUpdate)
      | _ -> RR.NoUpdate)
  | _ -> RR.NoUpdate

let submit_dispatcher {RR.send} = Pages.Submit.{
    header_click= (fun i event ->
        RE.Mouse.preventDefault event;
        send @@ A.toggle_test_case i);
    change_input_style= (fun style event ->
        RE.Mouse.preventDefault event;
        send @@ A.change_input_style style);
    all_toggle= (fun is_open event ->
        RE.Mouse.preventDefault event;
        send @@ A.all_toggle is_open);
    change_output= (fun i event ->
        send @@ A.change_output i @@ target_value event);
    submit_answer= (fun i event ->
        RE.Mouse.preventDefault event;
        send @@ A.submit_answer i);
    change_output_all= (fun event ->
        send @@ A.change_output_all @@ target_value event);
    submit_answer_all= (fun event ->
        RE.Mouse.preventDefault event;
        send A.submit_answer_all);
    reset= (fun event ->
        RE.Mouse.preventDefault event;
        send A.reset_submit)
  }

let playground_dispatcher {RR.send} {S.Playground.board= {size} as board} = Pages.Playground.{
    gear_click= (fun i _ ->
        let x, y = i mod size, i / size in
        match Board.touch x y board with
          Some board -> send @@ A.click_gear board
        | None -> ());
    change_size= (fun event ->
        let size = event |> target_value |> int_of_string |> max 2 |> min 8 in
        send @@ A.change_size size);
    reset= (fun event ->
        RE.Mouse.preventDefault event;
        send A.reset_board);
    change_board_input= (fun event ->
        send @@ A.change_board_input @@ target_value event);
    click_make_board= (fun event ->
        RE.Mouse.preventDefault event;
        send A.click_make_board)
  }

let component = RR.reducerComponent "App"

let make ?(initial_page=P.Problem) _children = {
  component with
  initialState= initial_state initial_page;
  reducer;
  render= fun self ->
    let content, problem_tab, submit_tab, playground_tab = match self.state with
        {S.page= P.Problem} ->
        Pages.Problem.c [], "active", "", ""
      | {S.page= P.Submit; submit} ->
        Pages.Submit.c ~submit ~dispatcher:(submit_dispatcher self) [], "", "active", ""
      | {S.page= P.Playground; playground} ->
        Pages.Playground.c ~playground ~dispatcher:(playground_dispatcher self playground) [], "", "", "active" in
    let move_page page event =
      RE.Mouse.preventDefault event;
      rewrite_hash (match page with P.Problem -> "problem" | P.Submit -> "submit" | P.Playground -> "playground");
      self.send @@ A.change_page page in
    div [
      header ~class_name:"main" [
        span ~class_name:"logo" [ s "Gears Out!" ];
        nav [
          ul [
            li ~class_name:problem_tab ~on_click:(move_page P.Problem) [ a ~href:"#problem" [ s {j|問題|j} ] ];
            li ~class_name:submit_tab ~on_click:(move_page P.Submit) [ a ~href:"#submit" [ s {j|提出|j} ] ];
            li ~class_name:playground_tab ~on_click:(move_page P.Playground) [ a ~href:"#playground" [ s {j|サンプル|j} ] ]
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