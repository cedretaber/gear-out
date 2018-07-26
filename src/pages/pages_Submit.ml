open BSReact

let copy_to_clipboard: string -> unit = [%bs.raw fun query -> {|
  const target = document.querySelector(query)
  target.select()
  document.execCommand("Copy")
|}]

module Submit = State.Submit

module ToInput = struct

  let to_nums {Board.gears; size} =
    let nums = Array.map (fun n -> (n mod 4 + 4) mod 4 + 1) gears in
    let nums, _, _ =
      Array.fold_right (fun i -> function
            acm, line, 1 -> (i :: line) :: acm, [], size
          | acm, line, n -> acm, i :: line, n - 1)
        nums
        ([], [], size) in
    nums

  let nums_to_string ({Board.size} as board) sep_of_num sep_of_line sep_of_size =
    let nums =
      board
      |> to_nums
      |> List.map (fun line -> String.concat sep_of_num @@ List.map string_of_int line)
      |> String.concat sep_of_line in
    String.concat sep_of_size [string_of_int size; nums]

  let competitive board =
    nums_to_string board " " "\n" "\n"

  let doukaku board =
    nums_to_string board "" "," "|"
end

let test_data_obj () = Js.Dict.(Js.Json.(
    let obj = empty () in
    set obj "event_id" @@ string "E26";
    set obj "event_url" @@ string "";
    obj
  ))

module TestCasePanel = struct

  module TC = State.Submit.TestCase

  type dispatcher = {
    header_click: RE.Mouse.t -> unit;
    change_output: RE.Form.t -> unit;
    submit_answer: RE.Mouse.t -> unit;
  }

  let component = RR.statelessComponent "TestCasePanel"

  let make
      ~idx
      ~input_style
      ~test_case:{TC.state; is_open; board; output; message}
      ~dispatcher:{header_click; change_output; submit_answer}
      _children = {
    component with
    render= fun _self ->
      let header =
        div ~class_name:"test-case-header" ~on_click:header_click [
          h3 [
            s {j|Case#$(idx)|j}
          ]
        ] in
      let panel_class = match state with
          TC.Waiting -> ""
        | TC.Accepted -> "accepted"
        | TC.WrongAnswer -> "wrong-answer" in
      if is_open then
        let input_text = match input_style with
            Submit.Competitive -> ToInput.competitive board
          | Submit.Doukaku ->
            let src = ToInput.doukaku board in
            {j|test("$(src)");|j}
          | Submit.JSON ->
            let src = ToInput.doukaku board in
            Js.Dict.(Js.Json.(
                let test_data = empty () in
                set test_data "number" @@ number @@ float_of_int idx;
                set test_data "src" @@ string src;
                let test_data = [|test_data|] in
                let obj = test_data_obj () in
                set obj "test_data" @@ objectArray test_data;
                stringify @@ object_ obj)) in
        let test_case_copy _ =
          copy_to_clipboard {j|.test-case-input.case-$(idx)|j} in
        div ~class_name:{j|test-case-panel $(panel_class)|j} [
          header;
          div ~class_name:"test-case-panel-body" [
            Parts.Board.c ~board [];
            div ~class_name:"test-case-io" [
              textarea ~class_name:{j|test-case-input case-$(idx)|j} ~read_only:true ~value:input_text [];
              button ~class_name:"test-case-copy" ~on_click:test_case_copy [
                s {j|クリップボードにコピー|j}
              ];
              textarea ~class_name:"test-case-output" ~read_only:(state <> TC.Waiting) ~on_change:change_output ~value:output [];
              button ~class_name:"test-case-submit" ~disabled:(state <> TC.Waiting) ~on_click:submit_answer [
                s {j|提出|j}
              ]
            ];
          ];
          match message with
            Some message ->
            div ~class_name:"error-message" [
              s message
            ]
          | None -> RR.null
        ]
      else
        div ~class_name:{j|test-case-panel $(panel_class)|j} [
          header
        ]
  }

  let c ~idx ~input_style ~test_case ~dispatcher children =
    RR.element @@ make ~idx ~input_style ~test_case ~dispatcher children
end

type dispatcher = {
  header_click: int -> RE.Mouse.t -> unit;
  change_input_style: Submit.input_style -> RE.Mouse.t -> unit;
  all_toggle: bool -> RE.Mouse.t -> unit;
  change_output: int -> RE.Form.t -> unit;
  submit_answer: int -> RE.Mouse.t -> unit;
}

let component = RR.statelessComponent "Submit"

let make
    ~submit:{Submit.state; input_style; test_cases}
    ~dispatcher:{header_click; change_input_style; all_toggle; change_output; submit_answer}
    _children = {
  component with
  render= fun _self ->
    let test_cases =
      test_cases
      |> Array.mapi (fun idx test_case ->
          let dispatcher = TestCasePanel.{
              header_click= header_click idx;
              change_output= change_output idx;
              submit_answer= submit_answer idx
            } in
          TestCasePanel.c ~idx ~input_style ~test_case ~dispatcher []) 
      |> Array.to_list in
    let input_style_buttons =
      Submit.[Competitive, {j|競プロ風|j}; Doukaku, {j|どうかく風|j}; JSON, "JSON"]
      |> List.map (fun (style, label) ->
          button ~on_click:(change_input_style style) ~disabled:(input_style = style) [ s label ]) in
    let all_toggle_buttons =
      [
        button ~on_click:(all_toggle false) [ s {j|全て閉じる|j} ];
        button ~on_click:(all_toggle true) [ s {j|全て開く|j} ]
      ] in
    div ~class_name:"submit" [
      div [
        div ~class_name:"input-style-buttons" input_style_buttons;
        div ~class_name:"all-toggle-buttons" all_toggle_buttons
      ];
      div ~class_name:"test-case-list" test_cases
    ]
}

let c ~submit ~dispatcher children =
  RR.element @@ make ~submit ~dispatcher children