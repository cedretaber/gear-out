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
      let size = board.size in
      let header =
        div ~class_name:"test-case-header" ~on_click:header_click [
          h3 [
            s {j|Case#$(idx)|j};
            small ~class_name:"test-case-size" [
              s {j|$(size)x$(size)|j}
            ]
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
        div ~class_name:{j|test-case-panel $(panel_class)|j} [
          header;
          div ~class_name:"test-case-panel-body" [
            div ~class_name:"board-holder" [
              Parts.Board.c ~board [];
            ];
            div ~class_name:"test-case-io" [
              div ~class_name:"input-set" [
                textarea ~class_name:{j|test-case-input case-$(idx)|j} ~read_only:true ~value:input_text [];
                button ~class_name:"test-case-copy" ~on_click:(fun _ -> copy_to_clipboard {j|.test-case-input.case-$(idx)|j}) [
                  i ~class_name:"far fa-copy fa-3x" []
                ];
              ];
              div ~class_name:"input-set" [
                textarea ~class_name:"test-case-output" ~read_only:(state <> TC.Waiting) ~on_change:change_output ~value:output [];
                button ~class_name:"test-case-submit" ~disabled:(state <> TC.Waiting) ~on_click:submit_answer [
                  i ~class_name:"fas fa-file-upload fa-3x" []
                ]
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

let make_input_all test_cases = function
    Submit.Competitive ->
    let input =
      test_cases
      |> Array.mapi (fun i {Submit.TestCase.board} ->
          ToInput.competitive board)
      |> Array.to_list
      |> String.concat "\n" in
    String.concat "\n" [string_of_int @@ Array.length test_cases; input]
  | Submit.Doukaku ->
    test_cases
    |> Array.mapi (fun i {Submit.TestCase.board} ->
        let src = ToInput.doukaku board in
        {j|/*$(i)*/ test("$(src)");|j})
    |> Array.to_list
    |> String.concat "\n"
  | Submit.JSON ->
    let test_data =
      test_cases
      |> Array.mapi (fun i {Submit.TestCase.board} ->
          let src = ToInput.doukaku board in
          Js.Dict.(Js.Json.(
              let test_data = empty () in
              set test_data "number" @@ number @@ float_of_int i;
              set test_data "src" @@ string src;
              test_data)))
      |> Js.Json.objectArray in
    Js.Dict.(Js.Json.(
        let obj = test_data_obj () in
        set obj "test_data" test_data;
        stringify @@ object_ obj))

type dispatcher = {
  header_click: int -> RE.Mouse.t -> unit;
  change_input_style: Submit.input_style -> RE.Mouse.t -> unit;
  all_toggle: bool -> RE.Mouse.t -> unit;
  change_output: int -> RE.Form.t -> unit;
  submit_answer: int -> RE.Mouse.t -> unit;
  change_output_all: RE.Form.t -> unit;
  submit_answer_all: RE.Mouse.t -> unit;
  reset: RE.Mouse.t -> unit;
}

let component = RR.statelessComponent "Submit"

let make
    ~submit:{Submit.state; input_style; test_cases; output}
    ~dispatcher:{header_click; change_input_style; all_toggle; change_output; submit_answer; change_output_all; submit_answer_all; reset}
    _children = {
  component with
  render= fun _self ->
    let input_all_value = make_input_all test_cases input_style in
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
      div ~class_name:"submit-controls" [
        div ~class_name:"button-set input-style-buttons" @@ (div ~class_name:"label" [ s {j|入力形式|j} ]) :: input_style_buttons;
        div ~class_name:"button-set all-toggle-buttons" @@ (div ~class_name:"label" [ s {j|パネル一括操作|j} ]) :: all_toggle_buttons;
        div ~class_name:"reset-button" [
          button ~on_click:reset [
            s {j|リセット|j}
          ]
        ];
      ];
      div ~class_name:"test-case-io-all" [
        div ~class_name:"input-set" [
          textarea ~class_name:"test-case-input" ~read_only:true ~value:input_all_value [];
          button ~class_name:"test-case-copy" ~on_click:(fun _ -> copy_to_clipboard ".test-case-io-all .test-case-input") [
            i ~class_name:"far fa-copy fa-3x" []
          ];
        ];
        div ~class_name:"input-set" [
          textarea ~class_name:"test-case-output" ~on_change:change_output_all ~value:output [];
          button ~class_name:"test-case-submit" ~on_click:submit_answer_all [
            i ~class_name:"fas fa-file-upload fa-3x" []
          ]
        ]
      ];
      div ~class_name:"test-case-list" test_cases
    ]
}

let c ~submit ~dispatcher children =
  RR.element @@ make ~submit ~dispatcher children