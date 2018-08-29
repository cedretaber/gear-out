open! BSReact

type dispatcher = {
  gear_click: int -> RE.Mouse.t -> unit;
  change_size: RE.Form.t -> unit;
  reset: RE.Mouse.t -> unit;
  change_board_input: RE.Form.t -> unit;
  click_make_board: RE.Mouse.t -> unit;
  change_ops_input: RE.Form.t -> unit;
  click_apply_ops: RE.Mouse.t -> unit;
}

let show_history =
  let impl sep_of_point sep_of_line hist =
    hist
    |> List.fold_left
      (fun acc (x, y) ->
        let point =
          [x; y]
          |> List.map (fun n -> string_of_int (n + 1))
          |> String.concat sep_of_point in
        point :: acc)
      []
    |> String.concat sep_of_line in
  function
      State.Submit.Competitive -> impl " " "\n"
    | _ -> impl "," "|"

let component = RR.statelessComponent "Playground"

let make
    ~input_style
    ~playground:{State.Playground.board; size; count; board_input; ops_input; history_reversed}
    ~dispatcher:{gear_click; change_size; reset; change_board_input; click_make_board; change_ops_input; click_apply_ops}
    _children = {
  component with
  render= fun _self ->
    let class_name =
      if Board.is_cleared board then
        "playground cleared"
      else
        "playground" in
    div ~class_name [
      div ~class_name:"playground-inputs" [
        div ~class_name:"input-set" [
          input ~type_:"number" ~value:(string_of_int size) ~on_change:change_size [];
          button ~on_click:reset [
            s {j|リセット|j}
          ]
        ];
        p [ s {j|ここに問題の入力を入れると、その状況を再現します。|j} ];
        div ~class_name:"input-set" [
          textarea ~on_change:change_board_input ~value:board_input [];
          button ~on_click:click_make_board [
            s {j|再現|j}
          ]
        ];
        p [ s {j|ここに問題の出力を入れると、その出力を実行します。|j} ];
        div ~class_name:"input-set" [
          textarea ~on_change:change_ops_input ~value:ops_input [];
          button ~on_click:click_apply_ops [
            s {j|実行|j}
          ]
        ]
      ];
      div ~class_name:"board-holder" [
        div ~class_name:"board" [
          Parts.Board.c ~board ~gear_click []
        ]
      ];
      div ~class_name:"playground-data" [
        div ~class_name:"count" [
          p [
            s {j|操作回数: $(count)|j}
          ]
        ];
        textarea ~read_only:true ~value:(show_history input_style history_reversed) [];
      ]
    ]
}

let c ~input_style ~playground ~dispatcher children =
  RR.element @@ make ~input_style ~playground ~dispatcher children