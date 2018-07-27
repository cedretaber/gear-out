open BSReact

type dispatcher = {
  gear_click: int -> RE.Mouse.t -> unit;
  change_size: RE.Form.t -> unit;
  reset: RE.Mouse.t -> unit
}

let component = RR.statelessComponent "Playground"

let make ~playground:{State.Playground.board; size; count} ~dispatcher:{gear_click; change_size; reset} _children = {
  component with
  render= fun _self ->
    let class_name =
      if Board.is_cleared board then
        "playground cleared"
      else
        "playground" in
    div ~class_name [
      div ~class_name:"input-set" [
        input ~type_:"number" ~value:(string_of_int size) ~on_change:change_size [];
        button ~on_click:reset [
          s {j|リセット|j}
        ]
      ];
      div ~class_name:"board-holder" [
        div ~class_name:"board" [
          Parts.Board.c ~board ~gear_click []
        ]
      ];
      div ~class_name:"count" [
        p [
          s {j|操作回数: $(count)|j}
        ]
      ]
    ]
}

let c ~playground ~dispatcher children =
  RR.element @@ make ~playground ~dispatcher children
