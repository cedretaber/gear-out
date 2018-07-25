open BSReact

let component = RR.statelessComponent "Playground"

let make ~board ~on_gear_click _children = {
  component with
  render= fun _self ->
    let class_name =
      if Board.is_cleared board then
        "playground cleared"
      else
        "playground" in
    div ~class_name [
      div ~class_name:"board" [
        Parts.Board.c ~board ~on_gear_click []
      ]
    ]
}

let c ~board ~on_gear_click children =
  RR.element @@ make ~board ~on_gear_click children
