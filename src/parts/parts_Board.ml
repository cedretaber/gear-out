open BSReact

let make_style size =
  let temp = Array.make size "1fr" |> Array.to_list |> String.concat " " in
  let size = string_of_int (55 * size) in
  let size = {j|$(size)px|j} in
  Obj.magic [%bs.obj {
    width= size;
    height= size;
    display= "grid";
    gridTemplateRows= temp;
    gridTemplateColumns= temp
  }]

let component = RR.statelessComponent "GearBoard"

let make ~board:{Board.size; gears} ?gear_click ?(class_name="") _children = {
  component with
  render= fun _self ->
    let class_name = {j|gear-board $(class_name)|j} in
    let style = make_style size in
    let children =
      gears
      |> Array.to_list
      |> List.mapi (fun i r ->
          let on_click = Belt.Option.map gear_click @@ fun f -> f i in
          Parts_Gear.c ~r ?on_click []
        ) in
    div ~class_name [
      div ~style children
    ]
}

let c ~board ?gear_click ?class_name children =
  RR.element @@ make ~board ?gear_click ?class_name children