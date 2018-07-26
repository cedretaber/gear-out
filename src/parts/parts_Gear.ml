open BSReact

let component = RR.statelessComponent "Gear"

let make ~r ?(class_name="") ?on_click _children = {
  component with
  render= fun _self ->
    let correct = if r mod 4 = 0 then "correct" else "" in
    let class_name = {j|gear-box $(class_name) $(correct)|j} in
    let deg = 90 * r in
    let rotate = {j|rotate($(deg)deg)|j} in
    let style = Obj.magic [%bs.obj {
        transform= rotate
      }] in
    div ~class_name ~style ?on_click [
      div ~class_name:"gear" [];
      div ~class_name:"arrow" []
    ]
}

let c ~r ?class_name ?on_click children =
  RR.element @@ make ~r ?class_name ?on_click children