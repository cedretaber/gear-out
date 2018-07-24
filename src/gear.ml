open BSReact

let component = RR.statelessComponent "Gear"

let make ~r ?(class_name="") ?(_on_click=(fun _ -> ())) _children = {
  component with
  render= fun _self ->
    let r = string_of_int r in
    let klass = {j|gear-box r$(r) $(class_name)|j} in
    div ~class_name:klass [
      div ~class_name:"gear" [];
      div ~class_name:"arrow" []
    ]
}

let c ~r ?class_name ?on_click children =
  RR.element @@ make ~r ?class_name ?_on_click:on_click children