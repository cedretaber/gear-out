open BSReact

let component = RR.statelessComponent "Submit"

let make _children = {
  component with
  render= fun _self ->
    div [
      s "submit"
    ]
}

let c children =
  RR.element @@ make children