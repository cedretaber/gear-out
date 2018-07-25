let initial_page =
  match [%raw "location.hash"] with
    "#problem" -> Some Page.Problem
  | "#submit" -> Some Page.Submit
  | "#playground" -> Some Page.Playground
  | _ -> None

let _ = ReactDOMRe.renderToElementWithId (App.c ?initial_page []) "app"