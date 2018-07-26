
type t = {
  size: int;
  gears: int array
}

let dirs = [1,0; 0,1; -1,0; 0,-1]

let touch' x y size gears =
  gears.(y * size + x) <- gears.(y * size + x) + 1;
  List.iter
    (fun (dx, dy) ->
       let x, y = x + dx, y + dy in
       if x >= 0 && x < size && y >= 0 && y < size then
         gears.(y * size + x) <- gears.(y * size + x) - 1;)
    dirs

let touch x y ({size; gears} as board) =
  if x < 0 || x >= size || y < 0 || y >= size then
    None
  else
    let gears = Array.copy gears in
    touch' x y size gears;
    Some { board with gears }

let is_cleared {gears} = Belt.Array.every gears @@ fun i -> i mod 4 == 0

let make ?gears size =
  let gears = match gears with
      Some gears when Array.length gears = size * size -> gears
    | _ ->
      let gears = Belt.Array.make (size * size) 0 in
      for x = 0 to size - 1 do
        for y = 0 to size - 1 do
          let c = Random.int 4 in
          for _ = 0 to c do
            touch' x y size gears
          done
        done
      done;
      gears in
  { size; gears }