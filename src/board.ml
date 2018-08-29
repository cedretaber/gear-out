
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

let to_nums {gears; size} =
  let nums = Array.map (fun n -> (n mod 4 + 4) mod 4 + 1) gears in
  let nums, _, _ =
    Array.fold_right (fun i -> function
          acm, line, 1 -> (i :: line) :: acm, [], size
        | acm, line, n -> acm, i :: line, n - 1)
      nums
      ([], [], size) in
  nums

let nums_to_string ({size} as board) sep_of_num sep_of_line sep_of_size =
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