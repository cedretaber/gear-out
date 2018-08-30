open Jest
open Expect

let () =

describe "FromInput" (fun () ->
  let module FromInput = App.FromInput in
  describe "parse" (fun () ->
    let expected = Some Board.{
      size= 4;
      gears= [|
        0; 1; 2; 3;
        1; 2; 3; 0;
        2; 3; 0; 1;
        3; 0; 1; 2
      |]
    } in
    describe "competitive" (fun () ->
      test "should make a board correctly" (fun () ->
        let str = {|4
1 2 3 4
2 3 4 1
3 4 1 2
4 1 2 3
|} in
        expect (FromInput.parse str) |> toEqual expected);
    );
    describe "doukaku" (fun () ->
      test "should make a board correctly" (fun () ->
        let str = {|test("4|1234,2341,3412,4123")|} in
        expect (FromInput.parse str) |> toEqual expected);
    );
  );
);

describe "FromOutput" (fun () ->
  let module FromOutput = App.FromOutput in
  describe "parse" (fun () ->
    let expected = Belt.Result.Ok [
      1,1; 2,2; 3,3; 0,2; 0,3; 2,2; 3,0; 3,1
    ] in
    describe "competitive" (fun () ->
      test "should parse ops correctly" (fun () ->
        let str = {|2 2
3 3
4 4
1 3
1 4
3 3
4 1
4 2|} in
        expect (FromOutput.parse str) |> toEqual expected);
    );
    describe "doukaku" (fun () ->
      test "should parse ops correctly" (fun () ->
        let str = "2,2|3,3|4,4|1,3|1,4|3,3|4,1|4,2" in
        expect (FromOutput.parse str) |> toEqual expected);
    );
  );
);