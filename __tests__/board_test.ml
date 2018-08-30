open Jest
open Expect

let () =

describe "touch" (fun () ->
  let board = Board.{
    size= 4;
    gears= [|
      1; 1; 1; 1;
      1; 1; 1; 1;
      1; 1; 1; 1;
      1; 1; 1; 1
    |]
  } in
  test "should rotate gears correctly" (fun () ->
    let expected = Board.{
      size= 4;
      gears= [|
        1; 0; 1; 1;
        0; 2; 0; 1;
        1; 0; 1; 1;
        1; 1; 1; 1
      |]
    } in
    expect (Board.touch 1 1 board) |> toEqual (Some expected));
);

describe "is_cleared" (fun () ->
  test "should be true with cleared board" (fun () ->
    let board = Board.{
      size= 4;
      gears= [|
        0; 0; 0; 0;
        0; 0; 0; 0;
        0; 0; 0; 0;
        0; 0; 0; 0
      |]
    } in
    expect (Board.is_cleared board) |> toBe true);
  test "should be false with uncleared board" (fun () ->
    let board = Board.{
      size= 4;
      gears= [|
        1; 0; 0; 2;
        2; 3; 3; 1;
        0; 1; 2; 0;
        1; 1; 0; 2
      |]
    } in
    expect (Board.is_cleared board) |> toBe false);
);

let board = Board.{
    size= 4;
    gears= [|
      1; 0; 0; 2;
      2; 3; 3; 1;
      0; 1; 2; 0;
      1; 1; 0; 2
    |]
  } in

describe "competitive" (fun () ->
  test "should return text correctly" (fun () ->
    let expected = {|4
2 1 1 3
3 4 4 2
1 2 3 1
2 2 1 3|} in
    expect (Board.competitive board) |> toEqual expected);
);

describe "doukaku" (fun () ->
  test "should eturn text correctly" (fun () ->
    let expected = "4|2113,3442,1231,2213" in
    expect (Board.doukaku board) |> toEqual expected);
);