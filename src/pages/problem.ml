open BSReact

module GearBoard = struct
  let style = Obj.magic [%bs.obj {
      width= "220px";
      height= "220px";
      display= "grid";
      gridTemplateRows= "1fr 1fr 1fr 1fr";
      gridTemplateColumns= "1fr 1fr 1fr 1fr"
    }]

  let component = RR.statelessComponent "GearBoard"

  let make ~gears _children = {
    component with
    render= fun _self ->
      let children = List.map (fun r -> Gear.c ~r []) gears in
      div ~class_name:"gear-board" [
        div ~style children
      ]
  }

  let c ~gears children =
    RR.element @@ make ~gears children
end

module CodeBlock = struct
  let component = RR.statelessComponent "CodeBlock"

  let make children = {
    component with
    render= fun _self ->
      div ~class_name:"code-block" [
        pre [
          code children
        ]
      ]
  }

  let c children =
    RR.element @@ make children
end

let initialState () = State.Problem

let reducer (_: Action.t) (_: State.t) = RR.NoUpdate

let component = RR.statelessComponent "Problem"

let make _children = {
  component with
  render= fun _self ->
    div ~class_name:"problem" [
      h1 [s "Gear Out!"];
      p [s {j|オフラインリアルタイムどう書く E26 問題|j}];
      h2 [s {j|問題文|j}];
      GearBoard.c ~gears:[1; 2; 2; 1; 4; 3; 1; 2; 1; 1; 2; 3; 1; 4; 1; 4] [];
      p [s {j|図のように歯車が設置されています。歯車には印が付いていて、その場所に応じて「上」「右」「下」「左」の向きがあります。|j}];
      p [
        s {j|「上」向きの歯車を、|j};
        b [s {j|正しい向き|j}];
        s {j|の歯車とします。|j}
      ];
      p [s {j|歯車は時計回りに回転させる事ができます。|j}];
      p [
        s {j|ある歯車を回転させると、その上下左右の歯車が|j};
        b [s {j|反時計回りに|j}];
        s {j|回転します。|j}
      ];
      p [s {j|全ての歯車を「正しい向き」にするには、どのような順番で歯車を回していけば良いでしょうか？|j}];
      h2 [s {j|入力|j}];
      p [s {j|入力には2つの形式があり、好きな方を選ぶ事ができます。どちらも、表される状況は同じです。|j}];
      p [s {j|一つは、全て1行の文字列で受け取る形式です。最初に縦横の大きさがあり、パイプを挟んでその後ろに歯車の状態が並びます。|j}];
      p [s {j|歯車の状態は全て数字で表され、「上」が "1" 、「右」が "2" 、「下」が "3" 、「左」が "4" です。|j}];
      p [s {j|連続する数字が横一列の歯車を表し、各横列をカンマで区切ります。|j}];
      CodeBlock.c [s {j|"4|0000,0000,0000,0000"|j}];
      p [s {j|もう一つは、複数行で受け取る形式です。最初の行に縦横の大きさ N が出力され、以下 N 行に歯車の状態が出力されます。|j}];
      p [s {j|歯車の状態は同じく「上」が "1" 、「右」が "2" 、「下」が "3" 、「左」が "4" ですが、各々の数字が空白区切りで表されます。|j}];
      CodeBlock.c [s {j|4\n0 0 0 0\n0 0 0 0\n0 0 0 0\n0 0 0 0|j}];
      h2 [s {j|出力|j}];
      p [s {j|回答は、歯車を動かす手順を1行ずつ出力してください。|j}];
      p [s {j|動かす歯車は x 軸と y 軸の座標で指定してください。|j}];
      p [s {j|1度の操作で、必ず1つの歯車を時計回りに90度だけ回す必要があります。それ以外の操作を行う事はできません。|j}];
      p [s {j|各行には歯車の座標の他に余計な出力をしてはいけません。|j}];
      p [s {j|不正な行があった場合、その時点で採点は中断され、テストは失敗します。|j}];
      h3 [s {j|出力例1|j}]
    ]
}

let c children =
  RR.element @@ make children