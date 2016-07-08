(* Tests for: WUtils`WUtils`Private`rowBoxFix1

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`rowBoxFix1[RowBox[{"b"}]]
    ,
    "<<RowBox>>"[{"b"}]
    ,
    TestID -> "rowBoxFix1-20150225-F3WIBY"
]

Test[
    WUtils`WUtils`Private`rowBoxFix1["<<RowBox>>"[{"b"}]]
    ,
    "<<<RowBox>>>"[{"b"}]
    ,
    TestID -> "rowBoxFix1-20150225-1LCYRW"
]

Test[
    WUtils`WUtils`Private`rowBoxFix1[
        RowBox[{RowBox[{"1", "2", "3"}]}]
    ]
    ,
    "<<RowBox>>"[{"<<RowBox>>"[{"1", "2", "3"}]}]
    ,
    TestID -> "rowBoxFix1-20150226-WCXNHH"
]