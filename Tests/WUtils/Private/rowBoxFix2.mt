(* Tests for: WUtils`WUtils`Private`rowBoxFix2

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`rowBoxFix2["\"<<RowBox>>\"[{\"b\"}]"]
    ,
    "RowBox[{\"b\"}]"
    ,
    TestID -> "rowBoxFix2-20150225-A5KL34"
]

Test[
    WUtils`WUtils`Private`rowBoxFix2["\"<<<RowBox>>>\"[{\"b\"}]"]
    ,
    "\"<<RowBox>>\"[{\"b\"}]"
    ,
    TestID -> "rowBoxFix2-20150226-M85EFP"
]