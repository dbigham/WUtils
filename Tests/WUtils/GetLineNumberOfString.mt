(* Tests for: WUtils`WUtils`GetLineNumberOfString

   Author: danielb
*)

Test[
    WUtils`WUtils`GetLineNumberOfString["abc", "b"]
    ,
    1
    ,
    TestID -> "GetLineNumberOfString-20150202-VBVSFC"
]

Test[
    WUtils`WUtils`GetLineNumberOfString["abc", "x"]
    ,
    None
    ,
    TestID -> "GetLineNumberOfString-20150202-HWTR17"
]

Test[
    WUtils`WUtils`GetLineNumberOfString["abc\ndef", "e"]
    ,
    2
    ,
    TestID -> "GetLineNumberOfString-20150202-6JF6KP"
]