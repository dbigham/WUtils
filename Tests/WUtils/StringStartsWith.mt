(* Tests for: WUtils`WUtils`StringStartsWith

   Author: danielb
*)

Test[
    WUtils`WUtils`StringStartsWith["abcdef", {"abc"}]
    ,
    True
    ,
    TestID -> "StringStartsWith-20150304-LBH5N5"
]

Test[
    WUtils`WUtils`StringStartsWith["abcdef", {"xyz"}]
    ,
    False
    ,
    TestID -> "StringStartsWith-20150304-YD57S2"
]

Test[
    WUtils`WUtils`StringStartsWith["abcdef", {"xyz", "abc"}]
    ,
    True
    ,
    TestID -> "StringStartsWith-20150304-D755C7"
]

Test[
    WUtils`WUtils`StringStartsWith["a", {"abc"}]
    ,
    False
    ,
    TestID -> "StringStartsWith-20150304-SLGAOD"
]