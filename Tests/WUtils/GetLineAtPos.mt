(* Tests for: WUtils`WUtils`GetLineAtPos

   Author: danielb
*)

Test[
    WUtils`WUtils`GetLineAtPos["abc\ndef\nghi", 1]
    ,
    {1, 3}
    ,
    TestID -> "GetLineAtPos-20150221-FKHZPB"
]

Test[
    WUtils`WUtils`GetLineAtPos["abc\ndef\nghi", 6]
    ,
    {5, 7}
    ,
    TestID -> "GetLineAtPos-20150221-LG7LD9"
]

Test[
    WUtils`WUtils`GetLineAtPos["abc\ndef\nghi", 10]
    ,
    {9, 11}
    ,
    TestID -> "GetLineAtPos-20150221-YQQYAS"
]

(* Last character of string *)
Test[
    WUtils`WUtils`GetLineAtPos["abc\ndef\nghi", 11]
    ,
    {9, 11}
    ,
    TestID -> "GetLineAtPos-20150221-C9PHNZ"
]

(* If the current position is a newline, returns the previous line. *)
Test[
    WUtils`WUtils`GetLineAtPos["abc\ndef\nghi", 4]
    ,
    {1, 3}
    ,
    TestID -> "GetLineAtPos-20150221-08CJSM"
]

(* On the trailing newline of an empty line. *)
Test[
    WUtils`WUtils`GetLineAtPos["abc\n\ndef\nghi", 5]
    ,
    {6, 5}
    ,
    TestID -> "GetLineAtPos-20150221-Z3KZ2L"
]