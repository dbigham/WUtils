(* Tests for: WUtils`WUtils`Private`toWindowMargins

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`toWindowMargins[0.5, 1920]
    ,
    {Automatic, -7.}
    ,
    TestID -> "toWindowMargins-20150226-QNG4W7"
]

Test[
    WUtils`WUtils`Private`toWindowMargins[0, 1920]
    ,
    {-7, Automatic}
    ,
    TestID -> "toWindowMargins-20150226-IQGGOE"
]

Test[
    WUtils`WUtils`Private`toWindowMargins[1, 1920]
    ,
    {1913, Automatic}
    ,
    TestID -> "toWindowMargins-20150226-JR92U3"
]

Test[
    WUtils`WUtils`Private`toWindowMargins[1.5, 1920]
    ,
    {Automatic, -1927.}
    ,
    TestID -> "toWindowMargins-20150226-O4AF12"
]