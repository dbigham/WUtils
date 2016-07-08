(* Tests for: WUtils`WUtils`StringTakeByDelim

   Author: danielb
*)

Test[
    WUtils`WUtils`StringTakeByDelim[
        "WUtils`WUtils`Private`MyNewFunc",
        "`",
        -1
    ]
    ,
    "MyNewFunc"
    ,
    TestID -> "StringTakeByDelim-20150201-O7COPZ"
]