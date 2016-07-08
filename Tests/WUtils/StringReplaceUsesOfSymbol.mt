(* Tests for: WUtils`WUtils`StringReplaceUsesOfSymbol

   Author: danielb
*)

Test[
    WUtils`WUtils`StringReplaceUsesOfSymbol[
        "MyContext`MySymbol = 1",
        "MyContext`",
        "MySymbol",
        "NewContext`",
        "NewSymbol"
    ]
    ,
    "NewContext`NewSymbol = 1"
    ,
    TestID -> "StringReplaceUsesOfSymbol-20160121-VBC4HW"
]

Test[
    WUtils`WUtils`StringReplaceUsesOfSymbol[
        "Options[MySymbol] = {}",
        "MyContext`",
        "MySymbol",
        "NewContext`",
        "NewSymbol"
    ]
    ,
    "Options[NewSymbol] = {}"
    ,
    TestID -> "StringReplaceUsesOfSymbol-20160121-KRPWIB"
]

Test[
    WUtils`WUtils`StringReplaceUsesOfSymbol[
        "MySymbol::blah = \"blah\"",
        "MyContext`",
        "MySymbol",
        "NewContext`",
        "NewSymbol"
    ]
    ,
    "NewSymbol::blah = \"blah\""
    ,
    TestID -> "StringReplaceUsesOfSymbol-20160121-MMBFMI"
]