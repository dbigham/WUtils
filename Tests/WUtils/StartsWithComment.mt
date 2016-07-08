(* Tests for: WUtils`WUtils`StartsWithComment

   Author: danielb
*)

Test[
    WUtils`WUtils`StartsWithComment[
        "(* Starts with comment. *)\nMyFunc[];"
    ]
    ,
    True
    ,
    TestID -> "StartsWithComment-20150225-6UEHQG"
]

Test[
    WUtils`WUtils`StartsWithComment["MyFunc[];"]
    ,
    False
    ,
    TestID -> "StartsWithComment-20150225-Y4NJRG"
]