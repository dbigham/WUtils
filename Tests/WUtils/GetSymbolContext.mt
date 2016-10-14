(* Tests for: WUtils`WUtils`GetSymbolContext

   Author: danielb
*)

Test[
    WUtils`WUtils`GetSymbolContext["GetSymbolContext"]
    ,
    "WUtils`WUtils`"
    ,
    TestID -> "GetSymbolContext-20151223-IEWEQA"
]

Test[
    Module[
        {},
        (
            myTestVar = 1;
            With[
                {tmp = WUtils`WUtils`GetSymbolContext["myTestVar"]},
                (
                    Remove[myTestVar];
                    tmp
                )
            ]
        )
    ]
    ,
    "Global`"
    ,
    TestID -> "GetSymbolContext-20151223-NWNA46"
]