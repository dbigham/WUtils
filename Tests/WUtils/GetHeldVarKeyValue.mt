(* Tests for: WUtils`WUtils`GetHeldVarKeyValue

   Author: danielb
*)

Test[
    Module[
        {heldVar = WUtils`WUtils`NewHeldVar["testVar"]},
        (
            WUtils`WUtils`SetHeldVarKeyValue[
                heldVar,
                "MyKey" -> "MyValue"
            ];
            WUtils`WUtils`GetHeldVarKeyValue[heldVar, "MyKey"]
        )
    ]
    ,
    "MyValue"
    ,
    TestID -> "GetHeldVarKeyValue-20150221-637SCA"
]