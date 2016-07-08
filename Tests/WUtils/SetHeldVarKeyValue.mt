(* Tests for: WUtils`WUtils`SetHeldVarKeyValue

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
            ReleaseHold[heldVar]
        )
    ]
    ,
    {"MyKey" -> "MyValue"}
    ,
    TestID -> "SetHeldVarKeyValue-20150221-V5WPQG"
]

Test[
    Module[
        {heldVar = WUtils`WUtils`NewHeldVar["testVar"]},
        (
            WUtils`WUtils`SetHeldVarKeyValue[
                heldVar,
                "MyKey1" -> "MyValue1"
            ];
            WUtils`WUtils`SetHeldVarKeyValue[
                heldVar,
                "MyKey2" -> "MyValue2"
            ];
            ReleaseHold[heldVar]
        )
    ]
    ,
    {"MyKey1" -> "MyValue1", "MyKey2" -> "MyValue2"}
    ,
    TestID -> "SetHeldVarKeyValue-20150221-OEZAL3"
]

Test[
    Module[
        {heldVar = WUtils`WUtils`NewHeldVar["testVar"]},
        (
            WUtils`WUtils`SetHeldVarKeyValue[
                heldVar,
                "MyKey1" -> "MyValue1"
            ];
            WUtils`WUtils`SetHeldVarKeyValue[
                heldVar,
                "MyKey2" -> "MyValue2"
            ];
            ReleaseHold[heldVar]
        )
    ]
    ,
    {"MyKey1" -> "MyValue1", "MyKey2" -> "MyValue2"}
    ,
    TestID -> "SetHeldVarKeyValue-20150221-SW0Z2W"
]

Test[
    Module[
        {heldVar = None},
        (
            WUtils`WUtils`NewHeldVar[
                heldVar,
                "MyKey" -> "MyValue"
            ];
            ReleaseHold[heldVar]
        )
    ]
    ,
    None
    ,
    TestID -> "SetHeldVarKeyValue-20150221-WH1EWK"
]