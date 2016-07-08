(* Tests for: WUtils`WUtils`CreateHeldVarIfNull

   Author: danielb
*)

Test[
    StringMatchQ[
        ToString[WUtils`WUtils`CreateHeldVarIfNull[Null]],
        StringExpression[
            "HoldComplete[WUtils`WUtils`Private`NewVar`heldVar",
            Repeated[DigitCharacter],
            "]"
        ]
    ]
    ,
    True
    ,
    TestID -> "CreateHeldVarIfNull-20150304-ELVD9H"
]

Test[
    WUtils`WUtils`CreateHeldVarIfNull[
        HoldComplete[WUtils`WUtils`Private`NewVar`heldVar2]
    ]
    ,
    HoldComplete[WUtils`WUtils`Private`NewVar`heldVar2]
    ,
    TestID -> "CreateHeldVarIfNull-20150304-OXWU30"
]

Test[
    ReleaseHold[WUtils`WUtils`CreateHeldVarIfNull[Null, {}]]
    ,
    {}
    ,
    TestID -> "CreateHeldVarIfNull-20150304-XS7AKO"
]