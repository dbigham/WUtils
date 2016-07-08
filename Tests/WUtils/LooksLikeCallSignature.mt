(* Tests for: WUtils`WUtils`LooksLikeCallSignature

   Author: danielb
*)

Test[
    LooksLikeCallSignature[HoldComplete[CouldBeWLSymbolQ["test"]], CouldBeWLSymbolQ]
    ,
    False
    ,
    TestID -> "LooksLikeCallSignature-20150126-P5GM8X"
]

Test[
    LooksLikeCallSignature[HoldComplete[CouldBeWLSymbolQ[str]], CouldBeWLSymbolQ]
    ,
    True
    ,
    TestID -> "LooksLikeCallSignature-20150126-KG8LLB"
]

Test[
    WUtils`WUtils`LooksLikeCallSignature[
        HoldComplete[
            WUtils`WUtils`GetVariablePossiblyFromParentPackage[context, symbolName]
        ],
        WUtils`WUtils`GetVariablePossiblyFromParentPackage
    ]
    ,
    True
    ,
    TestID -> "LooksLikeCallSignature-20150202-8FT3MD"
]