(* Tests for: WUtils`WUtils`Private`addCaptureToDownValue

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`addCaptureToDownValue[
        HoldPattern[myFunc[myArg]] :> Print[myArg]
    ]
    ,
    HoldPattern[myFunc[myArg]] :>
        WUtils`WUtils`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg}],
            Print[myArg]
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-73BBGZ"
]

Test[
    WUtils`WUtils`Private`addCaptureToDownValue[
        HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :> 1
    ]
    ,
    HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :>
        WUtils`WUtils`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg1, myArg2, myArg3}],
            1
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-WH9A5V"
]

Test[
    WUtils`WUtils`ReplaceSymbolsUsingPatterns[
        WUtils`WUtils`Private`addCaptureToDownValue[
            HoldPattern[myFunc[myArg, OptionsPattern[]]] :> Print[myArg]
        ],
        {
            RegularExpression["bindingAddedDynamicallyByGetOptionsPatternBinding.+"] :>
                "bindingAddedDynamicallyByGetOptionsPatternBinding"
        }
    ]
    ,
    HoldPattern[
        myFunc[myArg, bindingAddedDynamicallyByGetOptionsPatternBinding:OptionsPattern[]]
    ] :>
        WUtils`WUtils`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg, bindingAddedDynamicallyByGetOptionsPatternBinding}],
            Print[myArg]
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-K3SSGV"
]

Test[
    WUtils`WUtils`Private`addCaptureToDownValue[
        HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :>
            WUtils`WUtils`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg1, myArg2, myArg3}],
                1
            ]
    ]
    ,
    HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :>
        WUtils`WUtils`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg1, myArg2, myArg3}],
            1
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-Y9D5S4"
]