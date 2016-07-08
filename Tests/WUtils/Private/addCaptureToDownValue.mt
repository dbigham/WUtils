(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`addCaptureToDownValue

   Author: danielb
*)

TestExecute[$TestAbortTime = 600]

TestExecute[
    If[TrueQ[Quiet[Get["CalculateTestEnvironment.m"]]===$Failed],
        Get[
        StringCases[$CurrentFile,
        inputfile:(StartOfString~~___~~$PathnameSeparator~~"Tests"~~$PathnameSeparator)~~___
        :> inputfile<>"Utilities"<>$PathnameSeparator<>"CalculateTestEnvironment.m"][[1]]
        ]]
]

TestExecute[$CalculateDataPacletsInit = False;  << "CalculateLoader`"]

TestExecute[$TestAbortTime = $TestAbortTimeInitial]

Test[
    CalculateParse`GeneralLibrary`Private`addCaptureToDownValue[
        HoldPattern[myFunc[myArg]] :> Print[myArg]
    ]
    ,
    HoldPattern[myFunc[myArg]] :>
        CalculateParse`GeneralLibrary`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg}],
            Print[myArg]
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-73BBGZ"
]

Test[
    CalculateParse`GeneralLibrary`Private`addCaptureToDownValue[
        HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :> 1
    ]
    ,
    HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :>
        CalculateParse`GeneralLibrary`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg1, myArg2, myArg3}],
            1
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-WH9A5V"
]

Test[
    CalculateParse`GeneralLibrary`ReplaceSymbolsUsingPatterns[
        CalculateParse`GeneralLibrary`Private`addCaptureToDownValue[
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
        CalculateParse`GeneralLibrary`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg, bindingAddedDynamicallyByGetOptionsPatternBinding}],
            Print[myArg]
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-K3SSGV"
]

Test[
    CalculateParse`GeneralLibrary`Private`addCaptureToDownValue[
        HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :>
            CalculateParse`GeneralLibrary`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg1, myArg2, myArg3}],
                1
            ]
    ]
    ,
    HoldPattern[myFunc[myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]]] :>
        CalculateParse`GeneralLibrary`CaptureFunctionCall[
            myFunc,
            HoldComplete[{myArg1, myArg2, myArg3}],
            1
        ]
    ,
    TestID -> "addCaptureToDownValue-20150203-Y9D5S4"
]