(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`SetHeldVarKeyValue

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
    Module[
        {heldVar = CalculateParse`GeneralLibrary`NewHeldVar["testVar"]},
        (
            CalculateParse`Prototype`VirtualAssistant`Utility`SetHeldVarKeyValue[
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
        {heldVar = CalculateParse`GeneralLibrary`NewHeldVar["testVar"]},
        (
            CalculateParse`Prototype`VirtualAssistant`Utility`SetHeldVarKeyValue[
                heldVar,
                "MyKey1" -> "MyValue1"
            ];
            CalculateParse`Prototype`VirtualAssistant`Utility`SetHeldVarKeyValue[
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
        {heldVar = CalculateParse`GeneralLibrary`NewHeldVar["testVar"]},
        (
            CalculateParse`Prototype`VirtualAssistant`Utility`SetHeldVarKeyValue[
                heldVar,
                "MyKey1" -> "MyValue1"
            ];
            CalculateParse`Prototype`VirtualAssistant`Utility`SetHeldVarKeyValue[
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
            CalculateParse`GeneralLibrary`NewHeldVar[
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