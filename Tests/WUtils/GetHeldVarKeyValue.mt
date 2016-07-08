(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`GetHeldVarKeyValue

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/GetHeldVarKeyValue.mt"]
   ]
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
            CalculateParse`Prototype`VirtualAssistant`Utility`GetHeldVarKeyValue[heldVar, "MyKey"]
        )
    ]
    ,
    "MyValue"
    ,
    TestID -> "GetHeldVarKeyValue-20150221-637SCA"
]