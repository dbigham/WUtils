(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`EnsureDefined

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/EnsureDefined.mt"]
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
    CalculateParse`GeneralLibrary`CapturePrint[
        CalculateParse`Prototype`VirtualAssistant`Utility`EnsureDefined[
            myUndefinedVar,
            "Description of variable goes here."
        ]
    ]
    ,
    {
        {
            "The variable ",
            "Global`myUndefinedVar",
            " must be defined. ",
            "Description of variable goes here."
        }
    }
    ,
    TestID -> "EnsureDefined-20150226-IE319E"
]

Test[
    Block[
        {Print = Null},
        CalculateParse`Prototype`VirtualAssistant`Utility`EnsureDefined[
            myUndefinedVar,
            "Description of variable goes here."
        ]
    ]
    ,
    $Failed
    ,
    TestID -> "EnsureDefined-20150226-8W49F8"
]