(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`StartsWithComment

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
    CalculateParse`Prototype`VirtualAssistant`Utility`StartsWithComment[
        "(* Starts with comment. *)\nMyFunc[];"
    ]
    ,
    True
    ,
    TestID -> "StartsWithComment-20150225-6UEHQG"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`StartsWithComment["MyFunc[];"]
    ,
    False
    ,
    TestID -> "StartsWithComment-20150225-Y4NJRG"
]