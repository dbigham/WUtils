(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`EvaluateAndGetMessages

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["AlphaSource/CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/EvaluateAndGetMessages.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`EvaluateAndGetMessages[1/0]
    ,
    {ComplexInfinity, {Hold[Power::infy]}}
    ,
    Power::infy
    ,
    TestID -> "EvaluateAndGetMessages-20160613-RXS6UA"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`EvaluateAndGetMessages[1 + 1]
    ,
    {2, {}}
    ,
    TestID -> "EvaluateAndGetMessages-20160613-WR8TKI"
]