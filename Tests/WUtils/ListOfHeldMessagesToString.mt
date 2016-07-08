(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`ListOfHeldMessagesToString

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["AlphaSource/CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/ListOfHeldMessagesToString.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`ListOfHeldMessagesToString[
        {Hold[Power::infy], Hold[Power::infy]}
    ]
    ,
    "{Power::infy, Power::infy}"
    ,
    TestID -> "ListOfHeldMessagesToString-20160610-S7BEJE"
]