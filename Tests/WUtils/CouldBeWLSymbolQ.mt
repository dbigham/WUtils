(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`CouldBeWLSymbolQ

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/CouldBeWLSymbolQ.mt"]
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
    CouldBeWLSymbolQ["1"]
    ,
    False
    ,
    TestID -> "CouldBeWLSymbolQ-20150126-G7DUIN"
]

Test[
    CouldBeWLSymbolQ["Test"]
    ,
    True
    ,
    TestID -> "CouldBeWLSymbolQ-20150126-5BNT76"
]

Test[
    CouldBeWLSymbolQ["Test1"]
    ,
    True
    ,
    TestID -> "CouldBeWLSymbolQ-20150126-SCJXNH"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`CouldBeWLSymbolQ["#"]
    ,
    False
    ,
    TestID -> "CouldBeWLSymbolQ-20150128-3KCU4Q"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`CouldBeWLSymbolQ["1"]
    ,
    False
    ,
    TestID -> "CouldBeWLSymbolQ-20150128-XM9M4Z"
]