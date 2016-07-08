(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`StringReplaceUsesOfSymbol

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/StringReplaceUsesOfSymbol.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`StringReplaceUsesOfSymbol[
        "MyContext`MySymbol = 1",
        "MyContext`",
        "MySymbol",
        "NewContext`",
        "NewSymbol"
    ]
    ,
    "NewContext`NewSymbol = 1"
    ,
    TestID -> "StringReplaceUsesOfSymbol-20160121-VBC4HW"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`StringReplaceUsesOfSymbol[
        "Options[MySymbol] = {}",
        "MyContext`",
        "MySymbol",
        "NewContext`",
        "NewSymbol"
    ]
    ,
    "Options[NewSymbol] = {}"
    ,
    TestID -> "StringReplaceUsesOfSymbol-20160121-KRPWIB"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`StringReplaceUsesOfSymbol[
        "MySymbol::blah = \"blah\"",
        "MyContext`",
        "MySymbol",
        "NewContext`",
        "NewSymbol"
    ]
    ,
    "NewSymbol::blah = \"blah\""
    ,
    TestID -> "StringReplaceUsesOfSymbol-20160121-MMBFMI"
]