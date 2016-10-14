(* Tests for: CalculateParse`GeneralLibrary`SymbolToFile

   Ignored by QA because I don't think they have Alpha on the
   $Path, rendering FindFile useless.

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/WorkbenchPlugin/Tests/WorkbenchHelpers.mt"]
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
    Import["http://127.0.0.1:8193/go?command=test1", "String"]
    ,
    "OK"
]

Test[
    Import["http://127.0.0.1:8193/go?command=test2&file=/Alpha/Source/CalculateParse/JavaTokenizer.m", "String"]
    ,
    "OK"
]

Test[
    Import["http://127.0.0.1:8193/go?command=test3&file=/Alpha/Source/CalculateParse/JavaTokenizer.m", "String"]
    ,
    "OK"
]

Test[
    Import["http://127.0.0.1:8193/go?command=test4", "String"]
    ,
    "OK"
]

Test[
    Import["http://127.0.0.1:8193/go?command=test5", "String"]
    ,
    "OK"
]

Test[
    Import["http://127.0.0.1:8193/go?command=test6", "String"]
    ,
    "OK"
]
