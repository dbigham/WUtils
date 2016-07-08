(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`FunctionWithNoArgs

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
    CalculateParse`Prototype`VirtualAssistant`Utility`FunctionWithNoArgs["myFunc[]"]
    ,
    True
    ,
    TestID -> "FunctionWithNoArgs-20150225-JXQTIM"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`FunctionWithNoArgs["myFunc[arg]"]
    ,
    False
    ,
    TestID -> "FunctionWithNoArgs-20150225-OMDUNB"
]