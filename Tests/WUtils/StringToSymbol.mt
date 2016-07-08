(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`StringToSymbol

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
    Block[
        {$ContextPath = {"System`"}},
        CalculateParse`Prototype`VirtualAssistant`Utility`StringToSymbol["toSingleLine"]
    ]
    ,
    $Failed
    ,
    TestID -> "StringToSymbol-20150131-E1O94G"
]

Test[
    Block[
        {$ContextPath = {"CalculateParse`GeneralLibrary`", "System`"}},
        CalculateParse`Prototype`VirtualAssistant`Utility`StringToSymbol["toSingleLine"]
    ]
    ,
    CalculateParse`GeneralLibrary`Private`toSingleLine
    ,
    TestID -> "StringToSymbol-20150131-09D87A"
]

Test[
    Block[
        {$ContextPath = {"CalculateParse`Prototype`VirtualAssistant`VaActions`", "System`"}},
        CalculateParse`Prototype`VirtualAssistant`Utility`StringToSymbol["CreateIssueNotebook"]
    ]
    ,
    CalculateParse`Prototype`VirtualAssistant`VaActions`CreateIssueNotebook
    ,
    TestID -> "StringToSymbol-20150131-7VZKZT"
]