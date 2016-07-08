(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`MakeExpectedValueSystemIndependent

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
    CalculateParse`Prototype`VirtualAssistant`Utility`MakeExpectedValueSystemIndependent[
        "E:\\Users\\Daniel\\WolframWorkspaces\\Base2\\Alpha\\Source\\CalculateParse\\GeneralLibrary.m"
    ]
    ,
    HeldExpectedValue[FileNameJoin[{$AlphaRootDirectory, "Source", "CalculateParse", "GeneralLibrary.m"}]]
    ,
    TestID -> "MakeExpectedValueSystemIndependent-20150225-1AN0LX"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`MakeExpectedValueSystemIndependent[
        Sequence[]
    ]
    ,
    Sequence[]
    ,
    TestID -> "MakeExpectedValueSystemIndependent-20150225-BF7SHY"
]