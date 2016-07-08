(* Tests for: CalculateParse`GeneralLibrary`StringReplaceInFiles

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["UnitTests/CalculateParse/GeneralLibrary/StringReplaceInFiles.mt"]
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
    CalculateParse`GeneralLibrary`WithTemporaryFiles[
        {file1 = "apples and oranges"},
        (
            CalculateParse`GeneralLibrary`StringReplaceInFiles["oranges", "apples", {file1}];
            InputForm[Import[file1, "String"]]
        )
    ]
    ,
    InputForm["apples and apples"]
    ,
    TestID -> "StringReplaceInFiles-20160531-FY9NE0"
]

Test[
    CalculateParse`GeneralLibrary`WithTemporaryFiles[
        {file1 = "apples and oranges\n"},
        (
            CalculateParse`GeneralLibrary`StringReplaceInFiles["oranges", "apples", {file1}];
            InputForm[Import[file1, "String"]]
        )
    ]
    ,
    InputForm["apples and apples\r\n"]
    ,
    TestID -> "StringReplaceInFiles-20160531-E1B2KJ"
]

Test[
    CalculateParse`GeneralLibrary`WithTemporaryFiles[
        {file1 = "apples and oranges\n\n\n"},
        (
            CalculateParse`GeneralLibrary`StringReplaceInFiles["oranges", "apples", {file1}];
            InputForm[Import[file1, "String"]]
        )
    ]
    ,
    InputForm["apples and apples\r\n\r\n\r\n"]
    ,
    TestID -> "StringReplaceInFiles-20160531-2XGPSE"
]