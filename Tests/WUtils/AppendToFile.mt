(* Tests for: CalculateParse`GeneralLibrary`AppendToFile

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/AppendToFile.mt"]
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
        {myFile = "abc\n\n\n\n"},
        (
            CalculateParse`GeneralLibrary`AppendToFile[myFile, "NEW"];
            Import[myFile, "Text"]
        )
    ]
    ,
    "abc\n\nNEW"
    ,
    TestID -> "AppendToFile-20151202-DY6O5I"
]

Test[
    CalculateParse`GeneralLibrary`WithTemporaryFiles[
        {myFile = "abc"},
        (
            CalculateParse`GeneralLibrary`AppendToFile[myFile, "NEW"];
            Import[myFile, "Text"]
        )
    ]
    ,
    "abc\n\nNEW"
    ,
    TestID -> "AppendToFile-20151202-N8DRMJ"
]

Test[
    CalculateParse`GeneralLibrary`WithTemporaryFiles[
        {myFile = ""},
        (
            CalculateParse`GeneralLibrary`AppendToFile[myFile, "NEW"];
            Import[myFile, "Text"]
        )
    ]
    ,
    "\n\nNEW"
    ,
    TestID -> "AppendToFile-20151202-1BO5Z4"
]