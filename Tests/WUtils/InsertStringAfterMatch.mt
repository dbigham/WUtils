(* Tests for: CalculateParse`GeneralLibrary`InsertStringAfterMatch

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/InsertStringAfterMatch.mt"]
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
    CalculateParse`GeneralLibrary`InsertStringAfterMatch["abc def ghi", " 123", "def"]
    ,
    "abc def 123 ghi"
    ,
    TestID -> "InsertStringAfterMatch-20160120-0JCPJ4"
]

Test[
    CalculateParse`GeneralLibrary`InsertStringAfterMatch["abc def ghi", " 123", "d"~~_~~"f"]
    ,
    "abc def 123 ghi"
    ,
    TestID -> "InsertStringAfterMatch-20160120-6BSQ91"
]