(* Tests for: CalculateParse`GeneralLibrary`DeCamelCase

   Author: danielb
    
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/DeCamelCase.mt"]
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
    CalculateParse`GeneralLibrary`DeCamelCase["JustTesting"]
    ,
    "Just Testing"
    ,
    TestID -> "DeCamelCase-20160119-R0QRYE"
]

Test[
    CalculateParse`GeneralLibrary`DeCamelCase["JustTestingAgain"]
    ,
    "Just Testing Again"
    ,
    TestID -> "DeCamelCase-20160119-5LL3A2"
]

Test[
    CalculateParse`GeneralLibrary`DeCamelCase["justTesting"]
    ,
    "Just Testing"
    ,
    TestID -> "DeCamelCase-20160119-Y8DVYE"
]

Test[
    CalculateParse`GeneralLibrary`DeCamelCase["notcamelcased"]
    ,
    "notcamelcased"
    ,
    TestID -> "DeCamelCase-20160119-2BBAG1"
]

Test[
    CalculateParse`GeneralLibrary`DeCamelCase["not camel cased"]
    ,
    "not camel cased"
    ,
    TestID -> "DeCamelCase-20160119-M6K4TR"
]