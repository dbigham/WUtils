(* Tests for: CalculateParse`GeneralLibrary`GetLineAtPos

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/GetLineAtPos.mt"]
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

(* First character of string *)
Test[
    CalculateParse`GeneralLibrary`GetLineAtPos["abc\ndef\nghi", 1]
    ,
    {1, 3}
    ,
    TestID -> "GetLineAtPos-20150221-FKHZPB"
]

Test[
    CalculateParse`GeneralLibrary`GetLineAtPos["abc\ndef\nghi", 6]
    ,
    {5, 7}
    ,
    TestID -> "GetLineAtPos-20150221-LG7LD9"
]

Test[
    CalculateParse`GeneralLibrary`GetLineAtPos["abc\ndef\nghi", 10]
    ,
    {9, 11}
    ,
    TestID -> "GetLineAtPos-20150221-YQQYAS"
]

(* Last character of string *)
Test[
    CalculateParse`GeneralLibrary`GetLineAtPos["abc\ndef\nghi", 11]
    ,
    {9, 11}
    ,
    TestID -> "GetLineAtPos-20150221-C9PHNZ"
]

(* If the current position is a newline, returns the previous line. *)
Test[
    CalculateParse`GeneralLibrary`GetLineAtPos["abc\ndef\nghi", 4]
    ,
    {1, 3}
    ,
    TestID -> "GetLineAtPos-20150221-08CJSM"
]

(* On the trailing newline of an empty line. *)
Test[
    CalculateParse`GeneralLibrary`GetLineAtPos["abc\n\ndef\nghi", 5]
    ,
    {6, 5}
    ,
    TestID -> "GetLineAtPos-20150221-Z3KZ2L"
]