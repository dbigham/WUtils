(* Tests for: CalculateParse`GeneralLibrary`TabsOrSpaces

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
    CalculateParse`GeneralLibrary`TabsOrSpaces["CalculateParse`GeneralLibrary`"]
    ,
    "Spaces"
    ,
    TestID -> "TabsOrSpaces-20160120-SVTBSL"
]

(*
Disabled because running the GeneralLibrary unit tests won't have MachineLearning loaded,
at least not the version that has $useTabsOrSpaces defined. (yet)
Test[
    CalculateParse`GeneralLibrary`TabsOrSpaces["MachineLearning`"]
    ,
    "Tabs"
    ,
    TestID -> "TabsOrSpaces-20160120-HGT64S"
]
*)