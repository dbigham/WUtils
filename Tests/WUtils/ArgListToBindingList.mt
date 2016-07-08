(* Tests for: CalculateParse`GeneralLibrary`ArgListToBindingList

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/ArgListToBindingList.mt"]
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
    CalculateParse`GeneralLibrary`ArgListToBindingList[
        HoldComplete[{myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]}]
    ]
    ,
    HoldComplete[{myArg1, myArg2, myArg3}]
    ,
    TestID -> "ArgListToBindingList-20150203-SU9WVP"
]