(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`getOptionsPatternBinding

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/Private/getOptionsPatternBinding.mt"]
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
    CalculateParse`GeneralLibrary`Private`getOptionsPatternBinding[
        HoldPattern[myFunc[myArg, opts:OptionsPattern[]]] :> Print[myArg, ": ", {opts}]
    ]
    ,
    {
        HoldComplete[opts],
        HoldPattern[myFunc[myArg, opts:OptionsPattern[]]] :> Print[myArg, ": ", {opts}]
    }
    ,
    TestID -> "getOptionsPatternBinding-20150203-BAJ564"
]
