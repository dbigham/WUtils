(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`removeCaptureFromDownValue

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
    CalculateParse`GeneralLibrary`Private`removeCaptureFromDownValue[
        HoldPattern[myFunc[myArg_]] :>
            CalculateParse`GeneralLibrary`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg}],
                myArg + 1
            ]
    ]
    ,
    HoldPattern[myFunc[myArg_]] :> myArg + 1
    ,
    TestID -> "removeCaptureFromDownValue-20150203-EYHSWT"
]