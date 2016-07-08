(* Tests for: CalculateParse`GeneralLibrary`GetSymbolContext

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
    CalculateParse`GeneralLibrary`GetSymbolContext["runRegexes"]
    ,
    "CalculateParse`Parser1`Private`"
    ,
    TestID -> "GetSymbolContext-20151223-J5HAHD"
]

Test[
    CalculateParse`GeneralLibrary`GetSymbolContext["GetSymbolContext"]
    ,
    "CalculateParse`GeneralLibrary`"
    ,
    TestID -> "GetSymbolContext-20151223-IEWEQA"
]

Test[
    Module[
        {},
        (
            myTestVar = 1;
            With[
                {tmp = CalculateParse`GeneralLibrary`GetSymbolContext["myTestVar"]},
                (
                    Remove[myTestVar];
                    tmp
                )
            ]
        )
    ]
    ,
    "Global`"
    ,
    TestID -> "GetSymbolContext-20151223-NWNA46"
]