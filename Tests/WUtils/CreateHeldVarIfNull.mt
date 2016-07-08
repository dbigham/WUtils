(* Tests for: CalculateParse`GeneralLibrary`CreateHeldVarIfNull

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
    StringMatchQ[
        ToString[CalculateParse`GeneralLibrary`CreateHeldVarIfNull[Null]],
        StringExpression[
            "HoldComplete[CalculateParse`GeneralLibrary`Private`NewVar`heldVar",
            Repeated[DigitCharacter],
            "]"
        ]
    ]
    ,
    True
    ,
    TestID -> "CreateHeldVarIfNull-20150304-ELVD9H"
]

Test[
    CalculateParse`GeneralLibrary`CreateHeldVarIfNull[
        HoldComplete[CalculateParse`GeneralLibrary`Private`NewVar`heldVar2]
    ]
    ,
    HoldComplete[CalculateParse`GeneralLibrary`Private`NewVar`heldVar2]
    ,
    TestID -> "CreateHeldVarIfNull-20150304-OXWU30"
]

Test[
    ReleaseHold[CalculateParse`GeneralLibrary`CreateHeldVarIfNull[Null, {}]]
    ,
    {}
    ,
    TestID -> "CreateHeldVarIfNull-20150304-XS7AKO"
]