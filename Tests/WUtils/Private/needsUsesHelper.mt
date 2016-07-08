(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`needsUsesHelper

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/Private/needsUsesHelper.mt"]
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
    CalculateParse`GeneralLibrary`Private`needsUsesHelper[{HoldComplete[Needs["MyPackage`"]]}]
    ,
    {"MyPackage`"}
    ,
    TestID -> "needsUsesHelper-20150304-EI6173"
]

Test[
    CalculateParse`GeneralLibrary`Private`needsUsesHelper[
        {HoldComplete[Needs["MyPackage1`"]], HoldComplete[Needs["MyPackage2`"]]}
    ]
    ,
    {"MyPackage1`", "MyPackage2`"}
    ,
    TestID -> "needsUsesHelper-20150304-546X76"
]

Test[
    CalculateParse`GeneralLibrary`Private`needsUsesHelper[
        {
            HoldComplete[Needs["MyPackage1`"]],
            HoldComplete[Needs["MyPackage2`"]],
            HoldComplete[Needs["MyPackage2`"]]
        }
    ]
    ,
    {"MyPackage1`", "MyPackage2`"}
    ,
    TestID -> "needsUsesHelper-20150304-MFBNYN"
]

Test[
    CalculateParse`GeneralLibrary`Private`needsUsesHelper[{HoldComplete[1 + 1]}]
    ,
    {}
    ,
    TestID -> "needsUsesHelper-20150304-2YKCZ1"
]