(* Tests for: CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/RemoveHoldFromIndentedString.mt"]
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
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString["    HoldComplete[\"here\"]", "HoldComplete"]
    ,
    "    \"here\""
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-0BNL7R"
]

Test[
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString["HoldComplete[\"here\"]", "HoldComplete"]
    ,
    "\"here\""
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-ZW3HTS"
]

Test[
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString[
        "    HoldComplete[\n    Inner[\n        1,\n        2\n    ]\n    ]",
        "HoldComplete"
    ]
    ,
    "    Inner[\n        1,\n        2\n    ]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-D2DEVY"
]

Test[
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString[
        "HoldComplete[\n    Inner[\n        1,\n        2\n    ]\n]",
        "HoldComplete"
    ]
    ,
    "Inner[\n    1,\n    2\n]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-BU8VRL"
]

Test[
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString[
        "HoldComplete[\n\tLooksLikeCallSignature[CouldBeWLSymbolQ[\"test\"]], CouldBeWLSymbolQ]\n]",
        "HoldComplete"
    ]
    ,
    "LooksLikeCallSignature[CouldBeWLSymbolQ[\"test\"]], CouldBeWLSymbolQ]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-42MA28"
]

Test[
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString[
        "pliChartHoldComplete$586145[\n    Inner[\n        1,\n        2\n    ]\n]",
        "pliChartHoldComplete$586145"
    ]
    ,
    "Inner[\n    1,\n    2\n]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-1OX9HS"
]

(* The HoldComplete string didn't match, so nothing was done.
   Alternatively, we could perhaps return $Failed in this situation. *)
Test[
    CalculateParse`GeneralLibrary`RemoveHoldFromIndentedString[
        "    HoldComplete[1]",
        "HoldComplete2"
    ]
    ,
    "    HoldComplete[1]"
    ,
    TestID -> "RemoveHoldFromIndentedString-20150126-WD01ND"
]