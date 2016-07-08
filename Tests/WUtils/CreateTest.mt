(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`CreateTest

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/CreateTest.mt"]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`CreateTest[
        1 + 1,
        ToFileName[{$TemporaryDirectory}, "MyTestFile.m"],
        "TestId" -> "TESTID"
    ]
    ,
    "Test[\n    1 + 1\n    ,\n    2\n    ,\n    TestID -> \"TESTID\"\n]"
    ,
    TestID -> "CreateTest-20150225-VLNNAJ"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`CreateTest[
        Unevaluated[Sequence[]],
        ToFileName[{$TemporaryDirectory}, "MyTestFile.m"],
        "TestId" -> "TESTID"
    ]
    ,
    "Test[\n    Sequence[]\n    ,\n    Null\n    ,\n    TestID -> \"TESTID\"\n]"
    ,
    TestID -> "CreateTest-20150225-NB7SDR"
]

Test[
    CalculateParse`GeneralLibrary`CapturePrint[
        CalculateParse`Prototype`VirtualAssistant`Utility`CreateTest[
            1,
            "MyFile.m",
            "TestId" -> Missing[]
        ]
    ]
    ,
    {{"CreateTest: Invalid TestId: ", InputForm[Missing[]]}}
    ,
    TestID -> "CreateTest-20150304-LNBSRY"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`CreateTest[
        1 + 1,
        ToFileName[{$TemporaryDirectory}, "MyTestFile.m"],
        "TestId" -> "TESTID",
        "ExpectedMessages" -> {Hold[Power::infy]}
    ]
    ,
    "Test[\n    1 + 1\n    ,\n    2\n    ,\n    {Power::infy}\n    ,\n    TestID -> \"TESTID\"\n]"
    ,
    TestID -> "CreateTest-20160531-1GJV0B"
]