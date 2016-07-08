(* Tests for: WUtils`WUtils`CreateTest

   Author: danielb
*)

Test[
    WUtils`WUtils`CreateTest[
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
    WUtils`WUtils`CreateTest[
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
    WUtils`WUtils`CapturePrint[
        WUtils`WUtils`CreateTest[
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
    WUtils`WUtils`CreateTest[
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