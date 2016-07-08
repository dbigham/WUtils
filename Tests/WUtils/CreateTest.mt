(* Tests for: WUtils`WUtils`CreateTest

   Author: danielb
*)

Test[
    WUtils`WUtils`CreateTest[
        1 + 1,
        ToFileName[{$TemporaryDirectory}, "MyTestFile.m"],
        "TestId" -> "TESTID"
    ],
    "Test[\n\t1 + 1\n\t,\n\t2\n\t,\n\tTestID -> \"TESTID\"\n]",
    TestID -> "CreateTest-20150225-VLNNAJ"
]

Test[
    WUtils`WUtils`CreateTest[
        Unevaluated[Sequence[]],
        ToFileName[{$TemporaryDirectory}, "MyTestFile.m"],
        "TestId" -> "TESTID"
    ],
    "Test[\n\tSequence[]\n\t,\n\tNull\n\t,\n\tTestID -> \"TESTID\"\n]",
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
    ],
    "Test[\n\t1 + 1\n\t,\n\t2\n\t,\n\t{Power::infy}\n\t,\n\tTestID -> \"TESTID\"\n]",
    TestID -> "CreateTest-20160531-1GJV0B"
]