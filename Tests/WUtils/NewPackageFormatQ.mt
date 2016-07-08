(* Tests for: CalculateParse`GeneralLibrary`NewPackageFormatQ

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
    CalculateParse`GeneralLibrary`NewPackageFormatQ[$TestingTools, "UseMemoization" -> False]
    ,
    True
    ,
    TestID -> "NewPackageFormatQ-20151202-H5SWEU"
]

Test[
    CalculateParse`GeneralLibrary`NewPackageFormatQ[
        FindFile["Daniel`Tools`"],
        "UseMemoization" -> False
    ]
    ,
    False
    ,
    TestID -> "NewPackageFormatQ-20151202-YWE238"
]

Test[
    {
        CalculateParse`GeneralLibrary`NewPackageFormatQ[$TestingTools],
        CalculateParse`GeneralLibrary`NewPackageFormatQ[$TestingTools],
        Less[
            AbsoluteTiming[CalculateParse`GeneralLibrary`NewPackageFormatQ[$TestingTools]][[1]],
            0.001
        ]
    }
    ,
    {True, True, True}
    ,
    TestID -> "NewPackageFormatQ-20151202-58O71V"
]