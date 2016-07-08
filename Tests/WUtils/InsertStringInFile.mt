(* Tests for: CalculateParse`GeneralLibrary`InsertStringInFile

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
    With[
        {file = FileNameJoin[{$TemporaryDirectory, "InsertStringInFile.m"}]},
        (
            Export[file, "line\nINSERT AFTER\nline", "String"];
            CalculateParse`GeneralLibrary`InsertStringInFile[
                file,
                "\ninserted string",
                "INSERT AFTER",
                "AfterMatch" -> True
            ];
            With[{res = Import[file, "String"] // StringReplace[#, "\r" :> ""] &}, DeleteFile[file]; res]
        )
    ]
    ,
    "line\nINSERT AFTER\ninserted string\nline"
    ,
    TestID -> "InsertStringInFile-20150204-KGEVA2"
]

Test[
    With[
        {file = FileNameJoin[{$TemporaryDirectory, "InsertStringInFile.m"}]},
        (
            Export[file, "line\nINSERT BEFORE\nline", "String"];
            CalculateParse`GeneralLibrary`InsertStringInFile[
                file,
                "inserted string\n",
                "INSERT BEFORE",
                "AfterMatch" -> False
            ];
            With[{res = Import[file, "String"] // StringReplace[#, "\r" :> ""] &}, DeleteFile[file]; res]
        )
    ]
    ,
    "line\ninserted string\nINSERT BEFORE\nline"
    ,
    TestID -> "InsertStringInFile-20150204-JVQ5ZT"
]