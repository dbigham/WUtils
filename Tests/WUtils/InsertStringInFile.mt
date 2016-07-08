(* Tests for: WUtils`WUtils`InsertStringInFile

   Author: danielb
*)

Test[
    With[
        {file = FileNameJoin[{$TemporaryDirectory, "InsertStringInFile.m"}]},
        (
            Export[file, "line\nINSERT AFTER\nline", "String"];
            WUtils`WUtils`InsertStringInFile[
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
            WUtils`WUtils`InsertStringInFile[
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