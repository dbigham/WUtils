(* Tests for: WUtils`WUtils`AppendToFile

   Author: danielb
*)

Test[
    WUtils`WUtils`WithTemporaryFiles[
        {myFile = "abc\n\n\n\n"},
        (
            WUtils`WUtils`AppendToFile[myFile, "NEW"];
            Import[myFile, "Text"]
        )
    ]
    ,
    "abc\n\nNEW"
    ,
    TestID -> "AppendToFile-20151202-DY6O5I"
]

Test[
    WUtils`WUtils`WithTemporaryFiles[
        {myFile = "abc"},
        (
            WUtils`WUtils`AppendToFile[myFile, "NEW"];
            Import[myFile, "Text"]
        )
    ]
    ,
    "abc\n\nNEW"
    ,
    TestID -> "AppendToFile-20151202-N8DRMJ"
]

Test[
    WUtils`WUtils`WithTemporaryFiles[
        {myFile = ""},
        (
            WUtils`WUtils`AppendToFile[myFile, "NEW"];
            Import[myFile, "Text"]
        )
    ]
    ,
    "\n\nNEW"
    ,
    TestID -> "AppendToFile-20151202-1BO5Z4"
]