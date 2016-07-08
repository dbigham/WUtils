(* Tests for: WUtils`WUtils`StringReplaceInFiles

   Author: danielb
*)

Test[
    WUtils`WUtils`WithTemporaryFiles[
        {file1 = "apples and oranges"},
        (
            WUtils`WUtils`StringReplaceInFiles["oranges", "apples", {file1}];
            InputForm[Import[file1, "String"]]
        )
    ]
    ,
    InputForm["apples and apples"]
    ,
    TestID -> "StringReplaceInFiles-20160531-FY9NE0"
]

Test[
    WUtils`WUtils`WithTemporaryFiles[
        {file1 = "apples and oranges\n"},
        (
            WUtils`WUtils`StringReplaceInFiles["oranges", "apples", {file1}];
            InputForm[Import[file1, "String"]]
        )
    ]
    ,
    InputForm["apples and apples\r\n"]
    ,
    TestID -> "StringReplaceInFiles-20160531-E1B2KJ"
]

Test[
    WUtils`WUtils`WithTemporaryFiles[
        {file1 = "apples and oranges\n\n\n"},
        (
            WUtils`WUtils`StringReplaceInFiles["oranges", "apples", {file1}];
            InputForm[Import[file1, "String"]]
        )
    ]
    ,
    InputForm["apples and apples\r\n\r\n\r\n"]
    ,
    TestID -> "StringReplaceInFiles-20160531-2XGPSE"
]