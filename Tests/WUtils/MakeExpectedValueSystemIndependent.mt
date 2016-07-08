(* Tests for: WUtils`WUtils`MakeExpectedValueSystemIndependent

   Author: danielb
*)

Test[
    WUtils`WUtils`MakeExpectedValueSystemIndependent[
        "E:\\Users\\Daniel\\WolframWorkspaces\\Base2\\Alpha\\Source\\CalculateParse\\GeneralLibrary.m"
    ]
    ,
    HeldExpectedValue[FileNameJoin[{$AlphaRootDirectory, "Source", "CalculateParse", "GeneralLibrary.m"}]]
    ,
    TestID -> "MakeExpectedValueSystemIndependent-20150225-1AN0LX"
]

Test[
    WUtils`WUtils`MakeExpectedValueSystemIndependent[
        Sequence[]
    ]
    ,
    Sequence[]
    ,
    TestID -> "MakeExpectedValueSystemIndependent-20150225-BF7SHY"
]