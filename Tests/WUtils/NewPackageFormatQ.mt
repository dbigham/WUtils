(* Tests for: WUtils`WUtils`NewPackageFormatQ

   Author: danielb
*)

Test[
    WUtils`WUtils`NewPackageFormatQ[$TestingTools, "UseMemoization" -> False]
    ,
    True
    ,
    TestID -> "NewPackageFormatQ-20151202-H5SWEU"
]

Test[
    WUtils`WUtils`NewPackageFormatQ[
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
        WUtils`WUtils`NewPackageFormatQ[$TestingTools],
        WUtils`WUtils`NewPackageFormatQ[$TestingTools],
        Less[
            AbsoluteTiming[WUtils`WUtils`NewPackageFormatQ[$TestingTools]][[1]],
            0.001
        ]
    }
    ,
    {True, True, True}
    ,
    TestID -> "NewPackageFormatQ-20151202-58O71V"
]