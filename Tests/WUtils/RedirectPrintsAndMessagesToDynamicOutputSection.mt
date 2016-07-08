(* Tests for: WUtils`WUtils`RedirectPrintsAndMessagesToDynamicOutputSection

   Author: danielb
*)

Test[
    With[
        {
            var =
                WUtils`WUtils`DynamicOutputSectionVar[]
        },
        (
            WUtils`WUtils`RedirectPrintsAndMessagesToDynamicOutputSection[
                Print["Hello", " ", "again"];,
                var
            ];
            ReleaseHold[var]
        )
    ]
    ,
    Row[{"Hello", " ", "again"}]
    ,
    TestID -> "RedirectPrintsAndMessagesToDynamicOutputSection-20150217-O477IT"
]

Test[
    With[
        {
            var =
                WUtils`WUtils`DynamicOutputSectionVar[]
        },
        (
            WUtils`WUtils`RedirectPrintsAndMessagesToDynamicOutputSection[
                1/0,
                var
            ];
            ReleaseHold[var]
        )
    ]
    ,
    Row[{StringForm["Infinite expression `1` encountered.", HoldForm[0^(-1)]], ""}]
    ,
    TestID -> "RedirectPrintsAndMessagesToDynamicOutputSection-20150217-JFV3QD"
]