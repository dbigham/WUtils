(* Tests for: WUtils`WUtils`GetNearestTrailingSpan

   Author: danielb
*)

Test[
    WUtils`WUtils`GetNearestTrailingSpan[
        {10, 20},
        {{1, 3}, {4, 8}, {12, 14}, {23, 25}, {33, 55}}
    ]
    ,
    {23, 25}
    ,
    TestID -> "GetNearestTrailingSpan-20160527-MARTAH"
]