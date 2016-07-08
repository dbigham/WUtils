(* Tests for: WUtils`WUtils`GetNearestPrecedingSpan

   Author: danielb
*)

Test[
    WUtils`WUtils`GetNearestPrecedingSpan[
        {10, 20},
        {{1, 3}, {4, 8}, {12, 14}, {23, 25}, {33, 55}}
    ]
    ,
    {4, 8}
    ,
    TestID -> "GetNearestPrecedingSpan-20160527-IZ2RPP"
]