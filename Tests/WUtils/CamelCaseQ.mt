(* Tests for: WUtils`WUtils`CamelCaseQ

   Author: danielb
*)

Test[
    WUtils`WUtils`CamelCaseQ["JustTesting"]
    ,
    True
    ,
    TestID -> "CamelCaseQ-20160119-1PEPMU"
]

Test[
    WUtils`WUtils`CamelCaseQ["Just Testing"]
    ,
    False
    ,
    TestID -> "CamelCaseQ-20160119-8MWSHW"
]

Test[
    WUtils`WUtils`CamelCaseQ["justtesting"]
    ,
    False
    ,
    TestID -> "CamelCaseQ-20160119-K08SQ6"
]

Test[
    WUtils`WUtils`CamelCaseQ["justTesting"]
    ,
    True
    ,
    TestID -> "CamelCaseQ-20160119-E87GI1"
]