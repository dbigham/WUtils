(* Tests for: WUtils`WUtils`TabsOrSpaces

   Author: danielb
*)

Test[
    WUtils`WUtils`TabsOrSpaces["WUtils`WUtils`"]
    ,
    "Spaces"
    ,
    TestID -> "TabsOrSpaces-20160120-SVTBSL"
]

(*
Disabled because running the GeneralLibrary unit tests won't have MachineLearning loaded,
at least not the version that has $useTabsOrSpaces defined. (yet)
Test[
    WUtils`WUtils`TabsOrSpaces["MachineLearning`"]
    ,
    "Tabs"
    ,
    TestID -> "TabsOrSpaces-20160120-HGT64S"
]
*)