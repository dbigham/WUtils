(* Tests for: WUtils`WUtils`ArgListToBindingList

   Author: danielb
*)

Test[
    WUtils`WUtils`ArgListToBindingList[
        HoldComplete[{myArg1_Integer, myArg2_String, myArg3:OptionsPattern[]}]
    ]
    ,
    HoldComplete[{myArg1, myArg2, myArg3}]
    ,
    TestID -> "ArgListToBindingList-20150203-SU9WVP"
]