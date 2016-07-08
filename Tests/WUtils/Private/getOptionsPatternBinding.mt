(* Tests for: WUtils`WUtils`Private`getOptionsPatternBinding

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`getOptionsPatternBinding[
        HoldPattern[myFunc[myArg, opts:OptionsPattern[]]] :> Print[myArg, ": ", {opts}]
    ]
    ,
    {
        HoldComplete[opts],
        HoldPattern[myFunc[myArg, opts:OptionsPattern[]]] :> Print[myArg, ": ", {opts}]
    }
    ,
    TestID -> "getOptionsPatternBinding-20150203-BAJ564"
]
