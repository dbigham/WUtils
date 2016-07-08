(* Tests for: WUtils`WUtils`Private`removeCaptureFromDownValue

   Author: danielb
*)

Test[
    WUtils`WUtils`Private`removeCaptureFromDownValue[
        HoldPattern[myFunc[myArg_]] :>
            WUtils`WUtils`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg}],
                myArg + 1
            ]
    ]
    ,
    HoldPattern[myFunc[myArg_]] :> myArg + 1
    ,
    TestID -> "removeCaptureFromDownValue-20150203-EYHSWT"
]