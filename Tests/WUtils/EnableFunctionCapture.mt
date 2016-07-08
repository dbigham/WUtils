(* Tests for: WUtils`WUtils`EnableFunctionCapture

   Author: danielb
*)

Test[
    (
        DownValues[myFunc] =
            {
                HoldPattern[myFunc[myArg]] :> Print[myArg],
                HoldPattern[myFunc[myArg1, myArg2]] :> Print[myArg1, ", ", myArg2]
            };
        WUtils`WUtils`EnableFunctionCapture[myFunc];
        DownValues[myFunc]
    )
    ,
    {
        HoldPattern[myFunc[myArg]] :>
            WUtils`WUtils`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg}],
                Print[myArg]
            ],
        HoldPattern[myFunc[myArg1, myArg2]] :>
            WUtils`WUtils`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg1, myArg2}],
                Print[myArg1, ", ", myArg2]
            ]
    }
    ,
    TestID -> "EnableFunctionCapture-20150203-4XZJRY"
]

Test[
    WUtils`WUtils`EnableFunctionCapture[funcSymbol]
    ,
    Null
    ,
    TestID -> "EnableFunctionCapture-20150203-B52M9C"
]