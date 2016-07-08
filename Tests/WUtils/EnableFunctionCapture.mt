(* Tests for: CalculateParse`GeneralLibrary`EnableFunctionCapture

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/EnableFunctionCapture.mt"]
   ]
*)

TestExecute[$TestAbortTime = 600]

TestExecute[
    If[TrueQ[Quiet[Get["CalculateTestEnvironment.m"]]===$Failed],
        Get[
        StringCases[$CurrentFile,
        inputfile:(StartOfString~~___~~$PathnameSeparator~~"Tests"~~$PathnameSeparator)~~___
        :> inputfile<>"Utilities"<>$PathnameSeparator<>"CalculateTestEnvironment.m"][[1]]
        ]]
]

TestExecute[$CalculateDataPacletsInit = False;  << "CalculateLoader`"]

TestExecute[$TestAbortTime = $TestAbortTimeInitial]


Test[
    (
        DownValues[myFunc] =
            {
                HoldPattern[myFunc[myArg]] :> Print[myArg],
                HoldPattern[myFunc[myArg1, myArg2]] :> Print[myArg1, ", ", myArg2]
            };
        CalculateParse`GeneralLibrary`EnableFunctionCapture[myFunc];
        DownValues[myFunc]
    )
    ,
    {
        HoldPattern[myFunc[myArg]] :>
            CalculateParse`GeneralLibrary`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg}],
                Print[myArg]
            ],
        HoldPattern[myFunc[myArg1, myArg2]] :>
            CalculateParse`GeneralLibrary`CaptureFunctionCall[
                myFunc,
                HoldComplete[{myArg1, myArg2}],
                Print[myArg1, ", ", myArg2]
            ]
    }
    ,
    TestID -> "EnableFunctionCapture-20150203-4XZJRY"
]

Test[
    CalculateParse`GeneralLibrary`EnableFunctionCapture[funcSymbol]
    ,
    Null
    ,
    TestID -> "EnableFunctionCapture-20150203-B52M9C"
]