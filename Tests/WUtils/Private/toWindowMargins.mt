(* Tests for: CalculateParse`Prototype`VirtualAssistant`VaActions`Private`CalculateParse`Prototype`VirtualAssistant`VaActions`Private`toWindowMargins

   Author: danielb
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
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`toWindowMargins[0.5, 1920]
    ,
    {Automatic, 0.}
    ,
    TestID -> "toWindowMargins-20150226-QNG4W7"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`toWindowMargins[0, 1920]
    ,
    {0, Automatic}
    ,
    TestID -> "toWindowMargins-20150226-IQGGOE"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`toWindowMargins[1, 1920]
    ,
    {1920, Automatic}
    ,
    TestID -> "toWindowMargins-20150226-JR92U3"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`VaActions`Private`toWindowMargins[1.5, 1920]
    ,
    {Automatic, -1920.}
    ,
    TestID -> "toWindowMargins-20150226-O4AF12"
]