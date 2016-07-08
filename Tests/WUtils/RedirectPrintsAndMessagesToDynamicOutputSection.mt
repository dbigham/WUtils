(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`RedirectPrintsAndMessagesToDynamicOutputSection

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
    With[
        {
            var =
                CalculateParse`Prototype`VirtualAssistant`Utility`DynamicOutputSectionVar[]
        },
        (
            CalculateParse`Prototype`VirtualAssistant`Utility`RedirectPrintsAndMessagesToDynamicOutputSection[
                Print["Hello", " ", "again"];,
                var
            ];
            ReleaseHold[var]
        )
    ]
    ,
    Row[{"Hello", " ", "again"}]
    ,
    TestID -> "RedirectPrintsAndMessagesToDynamicOutputSection-20150217-O477IT"
]

Test[
    With[
        {
            var =
                CalculateParse`Prototype`VirtualAssistant`Utility`DynamicOutputSectionVar[]
        },
        (
            CalculateParse`Prototype`VirtualAssistant`Utility`RedirectPrintsAndMessagesToDynamicOutputSection[
                1/0,
                var
            ];
            ReleaseHold[var]
        )
    ]
    ,
    Row[{StringForm["Infinite expression `1` encountered.", HoldForm[0^(-1)]], ""}]
    ,
    TestID -> "RedirectPrintsAndMessagesToDynamicOutputSection-20150217-JFV3QD"
]