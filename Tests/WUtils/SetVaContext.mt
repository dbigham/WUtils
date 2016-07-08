(* Tests for: CalculateParse`Prototype`VirtualAssistant`VaActions`SetVaContext

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
    ReplaceAll[
        Block[
            {CalculateParse`Prototype`VirtualAssistant`VaSemantics`$VaContext = {}},
            CalculateParse`Prototype`VirtualAssistant`VaActions`SetVaContext[
                CalculateParse`Prototype`VirtualAssistant`VaSemantics`S["Notebook"][
                    "Name" -> "SetVaContext",
                    "Path" -> "..."
                ]
            ]
        ],
        timestamp:{_, _, _, _, __} :> "..."
    ]
    ,
    {
        CalculateParse`Prototype`VirtualAssistant`VaSemantics`S["ContextItem"][
            "Object" ->
                CalculateParse`Prototype`VirtualAssistant`VaSemantics`S["Notebook"][
                    "Name" -> "SetVaContext",
                    "Path" -> "..."
                ],
            "Timestamp" -> "..."
        ]
    }
    ,
    TestID -> "SetVaContext-20160102-VC9UUW"
]