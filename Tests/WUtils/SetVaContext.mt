(* Tests for: WUtils`WUtils`SetVaContext

   Author: danielb
*)

Test[
    ReplaceAll[
        Block[
            {CalculateParse`Prototype`VirtualAssistant`VaSemantics`$VaContext = {}},
            WUtils`WUtils`SetVaContext[
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