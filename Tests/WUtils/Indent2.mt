(* Tests for: WUtils`WUtils`Indent2

   Author: danielb
*)

Test[
    WUtils`WUtils`Indent2[Hello`Hello[World`world]]
    ,
    "Hello[world]"
    ,
    TestID -> "Indent2-20150126-LUPN4N"
]

Test[
    WUtils`WUtils`Indent2[Blah["a", "b"], "AlwaysIndentToLevel" -> 0],
    "Blah[\n\t\"a\",\n\t\"b\"\n]",
    TestID -> "Indent2-20150130-BRV2XX"
]
Test[
    WUtils`WUtils`Indent2[
        "primary kernel" :> (CurrentValue[InputNotebook[], Evaluator] = "Local"),
        "AlwaysIndentToLevel" -> 0
    ],
    "\"primary kernel\" :>\n\t(\n\tCurrentValue[InputNotebook[], Evaluator] = \"Local\"\n\t)",
    TestID -> "Indent2-20150130-OT52A6"
]
Test[
    WUtils`WUtils`Indent2[
        "primary kernel" :> (CurrentValue[InputNotebook[], Evaluator] = "Local"),
        "AlwaysIndentToLevel" -> 1
    ],
    "\"primary kernel\" :>\n\t(\n\tCurrentValue[\n\t\tInputNotebook[],\n\t\tEvaluator\n\t] =\n\t\t\"Local\"\n\t)",
    TestID -> "Indent2-20150130-VRLNLK"
]
Test[
    WUtils`WUtils`Indent2[
        "primary kernel" :> (CurrentValue[InputNotebook[], Evaluator] = "Local")
    ]
    ,
    "\"primary kernel\" :> (CurrentValue[InputNotebook[], Evaluator] = \"Local\")"
    ,
    TestID -> "Indent2-20150130-HS9D9M"
]

Test[
    WUtils`WUtils`Indent2[
        FixedOrder[
            "email" | FixedOrder["send", OptionalElement["an"], "email"],
            OptionalElement["to"],
            CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`name:GrammarToken["Name"],
            OptionalElement["using"],
            OptionalElement[
                CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`app:GrammarToken["EmailApp"],
                Automatic
            ]
        ] :>
            HeldHead[WUtils`WUtils`ComposeEmail][
                With[
                    {
                        CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`email =
                            CalculateParse`Prototype`VirtualAssistant`SrData`GetPerson[
                                CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`name,
                                "EmailAddress"
                            ]
                    },
                    If[
                        CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`email =!= Missing[],
                        "To" -> CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`email
                        ,
                        "To" -> CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`name
                    ]
                ],
                If[
                    CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`app =!= Automatic,
                    "Invoke" -> CalculateParse`Prototype`VirtualAssistant`PliGrammar`Private`app
                    ,
                    Sequence @@ {}
                ]
            ]
    ],
    "FixedOrder[\n\t\"email\" | FixedOrder[\"send\", OptionalElement[\"an\"], \"email\"],\n\tOptionalElement[\"to\"],\n\tname:GrammarToken[\"Name\"],\n\tOptionalElement[\"using\"],\n\tOptionalElement[app:GrammarToken[\"EmailApp\"], Automatic]\n] :>\n\tHeldHead[ComposeEmail][\n\t\tWith[\n\t\t\t{email = GetPerson[name, \"EmailAddress\"]},\n\t\t\tIf[email =!= Missing[], \"To\" -> email, \"To\" -> name]\n\t\t],\n\t\tIf[app =!= Automatic, \"Invoke\" -> app, Sequence @@ {}]\n\t]",
    TestID -> "Indent2-20150130-1GBKP9"
]
Test[
    WUtils`WUtils`Indent2[
        CalculateParse`GrammarSyntax`FO["fix debugger", op["window" | "windows"]] :>
            (
                CurrentValue[$FrontEnd, {DebuggerSettings, "ToolsWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}};
                CurrentValue[$FrontEnd, {DebuggerSettings, "StackWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}}
            )
    ],
    "FO[\"fix debugger\", op[\"window\" | \"windows\"]] :>\n\t(\n\t\tCurrentValue[$FrontEnd, {DebuggerSettings, \"ToolsWindowMargins\"}] =\n\t\t\t{{Automatic, 0}, {Automatic, 0}};\n\t\tCurrentValue[$FrontEnd, {DebuggerSettings, \"StackWindowMargins\"}] =\n\t\t\t{{Automatic, 0}, {Automatic, 0}}\n\t)",
    TestID -> "Indent2-20150130-5D58DK"
]
(* Trailing semi-colon. *)
Test[
    WUtils`WUtils`Indent2[
        CalculateParse`GrammarSyntax`FO["fix debugger", op["window" | "windows"]] :>
            (
                CurrentValue[$FrontEnd, {DebuggerSettings, "ToolsWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}};
                CurrentValue[$FrontEnd, {DebuggerSettings, "StackWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}};
            )
    ],
    "FO[\"fix debugger\", op[\"window\" | \"windows\"]] :>\n\t(\n\t\tCurrentValue[$FrontEnd, {DebuggerSettings, \"ToolsWindowMargins\"}] =\n\t\t\t{{Automatic, 0}, {Automatic, 0}};\n\t\tCurrentValue[$FrontEnd, {DebuggerSettings, \"StackWindowMargins\"}] =\n\t\t\t{{Automatic, 0}, {Automatic, 0}};\n\t)",
    TestID -> "Indent2-20150130-9N33T2"
]
Test[
    WUtils`WUtils`Indent2[WUtils`WUtils`Private`FO["a", "b"], "AlwaysIndentToLevel" -> 1],
    "FO[\n\t\"a\",\n\t\"b\"\n]",
    TestID -> "Indent2-20150130-KT5KZ2"
]
Test[
    WUtils`WUtils`Indent2[{"->", "\[Rule]"}]
    ,
    "{\"->\", \"\[Rule]\"}"
    ,
    TestID -> "Indent2-20150131-CLCENE"
]

Test[
    WUtils`WUtils`Indent2[
        HoldComplete[
            $grammar = GrammarDeploy2[GrammarRules[{l1:GrammarToken["IPAddress"] :> l1}]]
        ],
        "AlwaysIndentToLevel" -> 10
    ],
    "HoldComplete[\n\t$grammar =\n\t\tGrammarDeploy2[\n\t\t\tGrammarRules[\n\t\t\t\t{\n\t\t\t\t\tl1:GrammarToken[\"IPAddress\"] :>\n\t\t\t\t\t\tl1\n\t\t\t\t}\n\t\t\t]\n\t\t]\n]",
    TestID -> "Indent2-20150206-MMUNAV"
]
Test[
    WUtils`WUtils`Indent2[HoldComplete[Foo[1, 2]; ], "AlwaysIndentToLevel" -> 10],
    "HoldComplete[\n\tFoo[\n\t\t1,\n\t\t2\n\t];\n]",
    TestID -> "Indent2-20150206-UB1G7P"
]
Test[
    WUtils`WUtils`Indent2[
        HoldComplete[Foo[1, 2]; Bar[1, 2]; ],
        "AlwaysIndentToLevel" -> 0,
        "RemoveHold" -> True
    ]
    ,
    "Foo[1, 2];\nBar[1, 2];"
    ,
    TestID -> "Indent2-20150206-7G2BA1"
]

Test[
    WUtils`WUtils`Indent2[
        HoldComplete[Foo[1, 2]; Bar[1, 2]; ],
        "RemoveHold" -> True
    ]
    ,
    "Foo[1, 2]; Bar[1, 2]; "
    ,
    TestID -> "Indent2-20150206-EY289L"
]

Test[
    WUtils`WUtils`Indent2[
        HoldComplete[
            (
                $grammarSource = GrammarRules[{l1:GrammarToken["IPAddress"] :> l1}];
                $grammar = CalculateParse`PLI`NonProductionCode`GrammarDeploy[$grammarSource];
            )
        ],
        "AlwaysIndentToLevel" -> 10
    ],
    "HoldComplete[\n\t(\n\t\t$grammarSource =\n\t\t\tGrammarRules[\n\t\t\t\t{\n\t\t\t\t\tl1:GrammarToken[\"IPAddress\"] :>\n\t\t\t\t\t\tl1\n\t\t\t\t}\n\t\t\t];\n\t\t$grammar =\n\t\t\tGrammarDeploy[\n\t\t\t\t$grammarSource\n\t\t\t];\n\t)\n]",
    TestID -> "Indent2-20150206-UKILHK"
]
(* Tests Row-box specific functionality in Indent2. By default, RowBox would have
   been turned into an unwanted InputForm. *)
Test[
    WUtils`WUtils`Indent2[RowBox[{"a", "b"}]]
    ,
    "RowBox[{\"a\", \"b\"}]"
    ,
    TestID -> "Indent2-20150224-31QBX4"
]

Test[
    WUtils`WUtils`Indent2["<<RowBox>>"[{"a", "b"}]]
    ,
    "\"<<RowBox>>\"[{\"a\", \"b\"}]"
    ,
    TestID -> "Indent2-20150225-PF6812"
]

Test[
    WUtils`WUtils`Indent2[RowBox[{RowBox[{"1", "2", "3"}]}]]
    ,
    "RowBox[{RowBox[{\"1\", \"2\", \"3\"}]}]"
    ,
    TestID -> "Indent2-20150226-8OOBHC"
]

(* TODO: We don't want this to have '---' (etc) in its output. As l-kernel? *)
(*
Test[
    WUtils`WUtils`Indent2["USDollars"/"Kilograms", "FullFormStrings" -> True]
    ,
    "\"USDollars\"/\"Kilograms\""
    ,
    TestID -> "Indent2-20150929-IJ1ED2"
]
*)

Test[
    WUtils`WUtils`Indent2[{"->", "\[Rule]"}, "FullFormStrings" -> True]
    ,
    "{\"->\", \"\\[Rule]\"}"
    ,
    TestID -> "Indent2-20150929-J74Q91"
]

(* This shouldn't be handled (as a whole) by the FullFormStrings logic, because it's not a string. Regression test. *)
Test[
    WUtils`WUtils`Indent2[
        HoldComplete[
            WUtils`WUtils`DivideHack["USDollars", "Kilograms"]
        ],
        "FullFormStrings" -> True
    ]
    ,
    "HoldComplete[DivideHack[\"USDollars\", \"Kilograms\"]]"
    ,
    TestID -> "Indent2-20151009-KBD361"
]

Test[
    WUtils`WUtils`Indent2[
        {
            {
                <|
                    "SequenceSize" -> 1,
                    "Counts" -> {{} -> <|{1} -> 2, {2} -> 3, {3} -> 2, {4} -> 2|>}
                |>
            },
            {
                <|
                    "SequenceSize" -> 2,
                    "Counts" ->
                    {
                        {1} -> <|{2} -> 2|>,
                        {2} -> <|{3} -> 2, {4} -> 1|>,
                        {3} -> <|{1} -> 1, {4} -> 1|>,
                        {4} -> <|{2} -> 1|>
                    }
                |>
            }
        }
    ],
    "{\n\t{<|\"SequenceSize\" -> 1, \"Counts\" -> {{} -> <|{1} -> 2, {2} -> 3, {3} -> 2, {4} -> 2|>}|>},\n\t{\n\t\t<|\n\t\t\t\"SequenceSize\" -> 2,\n\t\t\t\"Counts\" ->\n\t\t\t{\n\t\t\t\t{1} -> <|{2} -> 2|>,\n\t\t\t\t{2} -> <|{3} -> 2, {4} -> 1|>,\n\t\t\t\t{3} -> <|{1} -> 1, {4} -> 1|>,\n\t\t\t\t{4} -> <|{2} -> 1|>\n\t\t\t}\n\t\t|>\n\t}\n}",
    TestID -> "Indent2-20151009-4B30NM"
]