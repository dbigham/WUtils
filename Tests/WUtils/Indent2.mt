(* Tests for: CalculateParse`GeneralLibrary`Indent2

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
    CalculateParse`GeneralLibrary`Indent2[Hello`Hello[World`world]]
    ,
    "Hello[world]"
    ,
    TestID -> "Indent2-20150126-LUPN4N"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[Blah["a", "b"], "AlwaysIndentToLevel" -> 0]
    ,
    "Blah[\n    \"a\",\n    \"b\"\n]"
    ,
    TestID -> "Indent2-20150130-BRV2XX"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        "primary kernel" :> (CurrentValue[InputNotebook[], Evaluator] = "Local"),
        "AlwaysIndentToLevel" -> 0
    ]
    ,
    "\"primary kernel\" :>\n    (\n    CurrentValue[InputNotebook[], Evaluator] = \"Local\"\n    )"
    ,
    TestID -> "Indent2-20150130-OT52A6"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        "primary kernel" :> (CurrentValue[InputNotebook[], Evaluator] = "Local"),
        "AlwaysIndentToLevel" -> 1
    ]
    ,
    "\"primary kernel\" :>\n    (\n    CurrentValue[\n        InputNotebook[],\n        Evaluator\n    ] =\n        \"Local\"\n    )"
    ,
    TestID -> "Indent2-20150130-VRLNLK"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        "primary kernel" :> (CurrentValue[InputNotebook[], Evaluator] = "Local")
    ]
    ,
    "\"primary kernel\" :> (CurrentValue[InputNotebook[], Evaluator] = \"Local\")"
    ,
    TestID -> "Indent2-20150130-HS9D9M"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
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
            HeldHead[CalculateParse`Prototype`VirtualAssistant`VaActions`ComposeEmail][
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
    ]
    ,
    "FixedOrder[\n    \"email\" | FixedOrder[\"send\", OptionalElement[\"an\"], \"email\"],\n    OptionalElement[\"to\"],\n    name:GrammarToken[\"Name\"],\n    OptionalElement[\"using\"],\n    OptionalElement[app:GrammarToken[\"EmailApp\"], Automatic]\n] :>\n    HeldHead[ComposeEmail][\n        With[\n            {email = GetPerson[name, \"EmailAddress\"]},\n            If[email =!= Missing[], \"To\" -> email, \"To\" -> name]\n        ],\n        If[app =!= Automatic, \"Invoke\" -> app, Sequence @@ {}]\n    ]"
    ,
    TestID -> "Indent2-20150130-1GBKP9"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        CalculateParse`GrammarSyntax`FO["fix debugger", op["window" | "windows"]] :>
            (
                CurrentValue[$FrontEnd, {DebuggerSettings, "ToolsWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}};
                CurrentValue[$FrontEnd, {DebuggerSettings, "StackWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}}
            )
    ]
    ,
    "FO[\"fix debugger\", op[\"window\" | \"windows\"]] :>\n    (\n        CurrentValue[$FrontEnd, {DebuggerSettings, \"ToolsWindowMargins\"}] =\n            {{Automatic, 0}, {Automatic, 0}};\n        CurrentValue[$FrontEnd, {DebuggerSettings, \"StackWindowMargins\"}] =\n            {{Automatic, 0}, {Automatic, 0}}\n    )"
    ,
    TestID -> "Indent2-20150130-5D58DK"
]

(* Trailing semi-colon. *)
Test[
    CalculateParse`GeneralLibrary`Indent2[
        CalculateParse`GrammarSyntax`FO["fix debugger", op["window" | "windows"]] :>
            (
                CurrentValue[$FrontEnd, {DebuggerSettings, "ToolsWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}};
                CurrentValue[$FrontEnd, {DebuggerSettings, "StackWindowMargins"}] =
                    {{Automatic, 0}, {Automatic, 0}};
            )
    ]
    ,
    "FO[\"fix debugger\", op[\"window\" | \"windows\"]] :>\n    (\n        CurrentValue[$FrontEnd, {DebuggerSettings, \"ToolsWindowMargins\"}] =\n            {{Automatic, 0}, {Automatic, 0}};\n        CurrentValue[$FrontEnd, {DebuggerSettings, \"StackWindowMargins\"}] =\n            {{Automatic, 0}, {Automatic, 0}};\n    )"
    ,
    TestID -> "Indent2-20150130-9N33T2"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        CalculateParse`Prototype`VirtualAssistant`VaActions`Private`FO["a", "b"],
        "AlwaysIndentToLevel" -> 1
    ]
    ,
    "FO[\n    \"a\",\n    \"b\"\n]"
    ,
    TestID -> "Indent2-20150130-KT5KZ2"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[{"->", "\[Rule]"}]
    ,
    "{\"->\", \"\[Rule]\"}"
    ,
    TestID -> "Indent2-20150131-CLCENE"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        HoldComplete[
            $grammar = GrammarDeploy2[GrammarRules[{l1:GrammarToken["IPAddress"] :> l1}]]
        ],
        "AlwaysIndentToLevel" -> 10
    ]
    ,
    "HoldComplete[\n    $grammar =\n        GrammarDeploy2[\n            GrammarRules[\n                {\n                    l1:GrammarToken[\"IPAddress\"] :>\n                        l1\n                }\n            ]\n        ]\n]"
    ,
    TestID -> "Indent2-20150206-MMUNAV"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        HoldComplete[Foo[1, 2]; ],
        "AlwaysIndentToLevel" -> 10
    ]
    ,
    "HoldComplete[\n    Foo[\n        1,\n        2\n    ];\n]"
    ,
    TestID -> "Indent2-20150206-UB1G7P"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
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
    CalculateParse`GeneralLibrary`Indent2[
        HoldComplete[Foo[1, 2]; Bar[1, 2]; ],
        "RemoveHold" -> True
    ]
    ,
    "Foo[1, 2]; Bar[1, 2]; "
    ,
    TestID -> "Indent2-20150206-EY289L"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
        HoldComplete[
            (
                $grammarSource = GrammarRules[{l1:GrammarToken["IPAddress"] :> l1}];
                $grammar = CalculateParse`PLI`NonProductionCode`GrammarDeploy[$grammarSource];
            )
        ],
        "AlwaysIndentToLevel" -> 10
    ]
    ,
    "HoldComplete[\n    (\n        $grammarSource =\n            GrammarRules[\n                {\n                    l1:GrammarToken[\"IPAddress\"] :>\n                        l1\n                }\n            ];\n        $grammar =\n            GrammarDeploy[\n                $grammarSource\n            ];\n    )\n]"
    ,
    TestID -> "Indent2-20150206-UKILHK"
]

(* Tests Row-box specific functionality in Indent2. By default, RowBox would have
   been turned into an unwanted InputForm. *)
Test[
    CalculateParse`GeneralLibrary`Indent2[RowBox[{"a", "b"}]]
    ,
    "RowBox[{\"a\", \"b\"}]"
    ,
    TestID -> "Indent2-20150224-31QBX4"
]

Test[
    CalculateParse`GeneralLibrary`Indent2["<<RowBox>>"[{"a", "b"}]]
    ,
    "\"<<RowBox>>\"[{\"a\", \"b\"}]"
    ,
    TestID -> "Indent2-20150225-PF6812"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[RowBox[{RowBox[{"1", "2", "3"}]}]]
    ,
    "RowBox[{RowBox[{\"1\", \"2\", \"3\"}]}]"
    ,
    TestID -> "Indent2-20150226-8OOBHC"
]

(* TODO: We don't want this to have '---' (etc) in its output. As l-kernel? *)
(*
Test[
    CalculateParse`GeneralLibrary`Indent2["USDollars"/"Kilograms", "FullFormStrings" -> True]
    ,
    "\"USDollars\"/\"Kilograms\""
    ,
    TestID -> "Indent2-20150929-IJ1ED2"
]
*)

Test[
    CalculateParse`GeneralLibrary`Indent2[{"->", "\[Rule]"}, "FullFormStrings" -> True]
    ,
    "{\"->\", \"\\[Rule]\"}"
    ,
    TestID -> "Indent2-20150929-J74Q91"
]

(* This shouldn't be handled (as a whole) by the FullFormStrings logic, because it's not a string. Regression test. *)
Test[
    CalculateParse`GeneralLibrary`Indent2[
        HoldComplete[
            CalculateParse`Prototype`VirtualAssistant`VaActions`DivideHack["USDollars", "Kilograms"]
        ],
        "FullFormStrings" -> True
    ]
    ,
    "HoldComplete[DivideHack[\"USDollars\", \"Kilograms\"]]"
    ,
    TestID -> "Indent2-20151009-KBD361"
]

Test[
    CalculateParse`GeneralLibrary`Indent2[
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
    "{\n    {<|\"SequenceSize\" -> 1, \"Counts\" -> {{} -> <|{1} -> 2, {2} -> 3, {3} -> 2, {4} -> 2|>}|>},\n    {\n        <|\n            \"SequenceSize\" -> 2,\n            \"Counts\" ->\n            {\n                {1} -> <|{2} -> 2|>,\n                {2} -> <|{3} -> 2, {4} -> 1|>,\n                {3} -> <|{1} -> 1, {4} -> 1|>,\n                {4} -> <|{2} -> 1|>\n            }\n        |>\n    }\n}",
    TestID -> "Indent2-20151009-4B30NM"
]