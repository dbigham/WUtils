BeginPackage["WUtils`WUtils`"]

Needs["CalculateParse`GeneralLibrary`"]; (* CreateReloadFunctionForDirectory, etc. *)

ReloadWUtils::usage = "ReloadWUtils  "

GetFunctionSource::usage = "GetFunctionSource  "

NextCharOfType::usage = "NextCharOfType  "

NextMatchingSubstring::usage = "NextMatchingSubstring  "

FindMatchingBracket::usage = "FindMatchingBracket  "

DoubleQuotedStringPattern::usage = "DoubleQuotedStringPattern  "

XPrint::usage = "XPrint  "

FindFirstBracketNotInComment::usage = "FindFirstBracketNotInComment  "

FindFirstNonWhitespceChar::usage = "FindFirstNonWhitespceChar  "

FindCodeSections::usage = "FindCodeSections  "

FindOptionsCodeSections::usage = "FindOptionsCodeSections  "

FindAttributesCodeSections::usage = "FindAttributesCodeSections  "

FindMathdocComments::usage = "FindMathdocComments  "

GetFunctionCodeSections::usage = "GetFunctionCodeSections  "

Begin["`Private`"]

With[{package = "WUtils`"},
With[{dir = DirectoryName[DirectoryName[FindFile[package]]]},
	WUtils`WUtils`Private`$ReloadFunction = ReloadWUtils;
	CalculateParse`GeneralLibrary`TabsOrSpaces[package] = "Tabs";
	If [!ValueQ[$reloadWUtils],
		$reloadWUtils =
			CreateReloadFunctionForDirectory[
				DirectoryName[DirectoryName[FindFile[package]]]
			];
	];
	WUtils`$UnitTestDir = FileNameJoin[{DirectoryName[DirectoryName[FindFile[package]]], "Tests"}];
	CalculateParse`Prototype`VirtualAssistant`Utility`NotebookTypeToDirectory[package] = FileNameJoin[{dir, "Notebooks"}];
];
];

(* Reloads .m files in this directory if they've changed. *)
ReloadWUtils[] := $reloadWUtils[]
If [ListQ[Global`$VaReloadFunctions],
	Global`$VaReloadFunctions =
		DeleteDuplicates[
			Append[Global`$VaReloadFunctions, ReloadWUtils]
		]
	];

(* Handy for disabling Print statements. Ensures that their arguments will no
   longer evaluate when they are disabled so that they don't slow the code down. *)
Attributes[XPrint] = {HoldAllComplete};

(*!
	\function GetFunctionSource
	
	\calltable
		GetFunctionSource[func] '' given a function, returns the source code.
		GetFunctionSource[func, source] '' given a function and the source code file's contents, returns the source code of the function.
	
	NOTE: This doesn't currently grab the line that exports the symbol if it's exported.
	
	Examples:
	
	GetFunctionSource[MyFunc, "MyFunc[] := Module[{}, 1]"] === "MyFunc[] := Module[{}, 1]"

	Unit tests: GetFunctionSource.mt

	\maintainer danielb
*)
Clear[GetFunctionSource];
Options[GetFunctionSource] =
{
	"File" -> Automatic			(*< the source code file. *)
};
GetFunctionSource[func_Symbol, OptionsPattern[]] :=
	Block[{file = OptionValue["File"], src, name},
		If [file === Automatic,
			Print["TODO: File -> Automatic"];
			Return[$Failed];
		];
		
		src = Import[file, "Text"];
		
		GetFunctionSource[func, src]
	];

GetFunctionSource::cff = "Couldn't find function `1`";
GetFunctionSource[func_Symbol, src_String] :=
	Block[{name, positions, firstBracketCharPos, nextNewlineCharPos, remainder, endPos, cases, bracketType,
		   closingBracketPos, remainderOfLine, nextNonWhitespaceCharPos, funcName},
		
		name = SymbolName[func];
		
		funcName = SymbolName[func];
		
		positions = GetFunctionCodeSections[src, funcName];
		
		If [positions === {} || positions === $Failed,
			Message[GetFunctionSource::cff, func];
			Return[$Failed];
		];
		
		positions = Join[positions, FindOptionsCodeSections[src, funcName]];
		positions = Join[positions, FindAttributesCodeSections[src, funcName]];
		positions = Join[positions, FindMathdocComments[src, funcName]];
		
		positions = Sort[positions];
		
		StringJoin[Riffle[StringTake[src, #] & /@ positions, "\n\n"]]
	];

bracketPattern["[" | "]"] := "[" | "]";
bracketPattern["(" | ")"] := "(" | ")";
bracketPattern["{" | "}"] := "{" | "}";
bracketPattern["(*" | "*)"] := "(*" | "*)";
bracketPattern[_] := $Failed;

bracketValue["["] := 1;
bracketValue["]"] := -1;
bracketValue["("] := 1;
bracketValue[")"] := -1;
bracketValue["{"] := 1;
bracketValue["}"] := -1;
bracketValue["(*"] := 1;
bracketValue["*)"] := -1;
bracketValue[_] := 0;

(*!
	\function FindMatchingBracket
	
	\calltable
		FindMatchingBracket[str, firstBracketPos] '' find the matching closing bracket in the string.
		FindMatchingBracket[str, bracket, startPos, initialCount] '' find the matching closing bracket in the string, starting from the given 'startPos', and starting with the initial count provided.
	
	NOTE: If the 'bracket' is a double quote, then
		  FindMatchingBracket[str, bracket, startPos, initialCount] makes the assumption that
		  the character previous to 'startPos' is the opening double quote.
	
	Examples:
	
	FindMatchingBracket["Func[1, 2, 3]", 5] === 13

	Unit tests: FindMatchingBracket.mt

	\maintainer danielb
*)
FindMatchingBracket::invb = "Invalid bracket character: `1`";
FindMatchingBracket[str_String, bracket_String, startPos_Integer, initialCount_Integer] :=
	Block[{nextTokenPos, remainder, pattern, counter, eatenPos, stringMatchPos},
		
		remainder = StringTake[str, {startPos, -1}];
		
		If [bracket === "\"",
			stringMatchPos = StringPosition[StringTake[str, {startPos - 1, -1}], doubleQuotedStringPattern[], 1];
			If [stringMatchPos =!= {} && stringMatchPos[[1, 1]] === 1,
				Return[startPos + stringMatchPos[[1, 2]] - 2];
				,
				Return[$Failed];
			];
		];
		
		pattern = bracketPattern[bracket];
		If [pattern === $Failed,
			Message[FindMatchingBracket::invb, bracket];
			Return[$Failed];
		];
		pattern = pattern | doubleQuotedStringPattern[] | commentPattern[];
		eatenPos = startPos - 1;
		counter = initialCount;
		While[True,
			nextTokenPos = StringPosition[remainder, pattern, 1];
			If [nextTokenPos === {},
				Return[$Failed, Block];
			];
			counter += bracketValue[StringTake[remainder, nextTokenPos[[1]]]];
			If [counter === 0,
				(* Found matching closing bracket. *)
				Return[eatenPos + nextTokenPos[[1, 2]], Block];
			];
			eatenPos += nextTokenPos[[1, 2]];
			remainder = StringTake[remainder, {nextTokenPos[[1, 2]] + 1, -1}];
		];
	]
	
FindMatchingBracket[str_String, firstBracketPos_Integer] :=
	Block[{bracketChar},
		
		bracketChar = StringTake[str, {firstBracketPos}];
		
		FindMatchingBracket[str, bracketChar, firstBracketPos + 1, 1]
	];

(*!
	\function doubleQuotedStringPattern
	
	\calltable
		doubleQuotedStringPattern[] '' a string pattern that can be used to find instances of double quoted strings.

	Example:

	StringCases["Just \"testing\"", doubleQuotedStringPattern[]] === {"\"testing\""}

	Unit tests: doubleQuotedStringPattern.mt

	\maintainer danielb
*)
doubleQuotedStringPattern[] := ("\"" ~~ Shortest[___] ~~ (Except["\\"] | "\\\\") ~~ "\"");

(*!
	\function commentPattern
	
	\calltable
		commentPattern[] '' a string pattern that can be used to find instances of comments.

	Example:

	StringCases[
		"(* just testing *)",
		WUtils`WUtils`Private`commentPattern[]
	]

	===

	{"(* just testing *)"}

	Unit tests: commentPattern.mt

	\maintainer danielb
*)
commentPattern[] := RegularExpression["\\(\\*.*?\\*\\)"];

(*!
	\function FindFirstBracketNotInComment
	
	\calltable
		FindFirstBracketNotInComment[str, startPos] '' find the first instance of a 'bracket', but ensure that it isn't inside of a comment.

	Examples:
	
	FindFirstBracketNotInComment["Func[]", 1] === 5

	Unit tests: FindFirstBracketNotInComment.mt

	\maintainer danielb
*)
FindFirstBracketNotInComment[str_, startPos_] :=
	Block[{pattern, remainder, eatenPos, counter, nextTokenPos},
		pattern = commentPattern[] | "(" | "[" | doubleQuotedStringPattern[];
		remainder = str;
		eatenPos = 0;
		counter = 1;
		While[True,
			nextTokenPos = StringPosition[remainder, pattern, 1];
			If [nextTokenPos === {},
				Return[None, Block];
			];
			If [!StringMatchQ[StringTake[remainder, {nextTokenPos[[1, 1]], -1}], StartOfString ~~ (commentPattern[] ~~ ___)],
				(* Not a comment. This is what we want. *)
				Return[eatenPos + nextTokenPos[[1, 1]], Block];
			];
			eatenPos += nextTokenPos[[1, 2]];
			remainder = StringTake[remainder, {nextTokenPos[[1, 2]] + 1, -1}];
		];
	];

(*!
	\function FindFirstNonWhitespceChar
	
	\calltable
		FindFirstNonWhitespceChar[str] '' return the position of the first non-whitespace character, but skip comments.

	Examples:
	
	FindFirstNonWhitespceChar["(* Comment *) abc"] === 15
	
	\maintainer danielb
*)
FindFirstNonWhitespceChar[str_] :=
	Block[{pattern, remainder, eatenPos, counter, nextTokenPos},
		pattern = commentPattern[] | Except[WhitespaceCharacter];
		remainder = str;
		eatenPos = 0;
		counter = 1;
		While[True,
			nextTokenPos = StringPosition[remainder, pattern, 1];
			If [nextTokenPos === {},
				Return[None, Block];
			];
			If [!StringMatchQ[StringTake[remainder, {nextTokenPos[[1, 1]], -1}], StartOfString ~~ (commentPattern[] ~~ ___)],
				(* Not a comment. This is what we want. *)
				Return[eatenPos + nextTokenPos[[1, 1]], Block];
			];
			eatenPos += nextTokenPos[[1, 2]];
			remainder = StringTake[remainder, {nextTokenPos[[1, 2]] + 1, -1}];
		];
	];

(*!
	\function FindCodeSections
	
	\calltable
		FindCodeSections[str, pattern, endType, initialBracketCount] '' find code sections within a string 'str'. The 'patterns' identify the beginning of a code section while the 'endType' specifies how to look for the end of the code section.

	Examples:
	
	FindCodeSections["Options[Func] = { ... }", "Options[", "}", 0] === {{1, 23}}

	Unit tests: FindCodeSections.mt

	\maintainer danielb
*)
FindCodeSections::cfcb = "Couldn't find closing bracket";
FindCodeSections[str_, pattern_, endType_, initialBracketCount_] :=
	Block[{positions, closingBracketPos},
		
		positions = StringPosition[str, pattern];
		
		Function[{position},
			closingBracketPos = FindMatchingBracket[str, endType, position[[2]] + 1, initialBracketCount];
			If [closingBracketPos === $Failed,
				Message[FindCodeSections::cfcb];
				Return[$Failed, Block];
			];
			{position[[1]], closingBracketPos}
		] /@ positions
	];

(*!
	\function FindOptionsCodeSections
	
	\calltable
		FindOptionsCodeSections[str, funcName] '' find code sections that are defining options for the given function name.

	Examples:
	
	FindOptionsCodeSections[
		"Options[Func] =
{
	\"MyOption\" -> Automatic	(*< sample option *)
}
",
		"Func"
	]

	===

	{{1, 65}}

	Unit tests: FindOptionsCodeSections.mt

	\maintainer danielb
*)
FindOptionsCodeSections[str_, funcName_] :=
	Block[{},
		FindCodeSections[
			str,
			StartOfLine ~~
			(" " | "	")... ~~
			"Options[" ~~
			WhitespaceCharacter... ~~
			funcName ~~
			WhitespaceCharacter... ~~
			"]" ~~
			WhitespaceCharacter... ~~
			"=",
			"}",
			0
		] 
	];

(*!
	\function FindAttributesCodeSections
	
	\calltable
		FindAttributesCodeSections[str, funcName] '' find code sections that are defining attributes for the given function name.

	Examples:
	
    FindAttributesCodeSections["Attributes[Func] = {HoldFirst}", "Func"] === {{1, 30}}

    Unit tests: FindAttributesCodeSections.mt

    \maintainer danielb
*)
FindAttributesCodeSections[str_, funcName_] :=
	Block[{},
		FindCodeSections[
			str,
			StartOfLine ~~
			(" " | "	")... ~~
			"Attributes[" ~~
			WhitespaceCharacter... ~~
			funcName ~~
			WhitespaceCharacter... ~~
			"]" ~~
			WhitespaceCharacter... ~~
			"=",
			"}",
			0
		] 
	];

(*!
	\function FindMathdocComments
	
	\calltable
		FindMathdocComments[str, funcName] '' find top of function Mathdoc comments.

	Examples:
	
    FindMathdocComments[
        "(!\n\t\\function FindMathdocComments\n\t\n\t\\calltable\n\t\tFindMathdocComments[str, funcName] '' find top of function Mathdoc comments.\n\t\n\t\

    Unit tests: FindMathdocComments.mt

    \maintainer danielb\n)\n",
        "FindMathdocComments"
    ]

    ===

    {}

	\maintainer danielb
*)
FindMathdocComments[str_, funcName_] :=
	Block[{},
		FindCodeSections[
			str,
			StartOfLine ~~
			(" " | "	")... ~~
			"(*!" ~~
			WhitespaceCharacter... ~~
			"\\function" ~~
			WhitespaceCharacter... ~~
			funcName ~~
			WhitespaceCharacter..,
			"*)",
			1
		] 
	];

(*!
	\function GetFunctionCodeSections
	
	\calltable
		GetFunctionCodeSections[str, funcName] '' returns code sections that define down values for the given function.

	Examples:
	
    GetFunctionCodeSections["MyFunc[] := Module[{}, 1]", "MyFunc"] === {{1, 25}}

    Unit tests: GetFunctionCodeSections.mt

    \maintainer danielb
*)
Clear[GetFunctionCodeSections];
GetFunctionCodeSections::cfn = "Couldn't find down value definition after ':='.";
GetFunctionCodeSections::cfcb = "Couldn't find closing bracket";
GetFunctionCodeSections[str_String, funcName_String] :=
	Block[{positions, remainder, nextNonWhitespaceCharPos, firstBracketCharPos, nextNewlineCharPos, closingBracketPos},
		
		(* Find positions that look like they're the start of a down value definition for the function. *)
		positions =
		StringPosition[
			str,
			StartOfLine ~~
			(" " | "	")... ~~
			funcName ~~
			(" " | "	")... ~~
			"[" ~~
			RepeatedNull[Except["\n"]] ~~
			"]" ~~
			(" " | "	")... ~~
			":="
		];
		
		(* For each position that looked like the start of a down value definition,
		   try to find the ending position of the down value definition, and then
		   the string span for that down value definition. *)
		Function[{pos},
			Catch[
				With[{startPos = pos[[2]] + 1},
					
					remainder = StringTake[str, {startPos, -1}];
					nextNonWhitespaceCharPos = FindFirstNonWhitespceChar[remainder];
					
					XPrint["remainder: ", InputForm[remainder]];
					XPrint["nextNonWhitespaceCharPos: ", nextNonWhitespaceCharPos];
					
					If [nextNonWhitespaceCharPos === None,
						(* Bizarre, there's nothing after the ":=". *)
						Message[GetFunctionCodeSections::cfn];
						Return[$Failed, Block];
					];
					
					remainder = StringTake[remainder, {nextNonWhitespaceCharPos, -1}];
					
					firstBracketCharPos = FindFirstBracketNotInComment[remainder, nextNonWhitespaceCharPos];
					nextNewlineCharPos = StringPosition[remainder, "\n" | "\r\n", 1];
					
					If [nextNewlineCharPos =!= {} &&
						(firstBracketCharPos === None || firstBracketCharPos > nextNewlineCharPos[[1, 1]]),
						(* There aren't any brackets in the next non-empty line, so it
						   should be the entire down value. *)
						XPrint["A"];
						Throw[{pos[[1]], startPos + nextNonWhitespaceCharPos + nextNewlineCharPos[[1, 1]] - 3}];
					];
					
					If [firstBracketCharPos === None,
						(* We couldn't find any brackets, and we couldn't find
						   a newline, so the down value must extend to the end
						   of the string. *)
						XPrint["B"];
						{pos[[1]], StringLength[str]}
						,
						closingBracketPos = FindMatchingBracket[remainder, firstBracketCharPos];
						If [closingBracketPos === $Failed,
							Message[GetFunctionCodeSections::cfcb];
							Return[$Failed, Block];
							,
							XPrint["C"];
							{pos[[1]], pos[[2]] + nextNonWhitespaceCharPos + closingBracketPos - 1}
						]
					]
				]
			]
		] /@ positions
	];

End[]

EndPackage[]