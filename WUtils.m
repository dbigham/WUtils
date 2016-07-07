BeginPackage["WUtils`WUtils`"]

ReloadWUtils::usage = "ReloadWUtils  "

GetFunctionSource::usage = "GetFunctionSource  "

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

FindMessageDefinitions::usage = "FindMessageDefinitions  "

FindCommentBeforeSpan::usage = "FindCommentBeforeSpan  "

ExpandSpansToIncludeTrailingSemiColons::usage = "ExpandSpansToIncludeTrailingSemiColons  "

TestFun33::usage = "TestFun33  "

DeleteNesetedSpans::usage = "DeleteNesetedSpans  "

CopyFunction::usage = "CopyFunction  "

InsertStringAfterMatch::usage = "InsertStringAfterMatch  "

InsertStringBeforeMatch::usage = "InsertStringBeforeMatch  "

InsertStringInFile::usage = "InsertStringInFile  "

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
	Block[{name, spans, firstBracketCharPos, nextNewlineCharPos, remainder, endPos, cases, bracketType,
		   closingBracketPos, remainderOfLine, nextNonWhitespaceCharPos, funcName},
		
		name = SymbolName[func];
		
		funcName = SymbolName[func];
		
		spans = GetFunctionCodeSections[src, funcName];
		
		If [spans === {} || spans === $Failed,
			Message[GetFunctionSource::cff, func];
			Return[$Failed];
		];
		
		spans = Join[spans, FindMessageDefinitions[src, funcName]];
		spans = Join[spans, FindOptionsCodeSections[src, funcName]];
		spans = Join[spans, FindAttributesCodeSections[src, funcName]];
		spans = Join[spans, FindMathdocComments[src, funcName]];
		
		spans =
			Join[
				spans,
				Flatten[DeleteCases[(FindCommentBeforeSpan[src, #1] & ) /@ spans, {}], 1]
			];
		
		(* We should keep trailing semi-colons if they're present. *)
		spans = ExpandSpansToIncludeTrailingSemiColons[src, spans];
		
		(* We should delete spans that are nested. If a larger span
		   contains a smaller span, then get rid of the smaller span.
		   One case where this is important is if a comment such as a
		   top-of-function Mathdoc comment contains something that
		   looks like a down value. *)
		spans = DeleteNesetedSpans[spans];
		
		spans = Sort[spans];
		
		StringJoin[Riffle[StringTake[src, #] & /@ spans, "\n"]]
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

(*!
	\function FindMessageDefinitions
	
	\calltable
		FindMessageDefinitions[str, funcName] '' finds message definitions for the given function.

	Examples:
	
	FindMessageDefinitions["Func::msg = \"Message text\"", "Func"] === {{1, 26}}

	Unit tests: FindMessageDefinitions.mt

	\maintainer danielb
*)
FindMessageDefinitions[str_, funcName_] :=
	Block[{},
		FindCodeSections[
			str,
			StartOfLine ~~
			(" " | "	")... ~~
			funcName ~~ "::" ~~
			(messageName:((LetterCharacter | DigitCharacter)..) /; (messageName =!= "usage")) ~~
			WhitespaceCharacter.. ~~
			"=" ~~
			WhitespaceCharacter.. ~~
			"\"",
			"\"",
			1
		] 
	];

(*!
	\function FindCommentBeforeSpan
	
	\calltable
		FindCommentBeforeSpan[str, span] '' returns the span of a comment if there is one that immediately precedes the given span.

	Examples:
	
	FindCommentBeforeSpan["(* Comment *)\nFunc[] := 1", {15, -1}] === {{1, 13}}

	Unit tests: FindCommentBeforeSpan.mt

	\maintainer danielb
*)
FindCommentBeforeSpan[str_, span_] :=
	Block[{priorString},
		priorString = StringTake[str, {1, span[[1]] - 1}];
		StringPosition[
			priorString,
			RegularExpression["\\(\\*.*?\\*\\)(?=\\s*$)"]
		]
	];

(*!
	\function ExpandSpansToIncludeTrailingSemiColons
	
	\calltable
		ExpandSpansToIncludeTrailingSemiColons[str, spans] '' given a list of spans, expand them to the right if they are followed by a trailing semi-colon.

	Examples:
	
	ExpandSpansToIncludeTrailingSemiColons["Func[]; Func2[];", {{1, 6}, {9, 15}}]

	===

	{{1, 7}, {9, 16}}

	Unit tests: ExpandSpansToIncludeTrailingSemiColons.mt

	\maintainer danielb
*)
ExpandSpansToIncludeTrailingSemiColons[str_, spans_] :=
	Block[{stringLen = StringLength[str]},
		Function[{span},
			With[{endPos = span[[2]]},
				If [endPos < stringLen && StringTake[str, {endPos + 1}] === ";",
					{span[[1]], endPos + 1}
					,
					span
				]
			]
		] /@ spans
	];

(*!
	\function DeleteNesetedSpans
	
	\calltable
		DeleteNesetedSpans[spans] '' delete spans that occur inside of other, larger spans.

	Examples:
	
	DeleteNesetedSpans[{{1, 5}, {3, 5}, {4, 8}, {4, 4}, {8, 8}}] === {{1, 5}, {4, 8}}

	Unit tests: DeleteNesetedSpans.mt

	\maintainer danielb
*)
DeleteNesetedSpans[spansIn_] :=
	Block[{spans = spansIn, prevSpan},
		spans = Sort[spans];
		spans =
			MapIndexed[
				With[{index = First[#2], item = #1},
					Which[
						index < Length[spans] && item[[1]] == spans[[index + 1, 1]] && item[[2]] < spans[[index + 1, 2]],
						XPrint[item, ": A"];
						Sequence @@ {}
						,
						index > 1 && (item[[2]] < spans[[index - 1, 2]] || (item[[1]] > spans[[index - 1, 1]] && item[[2]] <= spans[[index - 1, 2]])),
						XPrint[item, ": B"];
						Sequence @@ {}
						,
						True,
						item
					]
				] &,
				spans
			];
		DeleteDuplicates[spans]
	];

(*!
	\function CopyFunction
	
	\calltable
		CopyFunction[func, sourceFile, destFile] '' copy the source code of a function from one file to another.

	Examples:
	
	CopyFunction[func, sourceFile, destFile] === TODO
	
	\related '
	
	\maintainer danielb
*)
CopyFunction::cgs = "Couldn't get the source code for function `1` from `2`";
CopyFunction[func_, sourceFile_, destFile_] :=
	Block[{funcSourceCode, fileSource, exportedFunctionQ},
		
		If [!FileExistsQ[sourceFile], Message[CopyFunction::noopen, sourceFile]; Return[$Failed]];
		If [!FileExistsQ[destFile], Message[CopyFunction::noopen, destFile]; Return[$Failed]];
		
		fileSource = Import[sourceFile, "Text"];
		
		funcSourceCode = GetFunctionSource[func, fileSource];
		If [!StringQ[funcSourceCode],
			Message[CopyFunction::cgs, func, sourceFile];
			Return[$Failed];
		];
		
		InsertStringInFile[
			destFile,
			funcSourceCode <> "\n\n",
			StartOfLine ~~ "End[]",
			(* Since some files, like GeneralLibrary.m have multiple instances of End[]. *)
			"LastMatch" -> True
		];
		
		exportedFunctionQ = !StringFreeQ[fileSource, SymbolName[func] <> "::usage"];
		Print["exportedFunctionQ: ", exportedFunctionQ];
		If [TrueQ[exportedFunctionQ],
			ExportSymbol[func, destFile];
		];
	];
	
(*!
	\function InsertStringAfterMatch
	
	\calltable
		InsertStringAfterMatch[str, strToInsert, strToMatch] '' given a string, insert 'strToInsert' into it after the location of 'strToMatch'.
	
	Example:
	
	InsertStringAfterMatch["abc def ghi", " 123", "def"] === "abc def 123 ghi"

	Unit tests:

	RunUnitTests[CalculateParse`GeneralLibrary`InsertStringAfterMatch]

	\related 'InsertStringInFile
	
	\maintainer danielb
*)
Options[InsertStringAfterMatch] =
{
	"LastMatch" -> False	(*< insert wrt the last match, rather than the first match. *)
};
InsertStringAfterMatch[str_, strToInsert_, strToMatch_, OptionsPattern[]] :=
	Module[{pos, posToUse},
		
		pos =
			StringPosition[
				str,
				strToMatch,
				If [TrueQ[OptionValue["LastMatch"]],
					Infinity
					,
					1
				]
			];
			
		If [pos === {},
			$Failed
			,
			If [TrueQ[OptionValue["LastMatch"]],
			   posToUse = pos[[-1]];
			   ,
			   posToUse = pos[[1]];
			];
			
			StringInsert[str, strToInsert, posToUse[[2]] + 1]
		]
	]

(*!
	\function InsertStringBeforeMatch
	
	\calltable
		InsertStringBeforeMatch[str, strToInsert, strToMatch] '' given a string, insert 'strToInsert' into it before the location of 'strToMatch'.
	
	Example:
	
	InsertStringBeforeMatch["abc def ghi", " 123", "def"] === "abc def 123 ghi"
	
	\related 'InsertStringInFile 'InsertStringAfterMatch
	
	\maintainer danielb
*)
Options[InsertStringBeforeMatch] =
{
	"LastMatch" -> False	(*< insert wrt the last match, rather than the first match. *)
};
InsertStringBeforeMatch[str_, strToInsert_, strToMatch_, OptionsPattern[]] :=
	Module[{pos, posToUse},
		
		pos =
			StringPosition[
				str,
				strToMatch,
				If [TrueQ[OptionValue["LastMatch"]],
					Infinity
					,
					1
				]
			];
		
		If [pos === {},
			$Failed
			,
			
			If [TrueQ[OptionValue["LastMatch"]],
			   posToUse = pos[[-1, 1]];
			   ,
			   posToUse = pos[[1, 1]];
			];
			
			StringInsert[str, strToInsert, posToUse]
		]
	]

(*!
	\function InsertStringInFile
	
	\calltable
		InsertStringInFile[file, strToInsert, strToMatch] '' given a file, insert 'strToInsert' into it after the location of 'strToMatch'.
	
	Example:
	
	With[{file = FileNameJoin[{$TemporaryDirectory,"InsertStringInFile.m"}]},
		Export[file, "line\nINSERT AFTER\nline", "String"];
		InsertStringInFile[file, "\ninserted string", "INSERT AFTER", "AfterMatch" -> True];
		With[{res = Import[file, "String"]},
			DeleteFile[file];
			res
		]
	]

	Unit tests:

	RunUnitTests[InsertStringInFile]

	\related 'InsertStringAfterMatch
	
	\maintainer danielb
*)
Options[InsertStringInFile] =
{
	"AfterMatch" -> False,		(*< By default, this function inserts the string at the position of the match. If AfterMatch -> True, the insertion is performed at the end of the matched string. *)
	"LastMatch" -> False,		(*< insert wrt the last match, rather than the first match. *)
	"DataString" -> None		(*< If the file contents are already in memory, they can be passed in. In that case, they will be returned rather than written to disk. *)
};
InsertStringInFile::fi = "Failed insertion for file `1`";
InsertStringInFile[file_, strToInsert_, strToMatch_, OptionsPattern[]] :=
	Module[{data},
		If [!FileExistsQ[file],
			Message[InsertStringInFile::noopen, file];
			Return[$Failed];
			,
			If [OptionValue["DataString"] =!= None,
				data = OptionValue["DataString"];
				,
				data = Import[file, "Text"];
			];
			If [TrueQ[OptionValue["AfterMatch"]],
				data = InsertStringAfterMatch[data, strToInsert, strToMatch, "LastMatch" -> OptionValue["LastMatch"]];
				,
				data = InsertStringBeforeMatch[data, strToInsert, strToMatch, "LastMatch" -> OptionValue["LastMatch"]];
			];
			If [data =!= $Failed,
				If [OptionValue["DataString"] === None,
					Export[file, data, "Text"];
					,
					data
				]
				,
				Message[InsertStringInFile::fi, file];
				$Failed
			]
		]
	]

End[]

EndPackage[]