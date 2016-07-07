BeginPackage["WUtils`WUtils`"]

ReloadWUtils::usage = "ReloadWUtils  "

GetFunctionSource::usage = "GetFunctionSource  "

FindMatchingBracket::usage = "FindMatchingBracket  "

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

DeleteNesetedSpans::usage = "DeleteNesetedSpans  "

CopyFunction::usage = "CopyFunction  "

InsertStringAfterMatch::usage = "InsertStringAfterMatch  "

InsertStringBeforeMatch::usage = "InsertStringBeforeMatch  "

InsertStringInFile::usage = "InsertStringInFile  "

NewPackageFormatQ::usage = "NewPackageFormatQ  "

GetMaxStringPos::usage = "GetMaxStringPos  "

GetMinStringPos::usage = "GetMinStringPos  "

PreviousNewlineChar::usage = "PreviousNewlineChar  "

ExportSymbol::usage = "ExportSymbol  "

NextNewlineChar::usage = "NextNewlineChar  "

CreateMemoizationFunction::usage = "CreateMemoizationFunction  "

CopyFunctionUI::usage = "CopyFunctionUI  "

FunctionDependencies::usage = "FunctionDependencies  "

FilterOptions::usage = "FilterOptions  "

ImmediateFunctionDependencies::usage = "ImmediateFunctionDependencies  "

ProcessOneByOne::usage = "ProcessOneByOne  "

OpenFileInWorkbench::usage = "OpenFileInWorkbench  "

GetLineNumberOfStringInFile::usage = "GetLineNumberOfStringInFile  "

GetLineNumberOfString::usage = "GetLineNumberOfString  "

GetLineNumberOfPos::usage = "GetLineNumberOfPos  "

CreateReloadFunctionForDirectory::usage = "CreateReloadFunctionForDirectory  "

ComputeDependencyGraph::usage = "ComputeDependencyGraph  "

StringStartsWith::usage = "StringStartsWith  "

CreateHeldVarIfNull::usage = "CreateHeldVarIfNull  "

NewHeldVar::usage = "NewHeldVar  "

SetHeldVar::usage = "SetHeldVar  "

GetPackageName::usage = "GetPackageName  "

WLSymbolPattern::usage = "WLSymbolPattern  "

Memoized::usage = "Memoized  "

ReadListFileHeld::usage = "ReadListFileHeld  "

ProcessFileDependencies::usage = "ProcessFileDependencies  "

PreReloadFile::usage = "PreReloadFile  "

ModificationAgeInMinutes::usage = "ModificationAgeInMinutes  "

FileToContext::usage = "FileToContext  "

PostReloadFile::usage = "PostReloadFile  "

StringTakeByDelim::usage = "StringTakeByDelim  "

Begin["`Private`"]

(* Note: This code needs to come after the function definitions in this file, since
   otherwise things like CreateReloadFunctionForDirectory won't yet be defined. *)
With[{package = "WUtils`"},
With[{dir = DirectoryName[DirectoryName[FindFile[package]]]},
	WUtils`WUtils`Private`$ReloadFunction = ReloadWUtils;
	WUtils`TabsOrSpaces[package] = "Tabs";
	If [!ValueQ[$reloadWUtils],
		$reloadWUtils =
			CreateReloadFunctionForDirectory[
				DirectoryName[DirectoryName[FindFile[package]]]
			];
	];
	WUtils`$UnitTestDir = FileNameJoin[{DirectoryName[DirectoryName[FindFile[package]]], "Tests"}];
	Lui`NotebookTypeToDirectory[package] = FileNameJoin[{dir, "Notebooks"}];
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
	
	Known issues:
	
	- This function doesn't work if a down value's value has a top-level
	  infix operator. ex. MyFunc[] := "(" | ")"
	- That said, it would be easy to fix the case where the infix operator
	  occurs on the same line as ':='. (TODO)
	
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
		
		(* For now I'm going to create a single span that goes from
		   the start of the first span to the end of the last span
		   so that, for example, we don't miss definitions of
		   global variables that might happen after the top-of-function
		   Mathdoc comment but before the down value definition.
		   This is dangerous though because:
		   1) There might be things in between our spans that we don't
			  want.
		   2) If a function doesn't have a Mathdoc comment, or if it
			  has necessary globals defined prior to that comment,
			  or below the last down value definition, then those
			  code sections will be missed. (but creating a single span
			  doesn't cause that -- it just doesn't do anything to
			  protect us against that) *)
		If [Length[spans] > 0,
			spans = {{spans[[1, 1]], spans[[-1, 2]]}};
		];
		
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
Clear[CopyFunction];
Options[CopyFunction] =
{
	"DestContext" -> Null			(*< The destination context. Probabably would have been better to make CopyFunction take this as its argument rather than 'destFile'. *)
};
CopyFunction[func_, sourceFile_, destFile_, OptionsPattern[]] :=
	Block[{funcSourceCode, fileSource, exportedFunctionQ},
		
		If [!FileExistsQ[sourceFile], Message[CopyFunction::noopen, sourceFile]; Return[$Failed]];
		If [!FileExistsQ[destFile], Message[CopyFunction::noopen, destFile]; Return[$Failed]];
		
		fileSource = Import[sourceFile, "Text"];
		
		funcSourceCode = GetFunctionSource[func, fileSource];
		If [!StringQ[funcSourceCode],
			Message[CopyFunction::cgs, func, sourceFile];
			Return[$Failed];
		];
		
		(* If the full context occurs anywhere, then replace it. *)
		If [OptionValue["DestContext"] =!= Null,
			funcSourceCode = StringReplace[funcSourceCode, Context[func] :> OptionValue["DestContext"]];
		];
		
		InsertStringInFile[
			destFile,
			funcSourceCode <> "\n\n",
			StartOfLine ~~ "End[]",
			(* Since some files, like GeneralLibrary.m have multiple instances of End[]. *)
			"LastMatch" -> True
		];
		
		exportedFunctionQ = !StringFreeQ[fileSource, SymbolName[func] <> "::usage"];
		
		If [TrueQ[exportedFunctionQ],
			ExportSymbol[func, destFile];
		];
	];

(*!
	\function ExportSymbol
	
	\calltable
		ExportSymbol[symbol] '' given a currently Private symbol, modify its source file so that it is exported.
	
	Examples:
	
	ExportSymbol[
		WUtils`WUtils`Private`MyNewFunc
	]
	
	\related 'processNewlyDefinedPrivateSymbols 'PostReloadFile
	
	\maintainer danielb
*)
ExportSymbol[symbol_Symbol, file_] :=
	Module[{check, contents, targetPos, symbolName, found, insertionPos,
			insertOnPreviousLine, insertionStr, exportType},

		If [!FileExistsQ[file], Print["ExportSymbol: Missing file: " <> file]; Return[$Failed]];
		
		check = Check[contents = Import[file, "Text"], $Failed];
		
		If [check === $Failed,
			Print["ExportSymbol: Messages while reading: ", file, ". Replacement not performed on that file."];
			,
			check =
				Check[
					
					symbolName = SymbolName[symbol];
					
					If [!NewPackageFormatQ[file],
						
						targetPos = GetMaxStringPos[contents, symbolName <> "::usage"];
						
						(* Ensure the usage isn't already there. *)
						If [targetPos === None,
						
							targetPos = GetMaxStringPos[contents, StartOfLine ~~ LetterCharacter ~~ (LetterCharacter ~~ DigitCharacter)... ~~ "::usage"];
							If [targetPos =!= None,
								found = True;
								insertOnPreviousLine = False;
								,
								targetPos = GetMinStringPos[contents, StartOfLine ~~ "Begin" ~~ WhitespaceCharacter... ~~ "[" ~~ WhitespaceCharacter... ~~ "\"`Private`\"" ~~ WhitespaceCharacter... ~~ "]"];
								If [targetPos =!= None,
									found = True;
									insertOnPreviousLine = True;
								];
							];
							
							If [TrueQ[found],
							
								insertionStr = symbolName <> "::usage = \"" <> symbolName <> "  \"";
								
								If [TrueQ[insertOnPreviousLine],
									insertionPos = PreviousNewlineChar[contents, targetPos];
									contents = StringInsert[contents, "\n" <> insertionStr <> "\n", insertionPos];
									,
									insertionPos = NextNewlineChar[contents, targetPos];
									contents = StringInsert[contents, "\n\n" <> insertionStr, insertionPos];
								]
								,
								Print["ExportSymbol: Couldn't find insertion point for file: ", file];
								Return[$Failed];
							];
						];
						,
						targetPos = GetMaxStringPos[contents, StartOfLine ~~ ("PackageExport[" | "PackageScope[") ~~ WhitespaceCharacter... ~~ ToString[symbolName, InputForm]];
						
						(* See VaActions.m CreateFunctionInFile for comments on $PackageScopeFlag. *)
						If [TrueQ[$PackageScopeFlag],
							exportType = "PackageScope[";
							,
							If [Length[StringPosition[contents, "PackageScope["]] > 0,
								(* We'll guess they want PackageScope *)
								exportType = "PackageScope[";
								,
								(* We'll guess they want PackageExport *)
								exportType = "PackageExport[";
							];
						];
						
						(* Ensure the usage isn't already there. *)
						If [targetPos === None,
						
							targetPos = GetMaxStringPos[contents, StartOfLine ~~ WhitespaceCharacter... ~~ exportType];
							If [targetPos =!= None,
								found = True;
								insertOnPreviousLine = False;
								,
								targetPos = GetMinStringPos[contents, StartOfLine ~~ WhitespaceCharacter... ~~ "Package["];
								If [targetPos =!= None,
									found = True;
									insertOnPreviousLine = False;
								];
							];
							
							If [TrueQ[found],
							
								insertionStr = exportType <> ToString[symbolName, InputForm] <> "]";
								
								If [TrueQ[insertOnPreviousLine],
									insertionPos = PreviousNewlineChar[contents, targetPos];
									contents = StringInsert[contents, "\n" <> insertionStr <> "\n", insertionPos];
									,
									insertionPos = NextNewlineChar[contents, targetPos];
									contents = StringInsert[contents, "\n\n" <> insertionStr, insertionPos];
								]
								,
								Print["ExportSymbol: Couldn't find insertion point for file: ", file];
								Return[$Failed];
							];
						];
					]
					,
					$Failed
				];
				
			If [check === $Failed,
				Print["ExportSymbol: Messages while performing file modifications: ", file, ". Aborted."];
				,
				Export[file, contents, "Text"];
			];
		];
	]

(*!
	\function InsertStringAfterMatch
	
	\calltable
		InsertStringAfterMatch[str, strToInsert, strToMatch] '' given a string, insert 'strToInsert' into it after the location of 'strToMatch'.
	
	Example:
	
	InsertStringAfterMatch["abc def ghi", " 123", "def"] === "abc def 123 ghi"

	Unit tests:

	RunUnitTests[WUtils`WUtils`InsertStringAfterMatch]

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

(*!
	\function CreateMemoizationFunction
	
	\calltable
		CreateMemoizationFunction[] '' returns a symbol who's default DownValue is a predictable String that specifies memoization has not been set. When used with Memoized, the symbol can be used to remember previous computations.
	
	\related 'Memoized
	
	\maintainer danielb
*)
CreateMemoizationFunction[] :=
	Module[{memoizationSymbol},
		memoizationSymbol[args___] := "CreateMemoizationFunction:NotSet";
		memoizationSymbol
	]

(*!
	\function NewPackageFormatQ
	
	\calltable
		NewPackageFormatQ[file] '' given a file, returns True if it appears to use the new package format.

	Examples:
	
	NewPackageFormatQ["C:\\Temp\\MyFile.m", "UseMemoization" -> False] === True

	Unit tests:

	RunUnitTests[WUtils`WUtils`NewPackageFormatQ]

	\maintainer danielb
*)
(*Clear[NewPackageFormatQ];*) (* Don't clear this incase we need to override it for certain files. *)
Options[NewPackageFormatQ] =
{
	"UseMemoization" -> True	(*< memoize results? *)
};
If [!ValueQ[$newPackageFormatQMemoization],
	(* WARNING: CreateMemoizationFunction[] must be defined in this file prior to this point. *)
	$newPackageFormatQMemoization = CreateMemoizationFunction[];
];
NewPackageFormatQ[file_, OptionsPattern[]] :=
	Module[{},
		Memoized[
			newPackageFormatQHelper[file],
			If [TrueQ[OptionValue["UseMemoization"]],
				$newPackageFormatQMemoization
				,
				None
			],
			"MemoizationKey" -> file
		]
	];

(*!
	\function newPackageFormatQHelper
	
	\calltable
		newPackageFormatQHelper[file] '' given a file, returns True if it appears to use the new package format.
	
	\maintainer danielb
*)
newPackageFormatQHelper[file_] :=
	Module[{definedPackage},
		
		definedPackage =
			StringCases[
				Import[file, "Text"],
				(WhitespaceCharacter | StartOfLine) ~~
				"Package" ~~ WhitespaceCharacter... ~~
				"[" ~~ WhitespaceCharacter... ~~
				package:doubleQuotedStringPattern[] ~~ WhitespaceCharacter... ~~
				"]" :>
				   package
			];
		
		ListQ[definedPackage] && Length[definedPackage] > 0
	];

(*!
	\function Memoized
	
	\calltable
		Memoized[e, memoizationSymbol] '' evaluates the given expression and remembers its result so that subsequent calls with the same arguments return fast. If the memoizationSymbol is None, then the computation is performed without memoization. The memoization symbol would typically be created with CreateMemoizationFunction.

	Examples:
	
	With[{memoizationSymbol = CreateMemoizationFunction[]},
		{
			AbsoluteTiming[Memoized[FindFile["WUtils`WUtils`"], memoizationSymbol]][[1]],
			AbsoluteTiming[Memoized[FindFile["WUtils`WUtils`"], memoizationSymbol]][[1]],
			AbsoluteTiming[Memoized[FindFile["WUtils`WUtils`"], memoizationSymbol]][[1]],
		}
	]
	
	===
	
	{0.014502, 0., 0.}
	
	\related 'CreateMemoizationFunction
	
	\maintainer danielb
*)
Clear[Memoized];
Attributes[Memoized] = {HoldFirst};
Options[Memoized] =
{
	"MemoizationKey" -> Automatic,		  (*< the key used to refer to a unique computation. *)
	"AbortIfMessages" -> False			  (*< if this is True, we will only store the result if there weren't messages. *)
};
Memoized[e_, memoizationSymbol_, OptionsPattern[]] :=
	Module[{failureFlag, evaluatedRes},
		If [memoizationSymbol === None,
			e
			,
			With[{strKey =
					If [OptionValue["MemoizationKey"] === Automatic,
						ToString[HoldComplete[e], InputForm]
						,
						OptionValue["MemoizationKey"]
					]
				 },
				With[{memoizedRes = memoizationSymbol[strKey]},
					If [memoizedRes =!= "CreateMemoizationFunction:NotSet",
						memoizedRes
						,
						failureFlag =
						   Check[
								evaluatedRes = e;
								,
								"Memoized:MessagesDetected"
						   ];
						   
						If [evaluatedRes =!= "Memoized:MessagesDetected" || OptionValue["AbortIfMessages"] === False,
							memoizationSymbol[strKey] = evaluatedRes;
						];
						
						evaluatedRes
					]
				]
			]
		]
	]

(*!
	\function GetMaxStringPos
	
	\calltable
		GetMaxStringPos[str, substringList] '' given a string and a list of substrings to look for, returns the minimum position in the string of a match, or None if none.
	
	Examples:
	
	GetMaxStringPos["abcdefg", {"g", "d"}] === 7
	
	\maintainer danielb
*)
GetMaxStringPos[str_, substringList_] :=
	Module[{maxPos = None, positions, startPos},
		
		Function[{substring},
			positions = StringPosition[str, substring];
			
			If [positions =!= {},
				startPos = positions[[-1, 1]];
				
				If [maxPos === None || startPos < maxPos,
					maxPos = startPos;
				];
			];
			
		] /@ Flatten[{substringList}];
		
		maxPos
	]

(*!
	\function GetMinStringPos
	
	\calltable
		GetMinStringPos[str, substringList] '' given a string and a list of substrings to look for, returns the minimum position in the string of a match, or None if none.
	
	Examples:
	
	GetMinStringPos["abcdefg", {"g", "d"}] === 4
	
	\maintainer danielb
*)
GetMinStringPos[str_, substringList_] :=
	Module[{minPos = None, positions, startPos},
		
		Function[{substring},
			positions = StringPosition[str, substring];
			
			If [positions =!= {},
				startPos = positions[[1, 1]];
				
				If [minPos === None || startPos < minPos,
					minPos = startPos;
				];
			];
			
		] /@ Flatten[{substringList}];
		
		minPos
	]

(*!
	\function GetMinStringPos
	
	\calltable
		GetMinStringPos[str, substringList] '' given a string and a list of substrings to look for, returns the minimum position in the string of a match, or None if none.
	
	Examples:
	
	GetMinStringPos["abcdefg", {"g", "d"}] === 4
	
	\maintainer danielb
*)
GetMinStringPos[str_, substringList_] :=
	Module[{minPos = None, positions, startPos},
		
		Function[{substring},
			positions = StringPosition[str, substring];
			
			If [positions =!= {},
				startPos = positions[[1, 1]];
				
				If [minPos === None || startPos < minPos,
					minPos = startPos;
				];
			];
			
		] /@ Flatten[{substringList}];
		
		minPos
	]

(*!
	\function PreviousNewlineChar
	
	\calltable
		PreviousNewlineChar[str_String, pos_Integer] '' given a string and a position in that string, returns the previous newline character's position, or None if none.
	
	Example:

	PreviousNewlineChar["abc\ndef ", 7] === 4
	
	\maintainer danielb
*)
PreviousNewlineChar[str_String, pos_Integer] :=
	Module[{retVal = None},
		Do[
			With[{char = StringTake[str, {i}]},
				If [StringMatchQ[char, RegularExpression["[\n\r]"]],
					retVal = i;
					Return[];
				];
			]
			,
			{i, pos - 1, 1, -1}
		];
		retVal
	]

(*!
	\function NextNewlineChar
	
	\calltable
		NextNewlineChar[str_String, pos_Integer] '' given a string and a position in that string, returns the next newline character's position, or None if none.
	
	Example:

	NextNewlineChar["abc\ndef ", 1] === 4
	
	\maintainer danielb
*)
NextNewlineChar[str_String, pos_Integer] :=
	Module[{retVal = None},
		Do[
			With[{char = StringTake[str, {i}]},
				If [StringMatchQ[char, RegularExpression["[\n\r]"]],
					retVal = i;
					Return[];
				];
			]
			,
			{i, pos + 1, StringLength[str]}
		];
		retVal
	]

(*!
	\function CopyFunctionUI
	
	\calltable
		CopyFunctionUI[func, destContext] '' user interface for copying a function and any of its dependencies to a destination context/file.

	Examples:
	
	CopyFunctionUI[func, destContext] === TODO
	
	\related '
	
	\maintainer danielb
*)
CopyFunctionUI::cfdc = "Couldn't find file `1` for symbol `2` with context `3`";
CopyFunctionUI::cffdc = "Couldn't find destination file `1` for context `2`";
CopyFunctionUI[func_, destContext_] :=
	Block[{destFile, file, dependencies, otherFuncs, context, destContext2},
		
		destFile = FindFile[StringReplace[destContext, "Private`" ~~ EndOfString :> ""]];
		If [destFile === $Failed, Message[CopyFunctionUI::cfdc, file, destContext]; Return[$Failed, Block]];
		
		dependencies = FunctionDependencies[func];
		dependencies = DeleteDuplicates[Prepend[dependencies[[All, 2]], func]];
		
		(* For each dependency, look up its file. *)
		dependencies =
		Function[{dependency},
			With[{sym = dependency},
				context = Context[sym];
				context = StringReplace[context, "Private`" ~~ EndOfString :> ""];
				file = FindFile[context];
				If [file === $Failed, Message[CopyFunctionUI::cfdc, file, sym, context]; Return[$Failed, Block]];
				destContext2 = destContext;
				If [StringMatchQ[Context[sym], __ ~~ "`Private`" ~~ EndOfString], destContext2 = destContext2 <> "Private`"];
				If [Names[destContext2 <> SymbolName[sym]] =!= {} &&
					With[{sym2 = ToExpression[destContext2 <> SymbolName[sym]]}, DownValues[sym2] =!= {}],
					
					Print["Skipped: ", SymbolName[sym]];
					Sequence @@ {}
					,
					<|"Symbol" -> dependency, "File" -> file|>
				]
			]
		] /@ dependencies;
		
		With[{destFile2 = destFile},
		ProcessOneByOne[
			dependencies,
			"TitleFunction" ->
				Function[{dependency},
					ToString[dependency["Symbol"]]
				],
			"Function" ->
				Function[{dependency},
					Column[
						{
						dependency["File"],
						"",
						Row[
							{
							Button[
								"Copy",
								With[{sym = dependency["Symbol"]},
									destContext2 = destContext;
									If [StringMatchQ[Context[sym], __ ~~ "`Private`" ~~ EndOfString], destContext2 = destContext2 <> "Private`"];
									CopyFunction[dependency["Symbol"], dependency["File"], destFile2, "DestContext" -> destContext];
									OpenFileInWorkbench[destFile2, "Substring" -> SymbolName[sym] ~~ "[" ~~ Except[{"\n", "\r"}].. ~~ ":="];
								],
								ImageSize -> {100, 34}
							],
							" ",
							Button[
								"Source",
								With[{sym = dependency["Symbol"], sourceFile = dependency["File"]},
									OpenFileInWorkbench[sourceFile, "Substring" -> SymbolName[sym] ~~ "[" ~~ Except[{"\n", "\r"}].. ~~ ":="]
								],
								ImageSize -> {100, 34}
							]
							}
						],
						"",
						GetFunctionSource[dependency["Symbol"], "File" -> dependency["File"]]
						}
					]
				]
		]
		]
	];

(*!
	\function FunctionDependencies
	
	\calltable
		FunctionDependencies[funcSymbol] '' given a function symbol, returns a list of rules that represent the functions it calls, and then in turn the functions they call.
	
	\maintainer danielb
*)
Attributes[FunctionDependencies] = {HoldFirst};
Clear[FunctionDependencies];
Options[FunctionDependencies] =
{
	"MaxDepth" -> Infinity,	 (*< The maximum dependency depth to explore. *)
	"CurrentDepth" -> 1,		(*< The current depth being explored. Would be better as an option of a helper function. *)
	"BasicContextsToPrune" ->   (*< Some contexts like JLink` are essentially system contexts that we typically don't want to probe into. *)
		{
			"JLink`",
			"NETLink`"
		},
	"ContextsToPrune" ->		(*< Like "BasicContextsToPrune", but user specified. *)
		{
		},
	"LimitToContext" -> None,   (*< Can be used to limit the search within a certain context. *)
	"AlreadyExplored" -> {}	 (*< Functions already explored. Don't re-explore. *)
};
FunctionDependencies[funcSymbol_, opts:OptionsPattern[]] :=
	Block[{$functionDependenciesConsidered = Append[OptionValue["AlreadyExplored"], {funcSymbol}]},
		functionDependenciesHelper[funcSymbol, opts]
	];

Options[functionDependenciesHelper] = Options[FunctionDependencies];
functionDependenciesHelper[funcSymbol_, opts:OptionsPattern[]] :=
	Module[{immediateDependencies, dependencies, innerDependencies,
			contextsToPrune = Join[OptionValue["ContextsToPrune"], OptionValue["BasicContextsToPrune"]]},
		
		$functionDependenciesConsidered = Append[$functionDependenciesConsidered, funcSymbol];
		
		If [OptionValue["CurrentDepth"] > OptionValue["MaxDepth"],
			Return[{}, Module];
		];
		
		immediateDependencies =
			ImmediateFunctionDependencies[
				funcSymbol,
				FilterOptions[ImmediateFunctionDependencies, opts]
			];
			
		(* Drop any symbols with a context that matches the OptionValue["ContextsToPrune"],
		   or symbols that don't match OptionValue["LimitToContext"]. *)
		immediateDependencies =
			Select[
				immediateDependencies,
				Function[
					With[
						{sym = #1},
						(OptionValue["LimitToContext"] === None || StringStartsQ[Context[sym], OptionValue["LimitToContext"]]) &&
						And @@ ( !StringStartsQ[Context[sym], #1] & ) /@ contextsToPrune
					]
				]
			];
			
		dependencies =
			Function[{dependency},
				
				If [MemberQ[$functionDependenciesConsidered, dependency],
					innerDependencies = Sequence @@ {};
					,
					innerDependencies =
						functionDependenciesHelper[
							dependency,
							"CurrentDepth" -> OptionValue["CurrentDepth"] + 1,
							Sequence @@
								DeleteCases[{opts}, HoldPattern[Rule]["CurrentDepth", _]]
						];
				];
						
				{
					funcSymbol -> dependency,
					innerDependencies
				}
			] /@ immediateDependencies;
			
		DeleteDuplicates[
			Flatten[dependencies]
		]
	];

FilterOptions[head_Symbol, opts___] := Sequence @@ FilterRules[{opts}, Options[head]];

(*!
	\function ImmediateFunctionDependencies
	
	\calltable
		ImmediateFunctionDependencies[funcSymbol] '' given a function symbol, returns a list of rules that represent the functions it calls.

	NOTE: This function is quite brittle because it avoid treating any reference to
		  a symbol-with-downvalues as a function call. The reason it avoids that is
		  that it isn't uncommon to use function symbols in other ways, such as
		  Throw[$Failed, myFuncSymbol], which aren't actually function calls.
		  This function tries to make up for it by explicitly looking for
		  uses of Map, Apply, etc, which consititute function calls, but that
		  will always be a brittle approach.

	Examples:
	
	ImmediateFunctionDependencies[GetVariablePossiblyFromParentPackage] === {ContextContainsSymbol}
	
	\maintainer danielb
*)
Attributes[ImmediateFunctionDependencies] = {HoldFirst};
Clear[ImmediateFunctionDependencies];
Options[ImmediateFunctionDependencies] =
{
	"BasicContextsToPrune" ->   (*< Some contexts like JLink` are essentially system contexts that we typically don't want to probe into. *)
		{
			"JLink`",
			"NETLink`"
		},
	"ContextsToPrune" ->		(*< Like "BasicContextsToPrune", but user specified. *)
		{
		},
	"LimitToContext" -> None	(*< Can be used to limit the search within a certain context. *)
};
ImmediateFunctionDependencies[funcSymbol_, opts:OptionsPattern[]] :=
	Module[{downValues, symbols, res,
			contextsToPrune = Join[OptionValue["ContextsToPrune"], OptionValue["BasicContextsToPrune"]]},
		
		downValues =
			Replace[
				(* Some symbols are read-protected, so if that's the case,
				   quiet the message and return {}. *)
				Check[Quiet[DownValues[funcSymbol]], {}],
				HoldPattern[RuleDelayed][_, rhs_] :> HoldComplete[rhs],
				{1}
			];
		
		symbols =
			Cases[
				downValues,
				(sym_Symbol)[___] |
				HoldPattern[Map | Scan | Apply | MapThread | Inner | Outer | Operate | Nest | NestWhile | NestWhileList | NestList | NestGraph][sym_Symbol, ___] |
				HoldPattern[Inner][_, _, _, sym_Symbol] |
				HoldPattern[Distribute | Through][__, sym_Symbol, ___] |
				HoldPattern[Through][_[___, sym_Symbol, ___][_]] |
				HoldPattern[Level][_, _, sym_Symbol]
					(* Get rid of things like Symbol["blah" <> ToString[myInteger]],
					   which aren't actually instances of symbols but rather code
					   that can produce a symbol. *)
					/; !MatchQ[HoldComplete[sym], HoldComplete[Symbol[_]] | Symbol] :>
						HoldComplete[sym],
				Infinity,
				Heads -> True
			];
			
		symbols = DeleteDuplicates[symbols];
		
		res =
		ReleaseHold /@
			DeleteDuplicates @
			Select[
				symbols,
				(
					# /. HoldComplete[sym_] :>
						(
							Context[sym] =!= "System`" && 
							(OptionValue["LimitToContext"] === None || StringStartsQ[Context[sym], OptionValue["LimitToContext"]]) &&
							And @@ ( !StringStartsQ[Context[sym], #1] & ) /@ contextsToPrune &&
							DownValues[sym] =!= {}
						)
				) &
			];
			
		(* If a function recurses on itself, let's not report that explicitly. *)
		res =
		DeleteCases[
			res,
			funcSymbol
		];
		
		res
	];

(*!
	\function ProcessOneByOne
	
	\calltable
		ProcessOneByOne[list_List] '' creates a simple UI that can be used to process a list of items one by one. Updates the global variable Global`$item with the next item.
	
	Example:
	
	ProcessOneByOne[{"one", "two", "three"}]
	
	\maintainer danielb
*)
Options[ProcessOneByOne] =
{
	"Function" -> None,				(*< A function that will be run for each time. *)
	"TitleFunction" -> None,		   (*< A function that will be given an item and is expected to produce a title. *)
	"StartIndex" -> Automatic		  (*< The item to start at, or the file that should be used to store/retrieve the start index. *)
};
ProcessOneByOne[list_List, OptionsPattern[]] :=
	DynamicModule[{startIndex, startIndexFile = None},
		
		If [OptionValue["StartIndex"] =!= Automatic,
			startIndex = OptionValue["StartIndex"];
			If [StringQ[startIndex],
				(* We were given a file name instead. *)
				startIndexFile = OptionValue["StartIndex"];
				startIndex = If [FileExistsQ[startIndexFile], Get[startIndexFile], 1];
				(* Incase the file was bad. *)
				If [!IntegerQ[startIndex], startIndex = 1];
			];
			If [startIndex > Length[list],
				startIndex = Length[list];
			];
			,
			(* If a start index wasn't provided, then see if we can guess what
			   the start index was the last time this list of inputs was passed
			   to us. *)
			If [IntegerQ[Global`$itemIndex] &&
				ListQ[$prevProcessOneByOneInputs] &&
				Length[$prevProcessOneByOneInputs] >= 3 &&
				Length[list] >= 3 &&
				$prevProcessOneByOneInputs[[1;;3]] === list[[1;;3]],
				
				startIndex = Global`$itemIndex;
				,
				startIndex = 1;
			];
		];
		
		$prevProcessOneByOneInputs = list;
		
		If [Length[list] === 0,
			Print["No items in list."];
			Return[$Failed];
		];
		
		Global`$itemIndex = startIndex;
		
		Global`$item = list[[Global`$itemIndex]];
		
		Framed[
			Column[
				{
					Dynamic[
						Style[
							If [OptionValue["TitleFunction"] === None,
								If [ListQ[list[[Global`$itemIndex]]],
									Column[DeleteCases[list[[Global`$itemIndex]], HoldPattern[Rule][_, _] | {}]],
									list[[Global`$itemIndex]]
								],
								OptionValue["TitleFunction"][list[[Global`$itemIndex]]]
							],
							{"Text", FontSize -> 20}
						]
						,
						TrackedSymbols :> {Global`$itemIndex}
					]
					,
					Column[
						{
							Row[
								{
								Button[
									"<<",
									(
										Global`$itemIndex = 1;
										Global`$item = list[[Global`$itemIndex]];
									)
									,
									ImageSize -> {40, 36}
								]
								,
								" "
								,
								Button[
									"Prev",
									(
										If [Global`$itemIndex > 1,
											--Global`$itemIndex;
											Global`$item = list[[Global`$itemIndex]];
											,
											Print["At first item."];
										];
									)
									,
									ImageSize -> {100, 36}
								]
								
								,
								" "
								,
								Button[
									"Next",
									(
										If [Global`$itemIndex < Length[list],
											++Global`$itemIndex;
											Global`$item = list[[Global`$itemIndex]];
											,
											Print["No more items."];
										];
									)
									,
									ImageSize -> {200, 36}
								]
								,
								" "
								,
								
								Button[
									">>",
									(
										Global`$itemIndex = Length[list];
										Global`$item = list[[Global`$itemIndex]];
									)
									,
									ImageSize -> {40, 36}
								]
								}
							]
							,
							Dynamic[
								(* Save the item index. We piggy back on the UI code that listens for changes to Global`$itemIndex. *)
								If [startIndexFile =!= None,
									Put[Global`$itemIndex, startIndexFile];
								];
								Framed[#, FrameStyle -> None, FrameMargins -> Medium] & @
								Row[{ProgressIndicator[Global`$itemIndex / Length[list]], " (", ToString[Global`$itemIndex], "/", Length[list], ")"}]
								,
								TrackedSymbols :> {Global`$itemIndex}
							]
							,
							Dynamic[
								If [OptionValue["Function"] === None,
									""
									,
									Column[{"", OptionValue["Function"][list[[Global`$itemIndex]], "InputNum" -> Global`$itemIndex]}]
								]
								,
								TrackedSymbols :> {Global`$itemIndex}
							]
						}
					]
				}
				,
				Spacings -> {2, 2}
			]
			,
			FrameMargins -> 16
			,
			ImageSize -> {{800, 6000}, {10000, 100}}
		]
	]

(*!
	\function OpenFileInWorkbench
	
	\calltable
		OpenFileInWorkbench[file] '' given a file, opens it in Workbench. The file must be in a project defined in Global`$WorkbenchProjects.
	
	This requires that the "Workbench Helpers" Eclipse plugin has been installed.
	
	Examples:
	
	OpenFileInWorkbench[FindFile["WUtils`WUtils`"]]
	
	\maintainer danielb
*)
Clear[OpenFileInWorkbench];
Options[OpenFileInWorkbench] =
{
	"Line" -> Automatic,	(*< what line number to place the cursor at. *)
	"Substring" -> None	 (*< can specify either a substring or pattern to try and locate in the file. *)
};
$workbenchHelpersPluginUrl = "http://127.0.0.1:8193/go";
OpenFileInWorkbench[fileIn_, OptionsPattern[]] :=
	Module[{response, url, file = fileIn, line = OptionValue["Line"]},
		
		If [line === Automatic && OptionValue["Substring"] =!= None,
			line = GetLineNumberOfStringInFile[OptionValue["Substring"], file];
			
			If [line === None,
				Print["OpenFileInWorkbench: Couldn't find ", ToString[OptionValue["Substring"], InputForm], " in file ", file];
				Return[$Failed];
			];
		];		
		
		file = getPathRelativeToWorkbenchProjects[file];
		
		If [file === $Failed, Print["OpenFileInWorkbench: Couldn't make path relative to known Workbench projects."]; Return[$Failed]];
		
		url = $workbenchHelpersPluginUrl <> "?command=open&file=" <> file;
		
		If [line =!= Automatic,
			url = url <> "&line=" <> ToString[line];
		];
		
		response = Import[url, "String"];
		If [!StringQ[response] || (response =!= "OK" && StringFreeQ[response, "Done"]),
			Print["OpenFileInWorkbench: Unexpected response from " <> url <> ": ", response];
			$Failed
			,
			Null
		]
	]

(*!
	\function GetLineNumberOfStringInFile
	
	\calltable
		GetLineNumberOfStringInFile[str, file] '' given a string (or string pattern), returns the line number in the given file that it occurs on, or None if not found.
	
	Examples:
	
	GetLineNumberOfStringInFile[
		StartOfLine ~~ "GetLineNumberOfStringInFile[",
		SymbolToFile[GetLineNumberOfStringInFile]
	]
	
	\related 'GetLineNumberOfString 'OpenFileInWorkbench
	
	\maintainer danielb
*)
GetLineNumberOfStringInFile[str_, file_] :=
	Module[{},
		
		If [!FileExistsQ[file], Print["Missing file: " <> file]; Return[$Failed]];
		
		GetLineNumberOfString[
			Import[file, "String"],
			str
		]
	]

(*!
	\function GetLineNumberOfString
	
	\calltable
		GetLineNumberOfString[str, substring] '' given a string and a substring, returns the line number of the substring. The substring can also be a string pattern. Returns None if no matches.
	
	Examples:
	
	GetLineNumber["abc\ndef", "e"] === 2

	Unit tests:

	RunUnitTests[GetLineNumberOfString]

	\maintainer danielb
*)
GetLineNumberOfString[str_String, substring_] :=
	Module[{pos},
		pos = StringPosition[str, substring, 1];
		
		If [pos === {},
			None
			,
			GetLineNumberOfPos[str, pos[[1, 1]]]
		]
	]

(*!
	\function GetLineNumberOfPos
	
	\calltable
		GetLineNumberOfPos[str, pos] '' given a string and a position, returns the line number of the position in the string.
	
	Examples:
	
	GetLineNumber["abc\ndef", 5] === 2

	\maintainer danielb
*)
GetLineNumberOfPos[str_String, pos_Integer] :=
	Length[
		StringCases[
			StringTake[str, {1, pos}],
			"\n"
		]
	]

(*!
	\function getPathRelativeToWorkbenchProjects
	
	\calltable
		getPathRelativeToWorkbenchProjects[path] '' given a path to a file, make it relative to the Wolfram Workbench projects. The file must be in a project defined in Global`$WorkbenchProjects.

	Unit tests: getPathRelativeToWorkbenchProjects.mt

	\maintainer danielb
*)
getPathRelativeToWorkbenchProjects[pathIn_] :=
	Module[{path = pathIn, rest, res = $Failed, projectPath, projectName},
		
		If [ListQ[Global`$WorkbenchProjects],
			Function[{project},
				
				If [MatchQ[project, _Rule],
					projectPath = project[[1]];
					projectName = project[[2]];
					,
					projectPath = project;
					projectName = FileNameTake[projectPath, -1];
				];
				If [StringMatchQ[path, projectPath ~~ __],
					If [StringMatchQ[projectPath, __ ~~ ("/" | "\\")],
						projectPath = StringTake[projectPath, {1, -2}];
					];
					rest = StringTake[path, StringLength[projectPath] + 2 ;;];
					rest = StringReplace[rest, StartOfString ~~ $PathnameSeparator.. :> ""];
					res = "/" <> projectName <> "/" <> rest;
					(* Jump out of the inner function back into the main function/module. *)
					Return[];
				];
			] /@ Global`$WorkbenchProjects;
		];
		
		If [res =!= $Failed,
			StringReplace[res, "\\" :> "/"]
			,
			res
		]
	]

(*!
	\function CreateReloadFunctionForDirectory
	
	\calltable
		CreateReloadFunctionForDirectory[dir] '' given a directory containing .m files, creates and returns a reload function that can be run to reload changes to those .m files. Running the function only reloads files if they have changed since the last time the function was run.

	- Scan for .m files.
	- Compute dependencies betweeen .m files.
	- Produce and return a function that can be used as a function to reload
	  any changes to files.

	Examples:
	
	myReloadFunc =
	CreateReloadFunctionForDirectory[
		FileNameDrop[FindFile["CalculateParse`Prototype`VirtualAssistant`Utility`"], -1]
	];
	myReloadFunc[]
	
	\related 'ComputeDependencyGraph
	
	\maintainer danielb
*)
Clear[CreateReloadFunctionForDirectory];
Options[CreateReloadFunctionForDirectory] =
{
	"MaxDepth" -> 1,								(*< how many directories deep to look for .m files? *)
	"LoadFileFunction" -> Get,					  (*< the function used to load a .m file. For the new package format, GeneralUtilities`GetFragment would be used here. *)
	"AutoExportDirectoryReloadFunction" -> None,	(*< the MachineLearning package doesn't yet support adding new exported or packaged scoped symbols and then calling GetFragment, so we support calling a reload function that is a bigger hammer in those cases. *)
	"AdditionalDependencies" -> {}					(*< in addition to scanning the directory for dependencies, a list of custom dependencies can be passed in. *)
};
CreateReloadFunctionForDirectory[dir_, OptionsPattern[]] :=
	Module[{mFiles, dependencies, memoizationFunc = CreateMemoizationFunction[],
			files, prevReloadTimestamp, reloadFunc,
			maxDepth = OptionValue["MaxDepth"], prevFiles,
			reloadFunctionCreationTimestamp, modifiedFiles, exports,
			reloadFunctionToSendToAutoExport,
			additionalDependencies = OptionValue["AdditionalDependencies"]},
		
		mFiles = FileNames["*.m", dir, maxDepth];
		
		dependencies =
			ComputeDependencyGraph[
				mFiles,
				"Directories" -> {dir},
				(* Use memoization so that if we need to make
				   calls to things such as FindFile again, we
				   won't have to recompute. *)
				"Memoization" -> memoizationFunc,
				"Files" -> True
			];
			
		dependencies = Join[dependencies, additionalDependencies];
		
		reloadFunctionCreationTimestamp = Date[];
		
		With[{loadFileFunction = OptionValue["LoadFileFunction"]},
		
		reloadFunc =
		Function[
			
			(* Re-scan for .m files. It's possible that new files were created. *)
				
			files = FileNames["*.m", dir, maxDepth];
			files = Join[files, Cases[additionalDependencies, _String, 2]];
			
			If [ListQ[prevFiles] && files =!= prevFiles,

				(* New filese detected. *)
				Print["New files detected. Recomputing dependencies..."];
				
				memoizationFunc = CreateMemoizationFunction[];
				
				dependencies =
					ComputeDependencyGraph[
						files,
						"Directories" -> {dir},
						(* Use memoization so that if we need to make
						   calls to things such as FindFile again, we
						   won't have to recompute. *)
						"Memoization" -> memoizationFunc,
						"Files" -> True
					];
				
				dependencies = Join[dependencies, additionalDependencies];
			];
			
			prevFiles = files;
			
			(* Only load those files that have changed since we last loaded them. *)
			modifiedFiles =
				Flatten[Reap[
					Function[{file},
						If [(* We will only reload a file if it was altered AFTER this reload
							   function was generated. That will prevent the initial run of
							   the reload function from reloading everything. *)
							(DateDifference[reloadFunctionCreationTimestamp, FileDate[file, "Modification"]] /. Quantity[d_, "Days"] :> d) > 0 &&
							(
								(* Either not yet reloaded. *)
								!MatchQ[prevReloadTimestamp[file], _DateObject | _List] ||
								(* Or reloaded, but prior to the last modification. *)
								(DateDifference[prevReloadTimestamp[file], FileDate[file, "Modification"]] /. Quantity[d_, "Days"] :> d) > 0
							),
							
							Sow[file]
						]
					] /@ files;
				][[2]], 1];
			
			(* Some of the modified files may not have yet been
			   loaded, and if not, we probably shouldn't load
			   them, since we're advertising ourselves as a
			   "reloading" function. (?)
			   
			   On the flip side if someone modifies a file, what
			   are the chances they DON'T want it reloaded, even
			   if they have yet to load it manually? hmm. I suppose
			   for now we'll leave this unimplemented.
			   
			   Yet to be hooked up. If we wanted to look it up,
			   we'd need more code to use modifiedFilesThatAppearToHaveBeenPreviouslyLoaded
			   appropriately. *)
			(*
			modifiedFilesThatAppearToHaveBeenPreviouslyLoaded =
				Select[
					modifiedFiles,
					Memoized[
						AppearsToBePreviouslyLoadedMFile[#],
						memoizationFunc
					] &
				];
			*)
			
			(* Expand the file list wrt dependencies, and order properly wrt
			   dependencies. *)
			With[{expandedFilesList = ProcessFileDependencies[modifiedFiles, dependencies]},
				(* But we only really need to reload a dependant file
				   if the dependant was changed. Be careful to maintain
				   the order, since that's the core value proposition of
				   ProcessFileDependencies. *)
				modifiedFiles =
					Select[
						expandedFilesList,
						MemberQ[modifiedFiles, #] &
					]
			];
			
			reloadFunctionToSendToAutoExport =
				If [OptionValue["AutoExportDirectoryReloadFunction"] =!= None,
					None
					,
					loadFileFunction
				];
			
			(* Reload the files. *)
			exports = Flatten[Reap[
			Function[{file},
				
				Monitor[			
					PreReloadFile[file];
					
					Quiet[
						loadFileFunction[file],
						(* Dec 2015: Quiet the messages that result from GetFragment until Tali can hopefully resolve them. *)
						Attributes::notfound
					];
					
					PostReloadFile[file, reloadFunctionToSendToAutoExport];
					,
					Framed[
						Style[
							Row[{"Reloading ", FileNameTake[file, -1]}],
							FontFamily -> "Arial"
						],
						Background -> RGBColor[247/255, 247/255, 251/255],
						FrameMargins -> 15,
						FrameStyle -> Directive[GrayLevel[0.7]]
					]
				];
				
				(* Update the timestamp AFTER PostReloadFile, since
				   PostReloadFile may perform an AutoExport, thereby
				   modifying the file again and reloading it. *)
				prevReloadTimestamp[file] = FileDate[file, "Modification"];
				
			] /@ modifiedFiles;,
			"AutoExportedFunction"][[2]], 1];
			
			If [exports =!= {} && OptionValue["AutoExportDirectoryReloadFunction"] =!= None,
				ReleaseHold[OptionValue["AutoExportDirectoryReloadFunction"]];
			];
		];
		
		];
		
		reloadFunc
	]

(*!
	\function ComputeDependencyGraph
	
	\calltable
		ComputeDependencyGraph[file] '' given a WL file, analyzes uses of Needs/Get to compute a dependency graph. Recursive. If the .m file isn't a WL package, returns $Failed.

	Examples:
	
	ComputeDependencyGraph[FindFile["WUtils`WUtils`"]]
	
	\related 'ComputeDependencyGraphForPackage
	
	\maintainer danielb
*)
Clear[ComputeDependencyGraph];
Options[ComputeDependencyGraph] =
{
	"Directories" -> All,		   (*< a list can be specified that means that only packages that are within one of the directories should be considered. (subdirectories are allowed) *)
	"Memoization" -> None,		  (*< can be set to a symbol to turn on memoization. Used in combination with the listable down value of ComputeDependencyGraph to avoid duplicate processing of files. *)
	"Files" -> False				(*< return dependencies in terms of files, rather than packages. *)
};
ComputeDependencyGraph[file_String, OptionsPattern[]] :=
	Module[{res},
		res =
		Memoized[
			computeDependencyGraphHelper[
				file,
				"Directories" -> OptionValue["Directories"],
				"Memoization" -> OptionValue["Memoization"]
			],
			OptionValue["Memoization"],
			"MemoizationKey" -> {"computeDependencyGraphHelper", file}
		];
		
		If [TrueQ[OptionValue["Files"]],
			replacePackagesWithFiles[res, OptionValue["Memoization"]]
			,
			res
		]
	]

ComputeDependencyGraph[files_List, OptionsPattern[]] :=
	Module[{
				res,
				memoizationFunc =
					If[OptionValue["Memoization"] === None,
						CreateMemoizationFunction[]
						,
						OptionValue["Memoization"]
					]
		   },
		
		res =
		DeleteDuplicates[
			DeleteCases[
				Flatten[
					Map[
						Function[
							ComputeDependencyGraph[
								#,
								"Memoization" -> memoizationFunc,
								"Directories" -> OptionValue["Directories"]
							]
						],
						files
					]
				],
				(* If any of the .m files weren't actually packages, then
				   they'll result in $Failed items. *)
				$Failed
			]
		];
		
		If [TrueQ[OptionValue["Files"]],
			replacePackagesWithFiles[res, memoizationFunc]
			,
			res
		]
	]

(*!
	\function computeDependencyGraphHelper
	
	\calltable
		computeDependencyGraphHelper[file] '' given a WL file, analyzes uses of Needs/Get to compute a dependency graph. Recursive. If the .m file isn't a WL package, returns $Failed.
	
	\related 'computeDependencyGraphHelperForPackage
	
	\maintainer danielb
*)
Clear[computeDependencyGraphHelper];
Options[computeDependencyGraphHelper] =
{
	"FilesAlreadyProcessed" -> Null,		(*< a held variable can be passed in to keep track of which files have been visited already. *)
	"Directories" -> All,				   (*< a list can be specified that means that only packages that are within one of the directories should be considered. (subdirectories are allowed) *)
	"Memoization" -> None				   (*< can be set to a symbol to turn on memoization. Used in combination with the listable down value of ComputeDependencyGraph to avoid duplicate processing of files. *)
};
computeDependencyGraphHelper[file_, opts:OptionsPattern[]] :=
	Module[{needsUses, package, dependencies, filesAlreadyProcessed, dataStr, readListData},
		
		(* If a list of directories was given that we want to limit
		   our search to be within, then make sure we have't wandered
		   outside. *)
		If [OptionValue["Directories"] =!= All &&
			!StringStartsWith[file, OptionValue["Directories"]],
			Return[{}];
		];
		
		(* Use a symbol to keep track of which files have
		   been processed so that recursive calls don't
		   process a .m file that has already been processed.
		   ie. A dependency tree can have duplicate nodes
		   which are in different branches. *)
		filesAlreadyProcessed = CreateHeldVarIfNull[OptionValue["FilesAlreadyProcessed"], {}];
		
		(* If we've already processed this file, don't recurse. *)
		If [MemberQ[ReleaseHold[filesAlreadyProcessed], file],
			Return[{}];
		];
		
		dataStr = Import[file, "Text"];
		
		(* Get the package name of this .m file, because we want to skip .m files
		   that aren't actually packages. *)
		package = GetPackageName[file, "DataString" -> dataStr];
		If [package === None, Return[$Failed]];
		
		(* Read the .m file. *)
		With[{readListData = readListData},
			ReadListFileHeld[
				StringToStream[dataStr],
				needsUses = needsUsesHelper[readListData],
				readListData,
				"Package" -> package
			];
		];
		
		(* If we're only interested in files within certain directories, then
		   limit the Needs statements to those ones. *)
		If [OptionValue["Directories"] =!= All,
			needsUses =
				Select[
					needsUses,
					(
						With[{
								needsFilePath =
									Memoized[
										FindFile[#],
										OptionValue["Memoization"]
									]
							 },
							StringQ[needsFilePath] && StringStartsWith[needsFilePath, OptionValue["Directories"]]
						]
					) &
				]
		];
		
		dependencies = (package -> #) & /@ needsUses;
		
		(* Mark this file as having been processed. *)
		SetHeldVar[
			filesAlreadyProcessed,
			Append[ReleaseHold[filesAlreadyProcessed], file]
		];
		
		Flatten[
			AppendTo[
				dependencies,
				Memoized[
					computeDependencyGraphForPackageHelper[
						#,
						"FilesAlreadyProcessed" -> filesAlreadyProcessed,
						"Directories" -> OptionValue["Directories"],
						"Memoization" -> OptionValue["Memoization"]
					],
					OptionValue["Memoization"],
					"MemoizationKey" -> {"computeDependencyGraphForPackageHelper", #}
				] & /@ needsUses
			]
		]
	]

(*!
	\function StringStartsWith
	
	\calltable
		StringStartsWith[str, stringList] '' returns True if the given string starts with one of the strings in 'stringList'.

	Examples:
	
	StringStartsWith["abcdef", {"abc"}] === True

	Unit tests:

	RunUnitTests[WUtils`WUtils`StringStartsWith]

	\maintainer danielb
*)
StringStartsWith[str_, stringList_] :=
	Module[{strLen = StringLength[str]},
		Function[{item},
			If [strLen >= StringLength[item] &&
				StringTake[str, StringLength[item]] === item,
				Return[True, Module]
			]
		] /@ stringList;
		
		False
	]

(*!
	\function CreateHeldVarIfNull
	
	\calltable
		CreateHeldVarIfNull[val, defaultValue] '' if the argument passed in is Null, then a held var is created and returned. Otherwise, the argument is echoed. Useful for helper functions that can accept a held var as an option, but where the initial call would pass in Null by default.

	Examples:
	
	CreateHeldVarIfNull[Null] === HoldComplete[WUtils`WUtils`Private`NewVar`heldVar1]
	
	CreateHeldVarIfNull[HoldComplete[WUtils`WUtils`Private`NewVar`heldVar2]] === HoldComplete[WUtils`WUtils`Private`NewVar`heldVar2]

	Unit tests:

	RunUnitTests[WUtils`WUtils`CreateHeldVarIfNull]

	\related 'NewHeldVar 'SetHeldVar
	
	\maintainer danielb
*)
CreateHeldVarIfNull[val_, defaultValue_:"$NotSpecified$"] :=
	If [val === Null,
		If [defaultValue =!= "$NotSpecified$",
			With[{var = NewHeldVar["heldVar"]},
				SetHeldVar[var, defaultValue];
				var
			]
			,
			NewHeldVar["heldVar"]
		]
		,
		val
	]

(*!
	\function NewHeldVar
	
	\calltable
		NewHeldVariable[baseName] '' creates a new variable and wraps it in HoldComplete. This can act as a reference to that variable, rather than referring to its actual value.
	
	\related 'SetHeldVar
	
	\maintainer danielb
*)
NewHeldVar[baseName_] :=
	With[{var = Unique["WUtils`WUtils`Private`NewVar`" <> baseName]},
		HoldComplete[var]
	]

(*!
	\function SetHeldVar
	
	\calltable
		SetHeldVar[heldVar, value] '' update the value of the variable to the given value. The variable is held. This is a way to keep a reference to a variable as opposed to its value.
	
	Examples:
	
	SetHeldVar[HoldComplete[myVar], 1];
	
	myVar === 1
	
	\related 'DynamicOutputSectionVar
	
	\maintainer danielb
*)
SetHeldVar[heldVar_, value_] :=
	Module[{},
		heldVar /. HoldComplete[var_] :>
			(
			var = value;
			)
	]

(*!
	\function GetPackageName
	
	\calltable
		GetPackageName[file] '' given some WL code, returns the name of the package used in the BeginPackage statement, or None if none.
	
	Because this is such an expensive operation, GetPackageName uses Memoization so that
	subsequent calls are sure not to redo work.
	
	We would prefer to use ReadList to read the WL file as a list of HeldComplete expressions
	and use Cases to find BeginPackage, but that appears to pollute the symbol namespace
	because ReadList doesn't set $Context properly, and thus any symbols defined by the
	file get put into Global`.
	
	Example:

	GetPackageName["BeginPackage[\"WUtils`WUtils`\"];\n...\n"] === "WUtils`WUtils`"

	Unit tests:

	RunUnitTests[WUtils`WUtils`Private`GetPackageName]

	\maintainer danielb
*)
Clear[GetPackageName];
Options[GetPackageName] = Options[getPackageNameHelper] =
{
	"DataString" -> Null,	   (*< the file's contents as a string. If not passed in, it will be read from disk. *)
	"UseMemoization" -> True	(*< memoize results? *)
};
If [!ValueQ[$GetPackageNameMemoization],
	$GetPackageNameMemoization = CreateMemoizationFunction[];
];

GetPackageName[file_, opts:OptionsPattern[]] :=
	(
	Memoized[
		getPackageNameHelper[file, opts],
		If [TrueQ[OptionValue["UseMemoization"]],
			$GetPackageNameMemoization
			,
			None
		],
		"MemoizationKey" -> file
	]
	)

getPackageNameHelper[file_, OptionsPattern[]] :=
	Module[{matches, dataStr},
		
		If [OptionValue["DataString"] =!= Null,
			dataStr = OptionValue["DataString"];
			,
			dataStr = Import[file, "Text"];
		];
		
		matches =
			ToExpression /@
			Select[
				StringCases[
					dataStr,
					("BeginPackage" | "Package") ~~ WhitespaceCharacter... ~~
					"[" ~~ WhitespaceCharacter... ~~
					package:doubleQuotedStringPattern[] ~~ WhitespaceCharacter... ~~
					"]" :>
					   package
				],
				StringMatchQ[
					#,
					"\"" ~~ Repeated[WLSymbolPattern[] ~~ "`"] ~~ "\""
				] &
			];
			
		If [matches === {},
			None
			,
			matches[[1]]
		]
	]

(* String pattern for a WL symbol. *)
WLSymbolPattern[] = WordBoundary ~~ ("$" | LetterCharacter) ~~ ("$" | "`" | LetterCharacter | DigitCharacter)... ~~ WordBoundary;

(*!
	\function ReadListFileHeld
	
	\calltable
		ReadListFileHeld[fileOrStream, expr, listOfHeldExpressionsSymbol] '' given a file name or stream, reads the expressions in that file one by one and wraps them in HoldComplete, and then evalutes 'expr'. Cleans up any symbols that got inadvertantly defined. The listOfHeldExpressionsSymbol symbol is replaced within 'expr' with the data returned from ReadList before evaluating 'expr'.

	See also:
	
	https://mail-archive.wolfram.com/archive/l-kernel/2015/Mar00/0018.html

	Examples:
	
	With[{myVar = Unique["myVar"]},
		ReadListFileHeld[
			FindFile["WUtils`WUtils`"],
			(* Code to be executed. *)
			Print[thisVarWillHoldTheListOfHeldExpressions[[1;;5]] // Indent2],
			thisVarWillHoldTheListOfHeldExpressions
		]
	]
	
	\related 'ReadList
	
	\maintainer danielb
*)
Attributes[ReadListFileHeld] = {HoldRest};
Clear[ReadListFileHeld];
Options[ReadListFileHeld] =
{
	"Package" -> Null		   (*< If known, $Context can be set to the package's context so that symbols get the right context. *)
};
ReadListFileHeld[fileOrStream_, expr_, listOfHeldExpressionsSymbol_, OptionsPattern[]] :=
	Module[{newSymbols = {}, res},
		
		Block[{$Context =
					If [OptionValue["Package"] =!= Null,
						OptionValue["Package"] <> "`ReadListFileHeld`Private`"
						,
						$Context
					]
			  },
		
			newSymbols =
				Flatten[
					Reap[
						Block[{$NewSymbol = (Sow[#]&), data},
							
							(* Quiet'ing this for now since it sometimes seems to
							   result in shadow messages. The 'Remove' that we do
							   removes any symbols that we inadvertantly create, so
							   we're not particularily interested in shadow messages.
							   Not sure how to turn off ONLY shadow messages, so doing
							   a full Quiet, which seems a bit unfortunate. *)
							Quiet[
								data = ReadList[fileOrStream, HoldComplete[Expression]];
							];
							
							res =
							ReleaseHold[
								Replace[
									HoldComplete[
										expr
									],
									listOfHeldExpressionsSymbol -> data,
									Infinity
								]
							];
						]
					][[2]],
				 1
			];
			 
			Quiet[
				Remove & /@ newSymbols,
				(* I'm seeing some cases where symbols are reported as created,
				   but when we remove them, M complains they don't exist. *)
				Remove::rmnsm
			];
		];
		
		res
	]

(*!
	\function ProcessFileDependencies
	
	\calltable
		ProcessFileDependencies[files, dependencies] '' given a list of files to be reloaded, and a list of file dependencies, expand the list of files as necessary, and sort the list so that a dependent file is loaded prior to a depending file.
	
	Example:
	
	ProcessFileDependencies[
		{
			"C:\\Temp\\a.m",
			"C:\\Temp\\b.m",
			"C:\\Temp\\c.m"
		},
		{
			"a.m" -> "b.m",
			"b.m" -> "c.m"
		}
	]
	
	===
	
	{
		"C:\\Temp\\c.m",
		"C:\\Temp\\b.m",
		"C:\\Temp\\a.m"
	}
	
	\related 'dependenciesOfFile
	
	\maintainer danielb
*)
Clear[ProcessFileDependencies];
ProcessFileDependencies[files_, dependencies_] :=
	Module[{res, dependentList, isDependent},
		
		res = files;
		
		(* Get the dependencies of each file. *)
		Function[{file},
			dependentList = dependentsOfFile[file, dependencies];
			(* Populate a isDependent function for use in sorting. *)
			(isDependent[file, #] = True) & /@ dependentList;
			
			res = {res, dependentList};
			
			Function[{file2},
				(isDependent[file2, #] = True) & /@ dependentsOfFile[file2, dependencies];
			] /@ dependentList;
			
		] /@ files;
		
		res = DeleteDuplicates[Flatten[res]];
		
		isDependent[_, _] := False;
		
		(*Print[DownValues[isDependent] // Indent2];*)
		
		Sort[
			res,
			isDependent[#1, #2] &
		]
	]

(*!
	\function dependentsOfFile
	
	\calltable
		dependentsOfFile[file, dependencies] '' given a file and known dependency relationships, return the dependents of the given file.
	
	Doesn't go beyond a max depth to somewhat protect against circular dependencies.
	
	Example:
	
	dependentsOfFile[
		"C:\\Temp\\c.m"
		,
		{
			"C:\\Temp\\a.m" -> "C:\\Temp\\b.m",
			"C:\\Temp\\b.m" -> "C:\\Temp\\c.m"
		}
	]
	
	===
	
	{"C:\\Temp\\b.m", "C:\\Temp\\a.m"}
	
	\related 'ProcessFileDependencies
	
	\maintainer danielb
*)
Clear[dependentsOfFile];
dependentsOfFile[file_, dependencies_, depth_:0] :=
	Module[{res, foundDependents, nonDirectDependents = {}},
		
		If [depth > 15,
			Return[{}];
		];
		
		res = {};
		
		foundDependents = DeleteDuplicates[Select[dependencies, #[[2]] === file &][[All, 1]]];

		(* Dependencies of dependencies *)
		Function[{foundDependency},
			nonDirectDependents = Join[nonDirectDependents, dependentsOfFile[foundDependency, dependencies, depth + 1]];
		] /@ foundDependents;
		
		DeleteCases[Join[foundDependents, nonDirectDependents], file]
	]

(*!
	\function PreReloadFile
	
	\calltable
		PreReloadFile[file] '' called prior to reloading a file.
		
	Allows code to be run that wants to have the opportunity of
	doing something prior to code reloads.
	
	\related 'PostReloadFile
	
	\maintainer danielb
*)
PreReloadFile[file_] :=
	Module[{},
		(* If enabled, record the symbol names in a file prior to
		   reloading so that after reloading we can look for new
		   function definitions that should be exported. *)
		If [TrueQ[Global`$EnableAutoExport],
			(* Only consider files modified recently. Ideally,
			   we'd only like to perform this logic for files
			   modified by the user (not by a cvs update), so
			   this will help a bit. We also want to avoid
			   performing this logic after an Alpha restart
			   whereby there are files newer than the MX build
			   date, but which we don't care to run this logic
			   on upon reload. *)
			If [ModificationAgeInMinutes[file] < 10,
				With[{context = FileToContext[file, "PrivateContext" -> True]},
					If [context =!= $Failed &&
						(* Only bother to record the names if we
						   haven't already done that, since upon
						   PostReloadFile we updated this. *)
						recordedContextNames[context] === Null,
						(* Names seems to take on the order of 100 ms which
						   seems much slower than I would have imagined. It's
						   sad to add 100 ms to the speed of reloading
						   a file. Hopefully the benefit of not having to manually
						   export functions is worth it. (and of course this is
						   only enabled if someone sets Global`$EnableAutoExport = True. *)
						recordedContextNames[context] = Names[context <> "*"];
					];
				];
			]; 
		];
	]

(*!
	\function ModificationAgeInMinutes
	
	\calltable
		ModificationAgeInMinutes[file] '' given a file, returns how long ago it was last modified, in seconds.
	
	Examples:
	
	ModificationAgeInMinutes[FindFile["WUtils`WUtils`"]]
	
	\maintainer danielb
*)
ModificationAgeInMinutes[file_] := DateDifference[FileDate[file, "Modification"], Date[], "Minute"]

(*!
	\function FileToContext
	
	\calltable
		FileToContext[file] '' given a file, returns its expected context.
	
	Note: Slow because it reads the whole file.
	
	Examples:
	
	FileToContext[FindFile["WUtils`WUtils`"]]
	
	===
	
	"WUtils`WUtils`"
	
	\maintainer danielb
*)
Clear[FileToContext];
Options[FileToContext] =
{
	"PrivateContext" -> False	   (*< If True, returns the private context for the file. *)
};
Clear[fileToContextCache];
FileToContext[file_, OptionsPattern[]] :=
	Module[{context = $Failed, private = TrueQ[OptionValue["PrivateContext"]]},
		
		(* Rather than re-reading a file each time FileToContext is called, we'll
		   cache the result. *)
		With[{cachedRes = fileToContextCache[file, private]},
			If [cachedRes =!= Null,
				Return[cachedRes];
			];
		];
		
		If [!FileExistsQ[file], Print["Missing file: " <> file]; Return[$Failed]];
		
		context = GetPackageName[file, "DataString" -> Import[file, "Text"]];
		If [context === None, Return[None]];
		
		If [private,
			If [NewPackageFormatQ[file],
				(* But this doesn't always appear to be correct. For example,
				   in MachineLearning, the intermediate context path is
				   of the form "file$..." or something. *)
				context = context <> FileBaseName[file] <> "`PackagePrivate`";
				,
				context = context <> "Private`";
			];
		];
		
		fileToContextCache[file, private] = context;
		
		context
	]

(* Rather than re-reading a file each time FileToContext is called, we'll
   cache the result. *)
fileToContextCache[_, _] := Null;

recordedContextNames[_] := Null;

(*!
	\function PostReloadFile
	
	\calltable
		PostReloadFile[file, reloadFunction] '' called after reloading a file.
		
	Allows code to be run that wants to have the opportunity of
	doing something after code reloads.
	
	\related 'PreReloadFile
	
	\maintainer danielb
*)
Clear[PostReloadFile];
PostReloadFile[file_, reloadFunction_:Get] :=
	Module[{},
		
		(* If enabled, look for new function definitions that should be exported. *)
		If [TrueQ[Global`$EnableAutoExport],
			(* Only consider files modified recently. Ideally,
			   we'd only like to perform this logic for files
			   modified by the user (not by a cvs update), so
			   this will help a bit. *)
			If [ModificationAgeInMinutes[file] < 10,
				With[{context = FileToContext[file, "PrivateContext" -> True]},
					If [context =!= $Failed,
						With[{prevNames = recordedContextNames[context]},
							If [ListQ[prevNames],
								With[{names = Names[context <> "*"]},
									
									(* Update our cache since we did the work
									   of calling Names, which is expensive.
									   This avoids PreReloadFile from needing
									   to refresh the cache each time. *)
									recordedContextNames[context] = names;
									
									With[{newNames = Complement[names, prevNames]},
										processNewlyDefinedPrivateSymbols[newNames, file, reloadFunction];
									]
								]
							]
						]
					]
				]
			]; 
		];
		
	]

(*!
	\function processNewlyDefinedPrivateSymbols
	
	\calltable
		processNewlyDefinedPrivateSymbols[newNames, file, reloadFunction] '' given a list of newly defined private symbol names, looks for newly defined functions that start with a capital letter, and adds Exports for them.
	
	Examples:
	
	processNewlyDefinedPrivateSymbols[
		{
		"WUtils`WUtils`MyNewFunc",
		"WUtils`WUtils`$1322"
		},
		"MyFile.m",
		Get
	]
	
	\related 'PostReloadFile
	
	\maintainer danielb
*)
processNewlyDefinedPrivateSymbols[newNames_, file_, reloadFunction_] :=
	Module[{symbolsToExport},
		
		symbolsToExport = nameListToExportedSymbolList[newNames];
			
		Function[{symbol},
			Print["AutoExport: ", SymbolName[symbol]];
			
			ExportSymbol[symbol, file];
			
			(* Not sure whether I should do SymbolToFile[symbol], or use
			   the 'file' passed in. Unclear whether the new package format
			   likes it when you reload indidivual non-main files, or whether
			   it wants to reload the whole package at once. *)
			reloadFunction[file];
			
			(* Signal the reload function that something was AutoExported. *)
			Sow[True, "AutoExportedFunction"];
			
			(*Get[SymbolToFile[symbol]];*)
		] /@ symbolsToExport
	]

(*!
	\function nameListToExportedSymbolList
	
	\calltable
		nameListToExportedSymbolList[names] '' given a list of names, return the ones that look like they are intended to be exported functions.
	
	Examples:
	
	(* Note that this would only return the given value if
	   WUtils`WUtils`ModificationAgeInMinutes were
	   actually defined and had DownValues. *)
	
	nameListToExportedSymbolList[
		{
		"WUtils`WUtils`ModificationAgeInMinutes",
		"WUtils`WUtils`nameListToExportedSymbolList"
		}
	]
	
	===
	
	{
		WUtils`WUtils`ModificationAgeInMinutes
	}
	
	\related 'processNewlyDefinedPrivateSymbols
	
	\maintainer danielb
*)
nameListToExportedSymbolList[names_] :=
	ToExpression /@
		Select[
			names,
			With[{name = #},
				With[{symbolName = StringTakeByDelim[name, "`", -1]},
					With[{expression = ToExpression[name, StandardForm, HoldComplete]},
						MatchQ[expression, HoldComplete[_Symbol]] &&
						(* Starts with a capital letter? *)
						StringMatchQ[symbolName, firstLetter:LetterCharacter ~~ (LetterCharacter | DigitCharacter)... /; UpperCaseQ[firstLetter]] &&
						(* First check that there aren't any OwnValues. We don't want
						   symbols that have OwnValues. *)
						(expression /. HoldComplete[symbol_] :> OwnValues[symbol]) === {} &&
						(* Check for DownValues. If it's a function, it should have some. *)
						With[{symbol = ReleaseHold[expression]}, DownValues[symbol] =!= {}] &&
						(* Exported symbol of the same name doesn't yet exist? *)
						Names[StringReplace[name,  "`PackagePrivate" | "`Private" :> ""]] === {}
					]
				]
			] &
		]

(*!
	\function StringTakeByDelim
	
	\calltable
		StringTakeByDelim[str, delim, n] '' StringTake, but by portions demarkated by a delimiter.
	
	Example:
	
	StringTakeByDelim["WUtils`WUtils`Private`MyNewFunc", "`", -1] === "MyNewFunc"
	
	\maintainer danielb
*)
StringTakeByDelim[str_, delim_, n_] :=
	StringJoin[Riffle[Take[StringSplit[str, delim], n], delim]]

(*!
	\function replacePackagesWithFiles
	
	\calltable
		replacePackagesWithFiles[e, memoizationFunc] '' given an expression, replaces all strings, which are each assumed to be a package name, with the corresponding file name. If the memoizationFunc is not None, then it is used to avoid duplicate calls to FindFile, and can re-use previously done calls to FindFile associated with the memoizationFunc.

	Examples:
	
	`Private`replacePackagesWithFiles[
		{
			"WUtils`WUtils`",
			"NewContext`"
		},
		None
	]
	
	\related 'ComputeDependencyGraph
	
	\maintainer danielb
*)
replacePackagesWithFiles[e_, memoizationFunc_] :=
	e /. package_String :>
		Memoized[
			FindFile[package],
			memoizationFunc
		]

(*!
	\function needsUsesHelper
	
	\calltable
		needsUsesHelper[heldExpressions] '' given some WL code, finds all uses of Needs and returns the corresponding list of packages.

	Examples:
	
	WUtils`WUtils`Private`needsUsesHelper[
		{
		HoldComplete[Needs["MyPackage`"]]
		}
	]
	
	===
	
	{"MyPackage`"}

	Unit tests:

	RunUnitTests[WUtils`WUtils`Private`needsUsesHelper]

	\maintainer danielb
*)
needsUsesHelper[heldExpressions_] :=
	DeleteDuplicates[
		Cases[
			heldExpressions,
			HoldPattern[Needs | Global`PackageImport][package_String] :> package,
			(* Only consider top-level uses of Needs to avoid Needs if they
			   are within an If, for example. That underscores a limitation of
			   this function, which is that loading a package normally can
			   dynamically choose which of its Needs to to evaluate, whereas
			   we've decided to ignore dynamically choosen Needs statements. *)
			(* The reason we use {2, 3} and not {1} is that each expression is
			   wrapped in HoldComplete, and sometimes within CompoundExpression if
			   there is a semi-colon after the Needs. *)
			{2, 3}
		]
	]

(*!
	\function computeDependencyGraphForPackageHelper
	
	\calltable
		computeDependencyGraphForPackageHelper[package] '' given a WL package, analyzes uses of Needs/Get to compute a dependency graph. Recursive. If the .m file can't be found, or if the .m file isn't a WL package, returns $Failed.
	
	\related 'ComputeDependencyGraph
	
	\maintainer danielb
*)
Clear[computeDependencyGraphForPackageHelper];
Options[computeDependencyGraphForPackageHelper] =
{
	"FilesAlreadyProcessed" -> Null,		(*< a held variable can be passed in to keep track of which files have been visited already. *)
	"Directories" -> All,				   (*< a list can be specified that means that only packages that are within one of the directories should be considered. (subdirectories are allowed) *)
	"Memoization" -> None				   (*< can be set to a symbol to turn on memoization. Used in combination with the listable down value of ComputeDependencyGraph to avoid duplicate processing of files. *)
};
computeDependencyGraphForPackageHelper[package_, opts:OptionsPattern[]] :=
	Module[{file},
		
		file = Memoized[FindFile[package], OptionValue["Memoization"]];
		
		If [file === $Failed,
			Print["Couldn't find the file for package: ", InputForm[package]];
			Return[$Failed]
		];
		
		Memoized[
			computeDependencyGraphHelper[file, opts],
			OptionValue["Memoization"],
			"MemoizationKey" -> {"computeDependencyGraphHelper", file}
		]
		
	]

End[]

EndPackage[]