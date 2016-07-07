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
		CalculateParse`GeneralLibrary`Private`MyNewFunc
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

	RunUnitTests[CalculateParse`GeneralLibrary`NewPackageFormatQ]

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
			AbsoluteTiming[Memoized[FindFile["CalculateParse`GeneralLibrary`"], memoizationSymbol]][[1]],
			AbsoluteTiming[Memoized[FindFile["CalculateParse`GeneralLibrary`"], memoizationSymbol]][[1]],
			AbsoluteTiming[Memoized[FindFile["CalculateParse`GeneralLibrary`"], memoizationSymbol]][[1]],
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
	Block[{destFile, file, dependencies, otherFuncs, context},
		
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
				<|"Symbol" -> dependency, "File" -> file|>
			]
		] /@ dependencies;
		
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
						With[{destFile = destFile},
							Button[
								"Copy",
								CopyFunction[dependency["Symbol"], dependency["File"], destFile],
								ImageSize -> {100, 34}
							]
						],
						"",
						GetFunctionSource[dependency["Symbol"], "File" -> dependency["File"]]
						}
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

End[]

EndPackage[]