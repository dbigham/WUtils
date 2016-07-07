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

CreateUnitTests::usage = "CreateUnitTests  "

GetFunctionUsesFromNotebook::usage = "GetFunctionUsesFromNotebook  "

GetCodeCellObjects::usage = "GetCodeCellObjects  "

GetCodeCellExpressions::usage = "GetCodeCellExpressions  "

CodeCellBoxesToHeldExpressions::usage = "CodeCellBoxesToHeldExpressions  "

RowBoxesToString::usage = "RowBoxesToString  "

Indent3::usage = "Indent3  "

ToExpressionPreservingComments::usage = "ToExpressionPreservingComments  "

ReplaceCommentsWithExpressions::usage = "ReplaceCommentsWithExpressions  "

ListOfHeldToHeldList::usage = "ListOfHeldToHeldList  "

HeldListToListOfHeld::usage = "HeldListToListOfHeld  "

DistributeHeldCompoundExpressions::usage = "DistributeHeldCompoundExpressions  "

GetFunctionUsesFromNotebookHelper::usage = "GetFunctionUsesFromNotebookHelper  "

UnitTestFilename::usage = "UnitTestFilename  "

SymbolToFile::usage = "SymbolToFile  "

ContextToFile::usage = "ContextToFile  "

StringDropByDelim::usage = "StringDropByDelim  "

UnitTestDirectory::usage = "UnitTestDirectory  "

GetVariablePossiblyFromParentPackage::usage = "GetVariablePossiblyFromParentPackage  "

ContextContainsSymbol::usage = "ContextContainsSymbol  "

DynamicOutputSectionVar::usage = "DynamicOutputSectionVar  "

EnsureUnitTestFileExists::usage = "EnsureUnitTestFileExists  "

UnitTestTemplate::usage = "UnitTestTemplate  "

MakePathRelativeToPaths::usage = "MakePathRelativeToPaths  "

Username::usage = "Username  "

FunctionCallsTestedByFile::usage = "FunctionCallsTestedByFile  "

testIDSymbol::usage = "testIDSymbol  "

KeepRuleIfNotSequence::usage = "KeepRuleIfNotSequence  "

TestIDsInTestFile::usage = "TestIDsInTestFile  "

GetTestUiMetadata::usage = "GetTestUiMetadata  "

GetPreviousCell::usage = "GetPreviousCell  "

TabsOrSpaces::usage = "TabsOrSpaces  "

LooksLikeCallSignature::usage = "LooksLikeCallSignature  "

GetExpectedExpressionAndPossiblyModifyHeldFunctionCall::usage = "GetExpectedExpressionAndPossiblyModifyHeldFunctionCall  "

EvaluateAndGetMessages::usage = "EvaluateAndGetMessages  "

RemoveHoldFromIndentedString::usage = "RemoveHoldFromIndentedString  "

AddTestToFile::usage = "AddTestToFile  "

CreateTest::usage = "CreateTest  "

RandomDigitsOrLetters::usage = "RandomDigitsOrLetters  "

SetHeldVarKeyValue::usage = "SetHeldVarKeyValue  "

MakeExpectedValueSystemIndependent::usage = "MakeExpectedValueSystemIndependent  "

ListOfHeldMessagesToString::usage = "ListOfHeldMessagesToString  "

AddCellID::usage = "AddCellID  "

GetCodeCell::usage = "GetCodeCell  "

GetHeldVarKeyValue::usage = "GetHeldVarKeyValue  "

NotebookSubTest::usage = "NotebookSubTest  "

GetCellMetadata::usage = "GetCellMetadata  "

InsertTestCell::usage = "InsertTestCell  "

CellObjectToTest::usage = "CellObjectToTest  "

UpdateTest::usage = "UpdateTest  "

GetNearestPrecedingSpan::usage = "GetNearestPrecedingSpan  "

GetNearestTrailingSpan::usage = "GetNearestTrailingSpan  "

StartsWithComment::usage = "StartsWithComment  "

RedirectPrintsAndMessagesToDynamicOutputSection::usage = "RedirectPrintsAndMessagesToDynamicOutputSection  "

DynamicOutputSectionPrint::usage = "DynamicOutputSectionPrint  "

SmartButton::usage = "SmartButton  "

GroupTwoCells::usage = "GroupTwoCells  "

EditFunctionMathdoc::usage = "EditFunctionMathdoc  "

GetFunctionMathdoc::usage = "GetFunctionMathdoc  "

GetPackageSource::usage = "GetPackageSource  "

MathdocContainsExampleQ::usage = "MathdocContainsExampleQ  "

GetNextLine::usage = "GetNextLine  "

GetLineAtPos::usage = "GetLineAtPos  "

SetMathdocExample::usage = "SetMathdocExample  "

ExampleToString::usage = "ExampleToString  "

EditFunctionMathdocHelper::usage = "EditFunctionMathdocHelper  "

RemoveRelatedIfNotUsed::usage = "RemoveRelatedIfNotUsed  "

StringReplaceInFiles::usage = "StringReplaceInFiles  "

RunUnitTestsInNotebook::usage = "RunUnitTestsInNotebook  "

GetCodeCellObjectsContaining::usage = "GetCodeCellObjectsContaining  "

CodeCellObjectToExpression::usage = "CodeCellObjectToExpression  "

DynamicOutputSection::usage = "DynamicOutputSection  "

Indent2::usage = "Indent2  "

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
			XPrint[nextTokenPos, ": ", StringTake[remainder, nextTokenPos][[1]]];
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
doubleQuotedStringPattern[] := ("\"" ~~ RepeatedNull["\\\\" | "\\\"" | Except["\""]] ~~ "\"");

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
			(* Since some files have multiple instances of End[]. *)
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
Clear[CopyFunctionUI];
Options[CopyFunctionUI] =
{
	"SymbolsToPrune" ->			(*< Dependencies that should be ignored. *)
		{
		}
};
CopyFunctionUI[func_, destContext_, OptionsPattern[]] :=
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
$contextsToPruneByDefault =
	{
		"JLink`",
		"NETLink`",
		"FrontEnd`",
		"DataPaclets`",
		"CalculateScan`",
		"GeneralUtilities`",
		"CalculateParse`Expr`",
		"CalculateParse`GrammarSyntax`",
		"CalculateParse`TemplateParser4`",
		"CalculateParse`Content`",
		"CalculateParse`ParseTabularInput`",
		"CalculateParse`ParseTIUtils`",
		"CalculateParse`JavaTokenizer`",
		"CalculateParse`GlobalParseData`",
		"CalculateParse`ExportedFunctions`",
		"CalculateParse`Preprocessor1`",
		"CalculateParse`Private`",
		"CalculateParse`Parser1`",
		"CalculateParse`PLI`",
		"CalculateParse`Prototype`VirtualAssistant`FastGrammar`",
		"CalculateParse`Prototype`VirtualAssistant`VaSemantics`",
		"CalculateLoader`",
		"Tests`Utilities`ParserTestingTools`",
		"CalculateUtilities`",
		"DataScience`Utils`",
		"MUnit`",
		"Compile`"
	};
Options[FunctionDependencies] =
{
	"MaxDepth" -> Infinity,		(*< The maximum dependency depth to explore. *)
	"CurrentDepth" -> 1,		(*< The current depth being explored. Would be better as an option of a helper function. *)
	"BasicContextsToPrune" ->	(*< Some contexts like JLink` are essentially system contexts that we typically don't want to probe into. *)
		$contextsToPruneByDefault,
	"ContextsToPrune" ->		(*< Like "BasicContextsToPrune", but user specified. *)
		{
		},
	"SymbolsToPrune" ->			(*< Dependencies that should be ignored. *)
		{
		},
	"LimitToContext" -> None,	(*< Can be used to limit the search within a certain context. *)
	"AlreadyExplored" -> {}		(*< Functions already explored. Don't re-explore. *)
};
FunctionDependencies[funcSymbol_, opts:OptionsPattern[]] :=
	Block[{$functionDependenciesConsidered = Append[OptionValue["AlreadyExplored"], {funcSymbol}]},
		Global`djb = {};
		functionDependenciesHelper[funcSymbol, opts]
	];

Options[functionDependenciesHelper] = Options[FunctionDependencies];
functionDependenciesHelper[funcSymbol_, opts:OptionsPattern[]] :=
	Module[{immediateDependencies, dependencies, innerDependencies,
			contextsToPrune = Join[OptionValue["ContextsToPrune"], OptionValue["BasicContextsToPrune"]],
			symbolsToPrune = OptionValue["SymbolsToPrune"]},
		
		$functionDependenciesConsidered = Append[$functionDependenciesConsidered, funcSymbol];
		Global`djb = DeleteDuplicates[Append[Global`djb, Context[funcSymbol]]];
		If [OptionValue["CurrentDepth"] > OptionValue["MaxDepth"],
			Return[{}, Module];
		];
		
		immediateDependencies =
			ImmediateFunctionDependencies[
				funcSymbol,
				FilterOptions[ImmediateFunctionDependencies, opts]
			];
		
		(* Drop any symbols with a context that matches the OptionValue["ContextsToPrune"],
		   or symbols that don't match OptionValue["LimitToContext"] or symbols in
		   OptionValue["SymbolsToPrune"]. (although I'm confused why we're doing this here,
		   because don't we also do this in ImmediateFunctionDependencies?) *)
		immediateDependencies =
			Select[
				immediateDependencies,
				Function[
					With[
						{sym = #1},
						(OptionValue["LimitToContext"] === None || StringStartsQ[Context[sym], OptionValue["LimitToContext"]]) &&
						With[{tmp = And @@ ( !StringStartsQ[Context[sym], #1] & ) /@ contextsToPrune && !MemberQ[symbolsToPrune, sym]},
							If [!TrueQ[tmp] && DownValues[sym] =!= {},
								Print["Skipping: ", sym];
							];
							tmp
						]
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
	"BasicContextsToPrune" ->	(*< Some contexts like JLink` are essentially system contexts that we typically don't want to probe into. *)
		$contextsToPruneByDefault,
	"ContextsToPrune" ->		(*< Like "BasicContextsToPrune", but user specified. *)
		{
		},
	"SymbolsToPrune" ->			(*< Dependencies that should be ignored. *)
		{
		},
	"LimitToContext" -> None	(*< Can be used to limit the search within a certain context. *)
};
ImmediateFunctionDependencies[funcSymbol_, opts:OptionsPattern[]] :=
	Module[{downValues, symbols, res,
			contextsToPrune = Join[OptionValue["ContextsToPrune"], OptionValue["BasicContextsToPrune"]],
			symbolsToPrune = OptionValue["SymbolsToPrune"]},
		
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
							With[{tmp = And @@ ( !StringStartsQ[Context[sym], #1] & ) /@ contextsToPrune && !MemberQ[symbolsToPrune, sym]},
								If [!TrueQ[tmp] && DownValues[sym] =!= {},
									Print["Skipping: ", sym];
								];
								tmp
							] &&
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
			   this will help a bit. *)
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

(*!
	\function CreateUnitTests
	
	\calltable
		CreateUnitTests[funcSymbol] '' reads the current notebook's code cells and looks for uses of the given function. It then creates unit tests in an appropriate file.
	
	\related 'UnitTestDirectory 'GetFunctionUsesFromNotebook
	
	\maintainer danielb
*)
Clear[CreateUnitTests];
Options[CreateUnitTests] =
{
	"SubTest" -> None,					(*< Add the test to a sub-test file. *)
	"RunTests" -> True					(*< Run the tests after adding any tests? *)
};
CreateUnitTests[funcSymbol_Symbol, OptionsPattern[]] :=
	Module[{funcUses, file, funcUsesTested, missingFuncUses, str, expectedExpression,
			numTestsAdded = 0, testNum, alreadyTested, heldFunctionCall, knownTestId,
			firstTestProcessed, firstTestExpression, firstTestExpectedExpression,
			debugFlag = False, context = Context[funcSymbol], useTabs, messages},
	
		funcUses = GetFunctionUsesFromNotebook[funcSymbol, "IncludeAdditionalTestExpressions" -> True];
		
		If [TrueQ[debugFlag],
			Print["funcUses:"];
			Print[funcUses // Indent2];
		];
		
		If [funcUses === {},
			Print["CreateUnitTests: Couldn't find any uses of the function ", ToString@funcSymbol, " in Code cells in current notebook."];
			Return[$Failed];
		];
		
		file = UnitTestFilename[funcSymbol, "SubTest" -> OptionValue["SubTest"]];
		
		If [file === $Failed, Return[$Failed]];

		If [EnsureUnitTestFileExists[funcSymbol, file] === $Failed,
			Return[$Failed];
		];
		
		funcUsesTested = FunctionCallsTestedByFile[funcSymbol, file];
		
		(* Use this approach for the complement to preserve order. We do this
		   so that:
		   1) The test further down for testNum === 1 works as expected.
		   2) Otherwise it's disorienting to create tests in scrambled order
			  wrt what was in the notebook. *)
		(* Disabled for now, since sometimes we want to create test notebooks
		   that test the same thing multiple times but in different contexts.
		   Let's see if the test ID check below is strong enough to avoid
		   duplicates. *)
		(*alreadyTested[_] := False;
		(* Use ToString because for some reason, without it, the down values
		   don't match as I would think they should. *)
		(alreadyTested[ToString[#[[1]]]] = True) & /@ funcUsesTested;*)
		
		(* Already existant TestIDs *)
		knownTestId[_] := False;
		Function[{testId},
			knownTestId[testId] = True
		] /@ TestIDsInTestFile[file];
		
		missingFuncUses =
			Select[
				funcUses,
				(
					(* Not a known test ID. *)
					!TrueQ[knownTestId[Gett[GetTestUiMetadata[Gett[#, "CellObject"]], "TestId"]]] &&
					(* This expression isn't already in the test file. *)
					!TrueQ[alreadyTested[ToString[#[[1]]]]]
				)
				&
			];
			
		If [TrueQ[debugFlag],
			Print["funcUsesTested:"];
			Print[funcUsesTested // Indent2];
			Print["missingFuncUses:"];
			Print[missingFuncUses // Indent2];
		];
		
		XReturn[None, Module];
		
		testNum = 0;
		
		useTabs = (TabsOrSpaces[context] === "Tabs");
		
		Function[{funcUse},
			
			heldFunctionCall = funcUse[[1]];
			
			++testNum;
	   
			(* Try and avoid creating tests for the code cell that is
			   meant simply to be the call signature. (extracted from
			   the Mathdoc) *)
			If [testNum === 1 &&
				LooksLikeCallSignature[heldFunctionCall, funcSymbol],
				
				(* Ignore test. It looks like a call signature, not an
				   actual call. *)
				Null
				
				,
				messages =
					EvaluateAndGetMessages[
						{
							expectedExpression,
							heldFunctionCall
						} =
							GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[heldFunctionCall]
					][[2]];
				
				str = RemoveHoldFromIndentedString[Indent2[heldFunctionCall, "RemoveContexts" -> False]];
				
				FunctionUse[heldFunctionCall, Sequence @@ funcUse[[2;;]]] /.
					FunctionUse[HoldComplete[e_], args___] :>
						(
						If [StringPosition[str, "\n"] =!= {},
							Print["Adding test:\n", str];
							,
							Print["Adding test: ", str];
						];
						++numTestsAdded;
						
						If [!TrueQ[firstTestProcessed],
							
							firstTestExpression = HoldComplete[e];
							firstTestExpectedExpression = expectedExpression;
							
							firstTestProcessed = True;
						];
						
						AddTestToFile[
							 e,
							 file,
							 "Expected" -> expectedExpression,
							 "ExpectedMessages" -> messages,
							 "FunctionSymbol" -> funcSymbol,
							 "UseTabs" -> useTabs,
							 
							 (* Pass along arguments like "Comment". *)
							 Sequence @@
								 FilterRules[
									 {args},
									 Options[AddTestToFile]
								 ]
							 
							 (* So that uses of the function symbol in
								the test file aren't fully qualified.
								A Needs is added to the test file so
								that full qualification isn't necessary. *)
							 (* But the Needs approach doesn't seem to be
								working, so we'll fall back to fully qualified
								symbols. *)
							 (*"Contexts" -> {Context[funcSymbol]}*)
						];
						)
			];
			
		] /@ missingFuncUses;
		
		If [numTestsAdded > 0,
			(* Edit the function's MathDoc, if it exists, to indicate that
			   it has unit tests, and how to run them. *)
			EditFunctionMathdoc[
				funcSymbol,
				firstTestExpression,
				firstTestExpectedExpression
			];
			
			If [TrueQ[OptionValue["RunTests"]],
				(* Also run the unit tests. Not sure if we should run these in
				   a way that causes them to show up in the dynamic output section,
				   or to run just evaluate the cell in the notebook that contains
				   the call to RunUnitTests. Putting the output in the dynamic output
				   section is slightly odd in that you might want to dismiss the other
				   output in that section via the "OK" button, but then you loose your
				   test results. On the other hand, if smoeone uses CreateUnitTests
				   in a notebook that wasn't dynamically generated, and thus doesn't have
				   a test-running section, then calling RunUnitTestsInNotebook would be
				   futile. We could perhaps detect whether that section isn't present
				   and fall back to running them in the dynamic output section in that
				   case... ? *)
				(*Print[RunUnitTests[funcSymbol]];*)
				RunUnitTestsInNotebook[funcSymbol];
			];
			,
			Print["No new tests found."];
		]
	]

(*!
	\function GetFunctionUsesFromNotebook
	
	\calltable
		GetFunctionUsesFromNotebook[funcSymbol] '' reads the current notebook's code cells and looks for uses of the given function.
		
	Examples:
	
	Code cell:
	
	(* Test comment *)
	myFunc[1, 2, 3]
	
	(* Given a notebook with the above code cell. *)
	GetFunctionUsesFromNotebook[myFunc]
	
	===
	
	{
		FunctionUse[
			HoldComplete[myFunc[1, 2, 3]],
			"Comment" -> "Test comment"
		]
	}
	
	\maintainer danielb
*)
Clear[GetFunctionUsesFromNotebook];
Options[GetFunctionUsesFromNotebook] =
{
	"IncludeAdditionalTestExpressions" -> False			(*< Should other test expressions such as TestHead[...] be included? *)
};
GetFunctionUsesFromNotebook[funcSymbol_, opts:OptionsPattern[]] :=
	Module[{codeCellExpressions, codeCellObjects},
		
		codeCellObjects = GetCodeCellObjects[];
		
		codeCellExpressions = GetCodeCellExpressions[codeCellObjects];
		
		GetFunctionUsesFromNotebookHelper[funcSymbol, codeCellExpressions, opts]
	]

(*!
	\function GetCodeCellObjects
	
	\calltable
		GetCodeCellObjects[] '' returns the CellObjects in the current notebook that are code cells.
	\related '
	
	\maintainer danielb
*)
Clear[GetCodeCellObjects];
Options[GetCodeCellObjects] =
{
	"Notebook" -> Automatic			(*< the notebook from which to get the code cells. *)
};
GetCodeCellObjects[OptionsPattern[]] :=
	Module[{nb = OptionValue["Notebook"]},
		If [OptionValue["Notebook"] === Automatic,
			nb = InputNotebook[];
		];
		Select[Cells[nb], MatchQ[NotebookRead[#], Cell[BoxData[___], "Code", ___]] &]
	]

(*!
	\function GetCodeCellExpressions
	
	\calltable
		GetCodeCellExpressions[codeCellObjects] '' returns the expressions inside of code cells.
	
	Each returned item is either of the form:
	
	HoldComplete[cellExpression_]
	
	or:
	
	{
		{HoldComplete[firstLineExpression_], cellObject1},
		{HoldComplete[seondLineExpression_], cellObject2},
		...
	}
	
	\maintainer danielb
*)
Clear[GetCodeCellExpressions];
GetCodeCellExpressions[codeCellObjects_] :=
	Module[{codeCellBoxes, codeCellBox, codeCellExpression},
		
		codeCellBoxes =
			Function[{codeCellObject},
				
				codeCellBox =
					Replace[
						NotebookRead[codeCellObject],
						Cell[BoxData[c_], ___] :> c,
						{0, 1}
					];
				 
				If [codeCellBox === $Failed,
					Print["Failed to read code cell: ", codeCellObject];
					
					Sequence @@ {}
					,
					codeCellExpression = CodeCellBoxesToHeldExpressions[codeCellBox];
					
					{
						codeCellExpression,
						codeCellObject
					}
				]
					
			] /@ codeCellObjects
	]

(*!
	\function CodeCellBoxesToHeldExpressions
	
	\calltable
		CodeCellBoxesToHeldExpressions[codeCellBoxes] '' given the boxes gotten from a code cell in a notebook, convert it into a list of held expressions.
	
	\maintainer danielb
*)
CodeCellBoxesToHeldExpressions[codeCellBoxes_] :=
	Module[{},
		DistributeHeldCompoundExpressions[
			HeldListToListOfHeld[
				ToExpressionPreservingComments[
					(* Using Quiet because otherwise if I have a checkbox in any
					   code cells, RowBoxesToString prints noisey errors. *)
					RowBoxesToString[codeCellBoxes, "Quiet" -> True] /. $Failed :> ""
					,
					"HoldResult" -> True
					
				] /. HoldPattern[Rule]["Comment", Global`Comment[c_]] :> "Comment"[c]
			]
		]
	]

(*!
	\function RowBoxesToString
	
	\calltable
		RowBoxesToString[rowBoxes] '' given nested RowBoxes, converts them to a flat string.
	
	Examples:
	
	RowBoxesToString[
		RowBox[{"(*"," ",RowBox[{"Test"," ",RowBox[{"comment","."}]}]," ","*)"}]
	]
	
	===
	
	"(* Test comment. *)"

	Unit tests:

	RunUnitTests[WUtils`WUtils`RowBoxesToString]

	\related 'GetCodeCellExpressions
	
	\maintainer danielb
*)
Clear[RowBoxesToString];
Options[RowBoxesToString] =
{
	"Quiet" -> False
};
RowBoxesToString[rowBoxes_RowBox, OptionsPattern[]] :=
	Module[{str},
		str =
			ReplaceRepeated[
				rowBoxes,
				RowBox[{strs___String}] :> StringJoin[strs]
			];
		
		If [!StringQ[str],
			If [!TrueQ[OptionValue["Quiet"]],
				Print["Error: RowBoxesToString: Failed to convert to string:\n", Indent2[rowBoxes], "\nResult was:\n", Indent2[str]];
			];
			Return[$Failed];
		];
		
		str
	]
	
RowBoxesToString[{rowBoxes___}, opts:OptionsPattern[]] := StringJoin[RowBoxesToString[#, opts] & /@ {rowBoxes}]

RowBoxesToString[s_String, OptionsPattern[]] := s

(*!
	\function Indent2
	
	\calltable
		Indent2[e_] '' Given an expression, convert it to a string and use multiple lines / indenting as needed to improve readability.
*)
Options[Indent2] =
{
	"FullFormStrings" -> False,				 (*< If True, then strings are FullFormed, so that they can round trip to files properly as ASCII characters. (since we want .m files to be ASCII) *)
	"RemoveContexts" -> True,				   (*< Remove contexts from symbols for improved readability. *)
	"AlwaysIndentToLevel" -> None,			  (*< Can be set if you'd like all expressions prior to a certain depth to be indented. *)
	"AlwaysIndentRulesToLevel" -> None,		 (*< Can be set if you'd like all rules prior to a certain depth to be indented. *)
	"AlwaysIndentRuleDelayedToLevel" -> None,   (*< Can be set if you'd like all RuleDelayed's prior to a certain depth to be indented. *)
	"AlwaysIndentListsOfRuleToLevel" -> None,   (*< Can be set if you'd like all lists of rules prior to a certain depth to be indented. *)
	"DoNotIndentRuleRhs" -> False,			  (*< Can be set to True if you don't want rile RHSs to be indented. *)
	"RuleLeftHandSidesNotToIndent" -> {},	   (*< If a rule has one of these LHSs, it won't be indented. *)
	"RemoveHold" -> False,					  (*< If a Hold or HoldComplete wraps the input, is it there only to keep the code from evaluating? In that case, we'll remove it for the caller. *)
	"UseTabs" -> True						   (*< Convert spaces to tabs. *)
};
Indent2[e_, initialIndent_Integer:0, spacesPerIndent_Integer:4, opts:OptionsPattern[]] :=
	Module[{res},
		
		With[{e2 = rowBoxFix1[e]},
		With[{eModified = HoldComplete[e2] /. Repeated :> "Indent2:Repeated"},
			
			res =
			StringReplace[
				linesToString[
					indent[
						eModified,
						(* Since we added HoldComplete *)
						initialIndent,
						spacesPerIndent,
						If [TrueQ[OptionValue["RemoveHold"]],
							(* Since the user added a HoldComplete, we need to adjust all of the
							   "AlwaysIndentToLevel"-like options accordingly. *)
							Sequence @@ adjustOptionsInvolvingLevel[opts]
							,
							Sequence @@
							   Append[
								   filterIndentOptions[opts],
								   "LevelAdjustment" -> 0
							   ]
						]
					]
				],
				"\"Indent2:Repeated\"" :> "Repeated"
			];
			
			res =
				If [TrueQ[OptionValue["RemoveHold"]],
					RemoveHoldFromIndentedString[res]
					,
					res
				];
				
			res = rowBoxFix2[res];
			
			If [TrueQ[OptionValue["UseTabs"]],
				With[{spaces = StringJoin[Table[" ", {spacesPerIndent}]]},
					res =
						StringReplace[
							res,
							StartOfLine~~sp:Repeated[spaces] :>
								With[{numTabs = StringLength[sp]/spacesPerIndent},
									repeatedTabs[numTabs]
								]
						];
				]
			];
			
			res
		]
		]
	]

(* Returns a string with the given number of tabs. *)
repeatedTabs[num_] := (repeatedTabs[num] = StringJoin[Table["\t", {num}]]);
	
(*!
	\function rowBoxFix1
	
	\calltable
		rowBoxFix1[e] '' replace uses of RowBox so that they don't end up getting handled by InputForm, which turns them into an unwanted form.
	
	Examples:
	
	rowBoxFix1[RowBox[{"a"}]] === "<<RowBox>>"[{"a"}]
	
	\maintainer danielb
*)
rowBoxFix1[e_] :=
	(* Need to use ReplaceRepeated to handle nested RowBoxes properly. *)
	ReplaceRepeated[
		e /.
			(* Encode these if they were actually passed in as part of the input
			   so that we don't mistake them as RowBox symbols in rowBoxFix2. *)
			"<<RowBox>>" :> "<<<RowBox>>>",
		RowBox[args___] :> "<<RowBox>>"[args]
	]

(*!
	\function rowBoxFix2
	
	\calltable
		rowBoxFix2[e] '' replace/undo uses of an encoded RowBox specifier. See also: rowBoxFix1
	
	\related 'rowBoxFix1
	
	\maintainer danielb
*)
rowBoxFix2[e_] :=
	StringReplace[
		e,
		{
			"\"<<RowBox>>\"" :> "RowBox",
			"\"<<<RowBox>>>\"" :> "\"<<RowBox>>\""
		}
	]
	
(*!
	\function adjustOptionsInvolvingLevel
	
	\calltable
		adjustOptionsInvolvingLevel[opts] '' Because we wrap a HoldComplete around the expression to prevent if from evaluating, we need to adjust all of the level-based options by 1.
	
	Examples:
	
	adjustOptionsInvolvingLevel[
		"AlwaysIndentToLevel" -> 1
	]
	
	===
	
	{
		"AlwaysIndentToLevel" -> 2
	}
	
	\related 'Indent2
	
	\maintainer danielb
*)
Clear[adjustOptionsInvolvingLevel];
Options[adjustOptionsInvolvingLevel] = Options[Indent2];
adjustOptionsInvolvingLevel[opts:OptionsPattern[]] :=
	Join[
		DeleteCases[
		   filterIndentOptions[opts],
		   HoldPattern[Rule][
			   "AlwaysIndentToLevel" |
			   "AlwaysIndentRulesToLevel" | 
			   "AlwaysIndentRuleDelayedToLevel" | 
			   "AlwaysIndentListsOfRuleToLevel",
			   _
		   ]
		],
		{
			If [OptionValue["AlwaysIndentToLevel"] =!= None, "AlwaysIndentToLevel" -> OptionValue["AlwaysIndentToLevel"] + 1, Sequence @@ {}],
			If [OptionValue["AlwaysIndentRulesToLevel"] =!= None, "AlwaysIndentRulesToLevel" -> OptionValue["AlwaysIndentRulesToLevel"] + 1, Sequence @@ {}],
			If [OptionValue["AlwaysIndentRuleDelayedToLevel"] =!= None, "AlwaysIndentRuleDelayedToLevel" -> OptionValue["AlwaysIndentRuleDelayedToLevel"] + 1, Sequence @@ {}],
			If [OptionValue["AlwaysIndentListsOfRuleToLevel"] =!= None, "AlwaysIndentListsOfRuleToLevel" -> OptionValue["AlwaysIndentListsOfRuleToLevel"] + 1, Sequence @@ {}],		 
			"LevelAdjustment" -> 1
		}
	]

Clear[maxLineLength];
maxLineLength[h:Rule] := 90
maxLineLength[HoldComplete[h:Rule]] := 90
maxLineLength[_] := 90

(* Turn the so-far-accrued lines into a string. *)
linesToString[lines_] :=
	If [ListQ[lines],
		StringJoin[Riffle[Flatten[lines], "\n"]]
		,
		lines
	]

(*!
	\function filterIndentOptions
	
	\calltable
		filterIndentOptions[opts] '' given options passed to one of the indent functions, filters them so that only the options that should be propagated to recursive calls are left.
	
	Examples:
	
	filterIndentOptions[
		"FullFormStrings" -> True,
		"SomeOtherOptions" -> False
	]
	
	===
	
	{"FullFormStrings" -> True}

	\maintainer danielb
*)
filterIndentOptions[opts___] :=
	FilterRules[{opts}, Append[Options[Indent2], "LevelAdjustment" -> 0]]

Clear[indent];

Options[indent] =
Join[
	Options[Indent2],
	{
		"ArgumentOfRule" -> False,		  (*< True if this expression is an argument of a Rule or RuleDelayed. *)
		"LevelAdjustment" -> 0			  (*< The amount added to level-based options because of the hold wrapper around the expression. *)
	}
];

indent[e:HoldComplete[f_[args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
	If [f =!= Pattern &&
		(
			StringLength[literal] > maxLineLength[HoldComplete[f]] ||
			(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"])
		),
		{
			indentLiteral[HoldComplete[f], lvl, spacesPerIndent, filterIndentOptions[opts]]<>"[",
			indentArgs[HoldComplete[args], lvl, spacesPerIndent, filterIndentOptions[opts]],
			indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
		}
		,
		{indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
	]
  ]
  
(* Set *)
indent[e:HoldComplete[Set[arg1_, arg2_]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
	Module[{lBracket = Sequence @@ {}, rBracket = Sequence @@ {}},
		
		If [TrueQ[OptionValue["ArgumentOfRule"]],
			(* If we are a Set inside of a Rule or Rule delayed, then we need to use
			   brackets for proper precedence. *)
			lBracket = indentString["(", lvl, spacesPerIndent, filterIndentOptions[opts]];
			rBracket = indentString[")", lvl, spacesPerIndent, filterIndentOptions[opts]];
		];
		
		With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
			If [StringLength[literal] > maxLineLength[HoldComplete[Set]] ||
				(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]),
				{lBracket, linesToString[indent[HoldComplete[arg1], lvl, spacesPerIndent, filterIndentOptions[opts]]] <> " =", indent[HoldComplete[arg2], lvl + 1, spacesPerIndent, filterIndentOptions[opts]], rBracket}
				,
				{lBracket, linesToString[indent[HoldComplete[arg1], lvl, spacesPerIndent, filterIndentOptions[opts]]] <> " = " <> indentLiteral[HoldComplete[arg2], 0, spacesPerIndent, filterIndentOptions[opts]], rBracket}
			]
		]
	]

(* List *)
indent[e:HoldComplete[List[args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
	If [(StringLength[literal] > maxLineLength[HoldComplete[List]]) ||
		(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]) ||
		(IntegerQ[OptionValue["AlwaysIndentListsOfRuleToLevel"]] && lvl <= OptionValue["AlwaysIndentListsOfRuleToLevel"] && MatchQ[e, HoldComplete[List[__Rule]]]),
		If [HoldComplete[{args}] === HoldComplete[{}],
			{
				indentString["{", lvl, spacesPerIndent, filterIndentOptions[opts]],
				indentString["}", lvl, spacesPerIndent, filterIndentOptions[opts]]
			}
			,
			{
				indentString["{", lvl, spacesPerIndent, filterIndentOptions[opts]],
				indentArgs[HoldComplete[args], lvl, spacesPerIndent, filterIndentOptions[opts]],
				indentString["}", lvl, spacesPerIndent, filterIndentOptions[opts]]
			}
		]
		,
		{indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
	]
  ]

(* For Rule *)
indent[e:HoldComplete[Rule[arg1_, arg2_]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
	If [(
			(StringLength[literal] > maxLineLength[HoldComplete[Rule]]) ||
			(IntegerQ[OptionValue["AlwaysIndentRulesToLevel"]] && lvl <= OptionValue["AlwaysIndentRulesToLevel"] && !MatchQ[e, HoldComplete[Rule["Example", _]]]) ||
			(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"])
		)
		&&
		(
			(
				OptionValue["RuleLeftHandSidesNotToIndent"] === {} ||
				!MemberQ[OptionValue["RuleLeftHandSidesNotToIndent"], arg1]
			)
		),
		{
			appendToLast[indentLiteral[HoldComplete[arg1], lvl, spacesPerIndent, filterIndentOptions[opts]], " ->"],
				If [TrueQ[OptionValue["DoNotIndentRuleRhs"]] ||
					MatchQ[HoldComplete[arg2], HoldComplete[_List]],
					
					indent[HoldComplete[arg2], lvl, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]]
					,
					indent[HoldComplete[arg2], lvl + 1, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]]
				]
		}
		,
		{indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
	]
  ]
  
(* For RuleDelayed *)
indent[e:HoldComplete[RuleDelayed[arg1_, arg2_]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
	If [(
			StringLength[literal] > maxLineLength[HoldComplete[Rule]] ||
			(IntegerQ[OptionValue["AlwaysIndentRulesToLevel"]] && lvl <= OptionValue["AlwaysIndentRulesToLevel"] && !MatchQ[e, HoldComplete[Rule["Example", _]]]) ||
			(IntegerQ[OptionValue["AlwaysIndentRuleDelayedToLevel"]] && lvl <= OptionValue["AlwaysIndentRuleDelayedToLevel"]) ||
			(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"])
		),
		{
			appendToLast[indent[HoldComplete[arg1], lvl, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]], " :>"],
			indent[HoldComplete[arg2], lvl + 1, spacesPerIndent, "ArgumentOfRule" -> True, filterIndentOptions[opts]]
		}
		,
		{indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
	]
  ]
  
(* If *)
indent[e:HoldComplete[If[arg1_, args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
  With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
	If [StringLength[literal] > maxLineLength[HoldComplete[If]],
		With[{testLiteral = indentLiteral[HoldComplete[arg1], 0, spacesPerIndent, filterIndentOptions[opts]]},
			(* Is the condition being test large enough to warrant its own line? *)
			If [StringLength[testLiteral] > 60 ||
				(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]),
				{
					indentLiteral[If, lvl, spacesPerIndent, filterIndentOptions[opts]]<>"[",
					indentArgs[HoldComplete[arg1, args], lvl, spacesPerIndent, "ArgNum" -> 1, "CommaOnNewLine" -> True, filterIndentOptions[opts]],
					indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
				}
				,
				{
					indentLiteral[If, lvl, spacesPerIndent, filterIndentOptions[opts]]<>"["<>indentLiteral[HoldComplete[arg1], 0, spacesPerIndent, filterIndentOptions[opts]]<>",",
					indentArgs[HoldComplete[args], lvl, spacesPerIndent, "ArgNum" -> 2, "CommaOnNewLine" -> True, filterIndentOptions[opts]],
					indentString["]", lvl, spacesPerIndent, filterIndentOptions[opts]]
				}
			]
		]
		,
		{indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
	]
  ]
 
 (* CompoundExpression, one arg, with Null at the end. *)
 indent[e:HoldComplete[CompoundExpression[args_, Null]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
	With[{e2 = ReplacePart[e, {1, 0} -> HoldComplete][[1]]},
		(* Drop the Null. *)
		linesToString[indent[Drop[e2, -1], lvl, spacesPerIndent, filterIndentOptions[opts]]] <> ";"
	]
 
(* CompoundExpression *)
indent[e:HoldComplete[CompoundExpression[args__]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
	Module[{trailingNull = "", lines},
		With[{e2 = ReplacePart[e, {1, 0} -> HoldComplete][[1]] /.
					   (* Get rid of the trailing Null if it's there. *)
					   HoldComplete[args2___, Null] :> (trailingNull = ";"; HoldComplete[args2])},
			With[{tmp =
					linesToString[
						lines = 
						indentArgs[
							e2,
							lvl,
							spacesPerIndent,
							"ArgNum" -> 1,
							"CommaOnNewLine" -> False,
							"Delimiter" -> ";",
							filterIndentOptions[opts]
						]
					] <> trailingNull
				 },
				 
				If [lvl > OptionValue["LevelAdjustment"] && Length[lines] > 1,
					With[{spaces = StringJoin[Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]]]},
						Join[
							{spaces <> "("},
							{tmp},
							{spaces <> ")"}
						]
					]
					,
					tmp
				]
			]
		]
	]

(* Association *)
indent[e:HoldComplete[e2:Association[args___]], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
	With[{literal = indentLiteral[e, 0, spacesPerIndent, filterIndentOptions[opts]]},
		If [StringLength[literal] > maxLineLength[HoldComplete[Association]] ||
			(IntegerQ[OptionValue["AlwaysIndentToLevel"]] && lvl <= OptionValue["AlwaysIndentToLevel"]),
			If [HoldComplete[{args}] === HoldComplete[{}],
				{
					indentString["<|", lvl, spacesPerIndent, filterIndentOptions[opts]],
					indentString["|>", lvl, spacesPerIndent, filterIndentOptions[opts]]
				}
				,
				{
					indentString["<|", lvl, spacesPerIndent, filterIndentOptions[opts]],
					indentArgs[
						HoldComplete @@
							(* If you don't call Normal on the association, HoldComplete @@ args
							   munges the data for some reason. *)
							Normal[e2], lvl, spacesPerIndent, filterIndentOptions[opts]],
					indentString["|>", lvl, spacesPerIndent, filterIndentOptions[opts]]
				}
			]
			,
			{indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]]}
		]
	]

Clear[indentArgs];
Options[indentArgs] =
Join[
	Options[indent],
	{
		"ArgNum" -> 1,				 (*< argument number. *)
		"CommaOnNewLine" -> False,	 (*< should the comma be on its own line? *)
		"Delimiter" -> ","			 (*< character to use as the argument delimiter. *)
	}
];
indentArgs[e:HoldComplete[firstArg_, remainingArgs___], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
	If [Length[e] > 1,
		If [TrueQ[OptionValue["CommaOnNewLine"]] && OptionValue["ArgNum"] > 1,
			(* The commas that separate the arguments of If should be on their own line. (Except for the first one) *)
			{indent[HoldComplete[firstArg], lvl+1, spacesPerIndent, filterIndentOptions[opts]], indentString[OptionValue["Delimiter"], lvl+1, spacesPerIndent, filterIndentOptions[opts]], indentArgs[HoldComplete[remainingArgs], lvl, spacesPerIndent, "ArgNum" -> OptionValue["ArgNum"] + 1, "CommaOnNewLine" -> OptionValue["CommaOnNewLine"], "Delimiter" -> OptionValue["Delimiter"], filterIndentOptions[opts]]}
			,
			{appendToLast[indent[HoldComplete[firstArg], lvl+1, spacesPerIndent, filterIndentOptions[opts]], OptionValue["Delimiter"]], indentArgs[HoldComplete[remainingArgs], lvl, spacesPerIndent, "ArgNum" -> OptionValue["ArgNum"] + 1, "CommaOnNewLine" -> OptionValue["CommaOnNewLine"], "Delimiter" -> OptionValue["Delimiter"], filterIndentOptions[opts]]}
		]
		,
		indent[HoldComplete[firstArg], lvl+1, spacesPerIndent, filterIndentOptions[opts]]
	]
	
indent[e_, lvl_, spacesPerIndent_, opts:OptionsPattern[]] := indentLiteral[e, lvl, spacesPerIndent, filterIndentOptions[opts]] 
  
Clear[indentLiteral];
Options[indentLiteral] = Options[indent];
indentLiteral[e : HoldComplete[_], lvl_, spacesPerIndent_, opts:OptionsPattern[]] :=
	Module[{heldSymbol, contexts},
		
		Attributes[heldSymbol] = {HoldAllComplete};
		
		(* We'll strip domain grammar contexts here for readability.
		   This global variable can be set to True to avoid this. The reason
		   I'm not going to bother with an option right now is that it would
		   be too much plumbing passing it around. *)
		If [!TrueQ[OptionValue["RemoveContexts"]],
			Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> heldLiteralToString[e, opts]
			,
			contexts =
				Flatten[Reap[
					Replace[
						e,
						s_Symbol :>
							With[{res =
								  (
									  If [Context[s] =!= "System`", Sow[Context[s], "Context"]]
								  )
								 },
								res /; True
							],
						{1, Infinity},
						Heads -> True
					],
					"Context"
				][[2]], 1];

			Block[{$ContextPath = Join[contexts, {"System`"}]},
			   Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> heldLiteralToString[e, opts]
			]
		]
	]
	
Options[heldLiteralToString] = Options[indent];
heldLiteralToString[e:HoldComplete[_], OptionsPattern[]] :=
	Module[{},
		(* Remove the "HoldComplete[" and "]" from around the string. *)
		StringTake[
			If [TrueQ[OptionValue["FullFormStrings"]],
				toInputFormWithFullFormStrings[e]
				,
				ToString[e, InputForm]
			],
			{14, -2}
		]
	]
	
(*!
	\function toInputFormWithFullFormStrings
	
	\calltable
		toInputForm[e] '' converts an expression to InputForm, taking special care to first convert strings to FullForm. This is useful so that things like \[Rule] get rendered as "\[Rule]", and not as a non-ASCII character. Why is that useful? Because if we're going to write the reuslts in Indent2 to a file, then we'd like it to be ASCII so that it can be edited properly in Workbench, etc, and our standards for .m files are that they should be in ASCII. InputForm on its own would convert \[Rule] to "->", which wouldn't round trip properly to and from file.
	
	Examples:
	
	toInputFormWithFullFormStrings[{"a", "b", "\[Rule]"}] === "{a, b, \"\\[Rule]\"}"

	\related 'heldLiteralToString
	
	\maintainer danielb
*)
toInputFormWithFullFormStrings[e_] :=
	Module[{},
		ToString[e /. s_String :> FullForm[s]]
	]

indentLiteral[e_, lvl_, spacesPerIndent_, OptionsPattern[]] :=
	Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> ToString[e, InputForm]
indentString[str_, lvl_, spacesPerIndent_, OptionsPattern[]] :=
	Flatten[Table[Table[" ", {spacesPerIndent}], {lvl}]] <> str
	
appendToLast[list_, suffix_String] :=
	MapAt[# <> suffix &, Flatten[{list}], -1]

(*!
	\function ToExpressionPreservingComments
	
	\calltable
		ToExpressionPreservingComments[str] '' like ToExpression but attempts to preserve comments by converting them to expressions. Wraps the expression in a list so that top-level comments work.
	
	Examples:
	
	ToExpressionPreservingComments["myFunc[\"(* my comment *)\"]", "HoldResult" -> True]

	===

	HoldComplete[{myFunc["(* my comment *)"]}]

	Unit tests:

	RunUnitTests[WUtils`WUtils`ToExpressionPreservingComments]

	\maintainer danielb
*)
Clear[ToExpressionPreservingComments];
Options[ToExpressionPreservingComments] =
{
	"HoldResult" -> False,		  (*< Wrap the result in HoldComplete? *)
	"AppendCommas" -> False		 (*< append commas after comment expressions? ex. When comments are within a list. *)
};
ToExpressionPreservingComments[str_, OptionsPattern[]] :=
	Module[{data = str},
		
		(* Wrap in { ... } incase there is a top-level comment. *)
		data = ReplaceCommentsWithExpressions[data, "AppendCommas" -> OptionValue["AppendCommas"]];
		
		ListOfHeldToHeldList[
			ReadList[
				StringToStream[data],
				If [TrueQ[OptionValue["HoldResult"]],
					HoldComplete[Expression]
					,
					Expression
				]
			]
		]
	]

(*!
	\function ReplaceCommentsWithExpressions
	
	\calltable
		ReplaceCommentsWithExpressions[str] '' performs a replacement on a string, replacing any (* comments *) with WL expressions that will survive ToExpression.
	
	Examples:
	
	ReplaceCommentsWithExpressions[
	"(* Just testing *)
	\"a\""
	]
	
	===
	
	"\"Comment\" -> Comment[\"Just testing\"]
	\"a\""

	Unit tests:

	RunUnitTests[WUtils`WUtils`ReplaceCommentsWithExpressions]

	\maintainer danielb
*)
Clear[ReplaceCommentsWithExpressions];
Options[ReplaceCommentsWithExpressions] =
{
	"AppendCommas" -> False		 (*< append commas after comment expressions? ex. When comments are within a list. *)
};
ReplaceCommentsWithExpressions[str_, OptionsPattern[]] :=
	Module[{comma = ""},
		
		If [TrueQ[OptionValue["AppendCommas"]],
			comma = ",";
		];
		
		StringReplace[
			str,
			{
			b:doubleQuotedStringPattern[] :> b,
			commentPattern[] :>
				ToString["Comment" -> Global`Comment[toSingleLine["$1"]], InputForm] <> comma
			}
		]
	]

(* Turns a multi-line string into a single-line string. *)
toSingleLine[str_] :=
	StringReplace[str, RegularExpression["[\\n\\r]+\\s*"] :> " "]

(*!
	\function ListOfHeldToHeldList
	
	\calltable
		ListOfHeldToHeldList[listOfHeld_] '' given a List of eld items (ie. List[HoldComplete[a], HoldComplete[b], HoldComplete[c], ...]), converts it into a held list of things. (ie. HoldComplete[List[a, b, c, ...]])
	
	\maintainer danielb
*)
ListOfHeldToHeldList[listOfHeld_] := Replace[HoldComplete[listOfHeld], HoldComplete[x_] :> x, {2}];

(*!
	\function HeldListToListOfHeld
	
	\calltable
		HeldListToListOfHeld[heldList_] '' given a held List (ie. HoldComplete[List[a, b, c, ...]]), convert it into list of held things. (ie. List[HoldComplete[a], HoldComplete[b], HoldComplete[c], ...])
	
	\maintainer danielb
*)
HeldListToListOfHeld[heldList_] :=
	(
	Replace[
		List @@ (HoldComplete /@ (HoldComplete @@@ heldList)[[1]])
		,
		(* If one of the items was already surrounded by HoldComplete,
		   remove the extra one we added. *)
		HoldComplete[HoldComplete[e_]] :> HoldComplete[e]
		,
		2
	]
	);

(*!
	\function DistributeHeldCompoundExpressions
	
	\calltable
		DistributeHeldCompoundExpressions[e] '' When using ToExpressionPreservingComments on code, multi-line code ends up as HoldComplete[CompoundExpression[...]] but we'd like each of the items in the compound expression to be a top-level item.
	
	Examples:
	
	DistributeHeldCompoundExpressions[
		{
			HoldComplete["Comment" -> Comment["Test comment."]],
			HoldComplete[
				CompoundExpression[
					ReloadVirtualAssistantFiles[],
					1 + 1
				]
			]
		}
	]
	
	===
	
	{
		HoldComplete["Comment" -> Comment["Test comment."]],
		HoldComplete[ReloadVirtualAssistantFiles[]],
		HoldComplete[1 + 1]
	}
	
	\related 'GetCodeCellExpressions
	
	\maintainer danielb
*)
Clear[DistributeHeldCompoundExpressions];	
DistributeHeldCompoundExpressions[e_] :=
	Module[{},
		Replace[
			e,
			HoldComplete[CompoundExpression[a_, b__]] :>
				With[{res = (Sequence @@ HeldListToListOfHeld[HoldComplete[{a, b}]])},
					res /; True
				],
			{1}
		]
	]

(*!
	\function GetFunctionUsesFromNotebookHelper
	
	\calltable
		GetFunctionUsesFromNotebookHelper[funcSymbol, codeCellExpressions] '' given the expression extracted from code cells in a notebook, looks for and returns uses of the given function. Adds annotations to indicate top-of-code-cell comments, etc.
	
	Examples:
	
	GetFunctionUsesFromNotebookHelper[
		myFunc,
		{
			{
				{HoldComplete["Comment"["Test comment"]], HoldComplete[myFunc[1, 2, 3]]},
				CellObject[1]
			}
		}
	]
	
	===
	
	{
		FunctionUse[
			HoldComplete[myFunc[1, 2, 3]],
			"CellObject" -> CellObject[1],
			"Comment" -> "Test comment"
		]
	}

	Unit tests:

	RunUnitTests[CalculateParse`Prototype`VirtualAssistant`VaActions`GetFunctionUsesFromNotebookHelper]

	\related 'GetFunctionUsesFromNotebook
	
	\maintainer danielb
*)
Clear[GetFunctionUsesFromNotebookHelper];
Options[GetFunctionUsesFromNotebookHelper] =
{
	"IncludeAdditionalTestExpressions" -> False			(*< Should other test expressions such as TestHead[...] be included? *)
};
GetFunctionUsesFromNotebookHelper[funcSymbol_, codeCellExpressions_, OptionsPattern[]] :=
	Module[{positions, uses, wrappedSymbolsToIgnore, level1Pos, firstLine, comment, functionUse, level3Pos},
		
		With[{pattern =
				If [OptionValue["IncludeAdditionalTestExpressions"],
					HoldPattern[funcSymbol][___] | HoldPattern[TestHead][___] | HoldPattern[AsTest][___] | HoldPattern[MUnit`TestExecute][___]
					,
					HoldPattern[funcSymbol][___]
				]},
			positions =
				Position[
					codeCellExpressions,
					pattern,
					Infinity,
					Heads -> True
		
				(* We crop the position so that if the use of the function
				   was inside of a larger expression, we take the entire
				   larger expression. This is important for cases like
				   ReplaceStringInFile's example, where you need the
				   super expression for the use of the function to make
				   sense. *)
				][[All, 1;;3]];
		];
		
		uses =
			Function[{position},
				
				level1Pos = position[[1]];
				
				level3Pos = position[[3]];
				
				(* Does this code cell have a comment? *)
				comment = Sequence @@ {};
				If [level3Pos > 1,
					firstLine = codeCellExpressions[[level1Pos, 1, 1]];
					If [MatchQ[firstLine, HoldComplete["Comment"[___]]],
						(* This code cell has a comment as its first line. Annotate
						   the function use with it. *)
						comment = "Comment" -> firstLine[[1, 1]];
					]
				];
				
				functionUse = Part[codeCellExpressions, Sequence @@ position];
				
				FunctionUse[
					functionUse,
					"CellObject" -> codeCellExpressions[[level1Pos, 2]],
					comment
				]
				
			] /@ positions;
			
		(* The DeleteCases is useful when we're writing tests for Indent2 itself. *)
		wrappedSymbolsToIgnore = Alternatives @@ DeleteCases[{Indent2, InputForm, TextSearch`PackageScope`Indent2}, funcSymbol];
			
		(* If wrapped by Indent2 or InputForm, remove those, because
		   they are typically used for better viewing output. *)
		DeleteDuplicates@
		With[{wrappedSymbolsToIgnore2 = wrappedSymbolsToIgnore},
			Replace[
				uses,
				{
					FunctionUse[HoldComplete[wrappedSymbolsToIgnore2[e__]], args___] :> FunctionUse[HoldComplete[e], args],
					FunctionUse[HoldComplete[((wrappedSymbolsToIgnore2)[___] & )[e__]], args___] :> FunctionUse[HoldComplete[e], args]
				},
				{1},
				Heads -> True
			]
		]
	]

(*!
	\function UnitTestFilename
	
	\calltable
		UnitTestFilename[funcSymbol] '' given a function symbol, what unit test filename should be used?
	
	Examples:
	
	UnitTestFilename[WUtils`WUtils`CouldBeWLSymbolQ]

	Unit tests:

	RunUnitTests[WUtils`WUtils`UnitTestFilename]

	\related 'EnsureUnitTestFileExists `CreateUnitTests
	
	\maintainer danielb
*)
Clear[UnitTestFilename]; 
Options[UnitTestFilename] =
{
	"SourceFile" -> Automatic,			(*< The source file that implements the function. *)
	"SubTest" -> None,					(*< The sub-test file, if any *)
	"ReturnList" -> False				(*< If there is a directory for this function with sub-test files, should we return them all? *)
};
UnitTestFilename[funcSymbol_, OptionsPattern[]] :=
	Module[{dir, funcSymbolContext, file, alternateFile, sourceFile, res},
		
		(* Might be overridden for symbols like CodeToIndentedString
		   which are implemented in a different file than their context. *)
		sourceFile = OptionValue["SourceFile"];
		If [sourceFile === Automatic,
			sourceFile = SymbolToFile[funcSymbol];
		];
		
		If [sourceFile =!= $Failed,
			funcSymbolContext = FileToContext[sourceFile];
			
			If [funcSymbolContext === None,
				funcSymbolContext = Context[funcSymbol];
				,
				If [StringQ[funcSymbolContext] &&
					funcSymbolContext <> "Private`" === Context[funcSymbol],
					funcSymbolContext = Context[funcSymbol];
				];
			];
			,
			funcSymbolContext = Context[funcSymbol];
		];
		
		If [!StringQ[funcSymbolContext],
			Print["UnitTestFilename: Couldn't determine the context of function ", ToString@funcSymbol, "."];
			Return[$Failed];
		];
		
		dir = UnitTestDirectory[funcSymbolContext, If [StringQ[sourceFile], sourceFile, Null]];
		
		If [dir === Missing[],
			Print["UnitTestFilename: Couldn't determine the unit test directory for the function ", ToString@funcSymbol, "."];
			Return[$Failed];
		];
		
		(* Experimental: If a bunch of files share the same context, then
		   we want to create sub-directories based on the file's name.
		   For example, "MachineLearning`PackageScope`" is shared by
		   several files, so we'll create a "Markov" directory under it. *)
		If [StringQ[sourceFile] && FileBaseName[sourceFile] =!= FileBaseName[dir] && FileBaseName[dir] =!= "Private",
			dir = FileNameJoin[{dir, FileBaseName[sourceFile]}];
		];
		
		If [OptionValue["SubTest"] =!= None,
			dir = FileNameJoin[{dir, ToString[funcSymbol]}];
		];
		
		If [!FileExistsQ[dir],
			If [CreateDirectory[dir, CreateIntermediateDirectories -> True] === $Failed,
				Print["UnitTestFilename: Couldn't create directory: ", dir];
				Return[$Failed];
			];
		];
		
		If [StringQ[OptionValue["SubTest"]],
			file = FileNameJoin[{dir, OptionValue["SubTest"] <> ".mt"}];
			,
			file = FileNameJoin[{dir, SymbolName[funcSymbol] <> ".mt"}];
		];
		
		res =
		If [FileExistsQ[file],
			If [TrueQ[TrueQ[OptionValue["ReturnList"]]],
				With[{possibleSubTestDir = StringDrop[file, -3]},
					If [DirectoryQ[possibleSubTestDir],
						Join[{file}, FileNames["*.mt", possibleSubTestDir, Infinity]]
						,
						file	
					]
				]
				,
				file
			]
			,
			(* Also check if there's a "NonSQARun" directory. *)
			alternateFile = FileNameJoin[{dir, "NonSQARun", SymbolName[funcSymbol] <> ".mt"}];
			If [FileExistsQ[alternateFile],
				alternateFile
				,
				file
			]
		];
		
		If [TrueQ[OptionValue["ReturnList"]],
			Flatten[{res}]
			,
			res
		]
	]

(*!
	\function SymbolToFile
	
	\calltable
		SymbolToFile[symbol] '' given a symbol, returns the file it is defined in, or $Failed if unknown.
	
	Examples:
	
	SymbolToFile[WUtils`WUtils`SymbolToFile]

	Unit tests:

	RunUnitTests[SymbolToFile]

	\related 'ContextToFile
	
	\maintainer danielb
*)
SymbolToFile[symbol_] := ContextToFile[Context[symbol]]
	
SymbolToFile[CalculateScan`CommonSymbols`CodeToIndentedString] := FindFile["WUtils`WUtils`"]

(*!
	\function ContextToFile
	
	\calltable
		ContextToFile[context] '' given a context, returns the path to the corresponding file. Similar to FindFile, except that it also works with a Private context. Returns $Failed if the file could not be found.

	Example:

	ContextToFile["WUtils`WUtils`"]

	Unit tests:

	RunUnitTests[WUtils`WUtils`ContextToFile]

	\maintainer danielb
*)

(* Best not to Clear this when reloading this file, since files in non-standard
   locations may want to override the default location for their context. *)
(*Clear[ContextToFile];*)

ContextToFile[context_String] :=
	Module[{file, file2, context2},
		
		If [StringMatchQ[context, __ ~~ "`Private`"],
			(* It's important to recurse here and not run the below code directly
			   on the modified context, so that ContextToFile can be overloaded
			   in other files if they are in a non-standard location. *)
			ContextToFile[StringDrop[context, -8]]
			,
			If [StringMatchQ[context, __ ~~ "`PackagePrivate`"],
				context2 = ContextToFile[StringDrop[context, -15]];
				If [context2 === $Failed,
					context2 = ContextToFile[StringDropByDelim[context, "`", -2] <> "`"]
					,
					context2
				]
				,	
				file = FindFile[context];
				
				(* If we're looking up a context that corresponds to a Kernel/init.m file,
				   then check whether there's a file in the parent directory with the
				   appropriate name. *)
				If [FileNameTake[file, -1] === "init.m" &&
					FileNameTake[file, {-2}] === "Kernel",
					
					file2 = FileNameJoin[{FileNameDrop[file, -2], Last[StringSplit[context, "`"]] <> ".m"}];
					If [FileExistsQ[file2],
						file2
						,
						file
					]
					,
					file
				]
			]
		] 
	]

(*!
	\function StringDropByDelim
	
	\calltable
		StringDropByDelim[str, delim, n] '' StringDrop, but by portions demarkated by a delimiter.
	
	Example:
	
	StringDropByDelim["a&b&c", "&", -1] === "a&b"
	
	\maintainer danielb
*)
StringDropByDelim[str_, delim_, n_] :=
	StringJoin[Riffle[Drop[StringSplit[str, delim], n], delim]]

(*!
	\function UnitTestDirectory
	
	\calltable
		UnitTestDirectory[context] '' given a context, returns the directory that unit tests should be placed into.
	
	We first check whether the given context defines a variable named $UnitTestDir.
	If so, we return it. Otherwise, we check its Private context. If it's not defined
	there, we progressively check higher and higher level contexts to see whether
	any of them define $UnitTestDir.
	
	That allows a package such as CalculateParse` to define a unit test directory,
	and allows sub-contexts to override that directory.
	
	Returns Missing[] if the default unit test directory isn't defined.
	
	Example:
	
	UnitTestDirectory["WUtils`WUtils`"]

	Unit tests:

	RunUnitTests[UnitTestDirectory]

	\related 'CreateUnitTests
	
	\maintainer danielb
*)
Clear[UnitTestDirectory];
UnitTestDirectory[context_String, sourceFile_:Null] :=
	Module[{contextParts, contextValueWasDefinedIn, unitTestBaseDir},
		
		contextParts = StringSplit[context, "`"];
		
		With[{tmp = GetVariablePossiblyFromParentPackage[context, "$UnitTestDir"]},
			If [tmp === Missing[],
				Return[Missing[]]
				,
				{contextValueWasDefinedIn, unitTestBaseDir} = tmp;
				
				If [MatchQ[unitTestBaseDir, _Function],
					(* We have a function that takes the source file as an argument
					   and returns the test directory. *)
					FileNameJoin[
						{
							unitTestBaseDir[sourceFile],
							(* One sub-directory per source file. (and one test file per function) *)
							FileBaseName[sourceFile]
						}
					]
					,
					(* If we found the variable defined in a private context, drop the Private
					   portion of the context. *)
					contextValueWasDefinedIn = StringReplace[contextValueWasDefinedIn, "`Private`" :> "`"];
					
					FileNameJoin[
						{
							unitTestBaseDir,
							Sequence @@ StringSplit[StringDrop[context, StringLength[contextValueWasDefinedIn]], "`"]
						}
					]
				]
			]
		]
	]

(*!
	\function GetVariablePossiblyFromParentPackage
	
	\calltable
		GetVariablePossiblyFromParentPackage[context, symbolName] '' given a starting context and a symbol, returns the value of the symbol with the given name (and the context it was found in), checking both the private and public context. If it is not found, successive parent/ancestor packages are tried. Returns Missing[] if not found.
		
	This allows packages to define settings which can then be overridden by their children packages. (ex. $UnitTestDir)

	Unit tests:

	RunUnitTests[GetVariablePossiblyFromParentPackage]

	\related 'ReloadFunction
	
	\maintainer danielb
*)
GetVariablePossiblyFromParentPackage[context_, symbolName_] :=
	Module[{contextParts, thisContext},
		
		contextParts = StringSplit[context, "`"];
		
		(* Try progressively higher-level contexts, looking for the
		   symbol. *)
		Function[{i},
			
			thisContext = StringJoin[Riffle[contextParts[[1 ;; i]], "`"]] <> "`";
			
			If [ContextContainsSymbol[thisContext, symbolName],
				With[{symbol = Symbol[thisContext <> symbolName]},
					If [!MatchQ[symbol, _Symbol] || SymbolName[symbol] =!= symbolName || DownValues[symbol] =!= {},
						Return[{thisContext, symbol}, Module];
					];
				];
			];
			
			If [contextParts[[i]] =!= "Private",
				
				(* Also try the Private context, which is where we
				   would expect $UnitTestDir to be defined. *)
				thisContext = thisContext <> "Private`";
				
				If [ContextContainsSymbol[thisContext, symbolName],
					With[{symbol = Symbol[thisContext <> symbolName]},
						If [!MatchQ[symbol, _Symbol] || SymbolName[symbol] =!= symbolName || DownValues[symbol] =!= {},
							Return[{thisContext, symbol}, Module];
						]
					];
				];
			]
			
		] /@ Reverse[Range[1, Length[contextParts]]];
		
		Return[Missing[]]
	]

(*!
	\function ContextContainsSymbol
	
	\calltable
		ContextContainsSymbol[context, symbolName] '' returns True if the given context contains a symbol with the given name.
	
	Example:
	
	ContextContainsSymbol["WUtils`WUtils`", "ContextContainsSymbol"] === True
	
	\maintainer danielb
*)
ContextContainsSymbol[context_, symbolName_] :=
	Module[{names},
		names = Last /@ (StringSplit[#, "`"] & /@ Names[context <> "*"]);
		If [MemberQ[names, symbolName],
			True
			,
			False
		]
	]

(*!
	\function EnsureUnitTestFileExists
	
	\calltable
		EnsureUnitTestFileExists[funcSymbol, file] '' ensures the given unit test file exists.
	
	\related 'CreateUnitTests
	
	\maintainer danielb
*)
EnsureUnitTestFileExists[funcSymbol_, file_] :=
	Module[{template},
		If [!FileExistsQ[file],
			template = UnitTestTemplate[funcSymbol, file];
			If [template === $Failed, Return[$Failed]];
			
			Export[file, template, "String"]
		];
	]

(*!
	\function UnitTestTemplate
	
	\calltable
		UnitTestTemplate[funcSymbol] '' given a function symbol, generates a template for a unit test file.

	\related 'EnsureUnitTestFileExists 'CreateUnitTests
	
	\maintainer danielb
*)
UnitTestTemplate[funcSymbol_, file_] :=
	Module[{relativePath, insideCalculateParse},
		
		relativePath = MakePathRelativeToPaths[file];
		
		insideCalculateParse = !StringFreeQ[Context[funcSymbol], "CalculateParse"];
		
		If [!StringQ[relativePath],
			Print["UnitTestTemplate: Couldn't determine path relative to Alpha for file: " <> file];
			Return[$Failed];
		];

"(* Tests for: " <> Context[funcSymbol] <> Last[StringSplit[ToString[funcSymbol], "`"]] <> "

   Author: " <> Username[] <>
If [insideCalculateParse,
	"
	
   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
	   FindFile[" <> ToString[relativePath, InputForm] <> "]
   ]
*)

TestExecute[$TestAbortTime = 600]

TestExecute[
	If[TrueQ[Quiet[Get[\"CalculateTestEnvironment.m\"]]===$Failed],
		Get[
		StringCases[$CurrentFile,
		inputfile:(StartOfString~~___~~$PathnameSeparator~~\"Tests\"~~$PathnameSeparator)~~___
		:> inputfile<>\"Utilities\"<>$PathnameSeparator<>\"CalculateTestEnvironment.m\"][[1]]
		]]
]

TestExecute[$CalculateDataPacletsInit = False;  << \"CalculateLoader`\"]

TestExecute[$TestAbortTime = $TestAbortTimeInitial]
"
,
"
*)"
]
	(* Doesn't seem to work, so we'll use fully qualified symbols. *)
	(* Needs[" <> ToString[Context[funcSymbol], InputForm] <> "]; *)
	]

(*!
	\function MakePathRelativeToPaths
	
	\calltable
		MakePathRelativeToPaths[path] '' given a path, determine which $Path it is relative to, and return the relative path. (which could be then used with FindFile in the future, etc)
	
	\maintainer danielb
*)
MakePathRelativeToPaths[path_] :=
	Module[{paths, res},
		
		paths = Select[$Path, StringMatchQ[path, # ~~ __] &];
		
		(* Take the longest, so that, for example, we get
		   a relative path like CalculateParse/Prototype/VirtualAssistant
		   even if we also had a path on $Path that could have
		   resulted in Source/CalculateParse/Prototype/VirtualAssistant. *)
		paths = Reverse[SortBy[paths, StringLength[#] &]];
		
		If [Length[paths] > 0,
			res =
				StringReplace[
					StringDrop[
						path,
						StringLength[paths[[1]]]
					],
					(* Favor / for separators *)
					$PathnameSeparator :> "/"
				];
				
			If [StringMatchQ[res, "/" ~~ __],
				StringDrop[res, 1]
				,
				res
			]
			,
			$Failed
		]
	]

(* The user's usrename. *)
Username[] :=
	If [ValueQ[$UserNameOverride],
		(* Handy for unit tests. *)
		$UserNameOverride
		,
		($UserName /. "Daniel" | "meredithbigham" :> "danielb")
	]

(*!
	\function FunctionCallsTestedByFile
	
	\calltable
		FunctionCallsTestedByFile[funcSymbol, file] '' given a test file and a function symbol, returns all calls to that function tested by the file. 
	
	Output format:
	
	A list of items of the form:
	
	{HoldComplete[testedExpression_], "TestId" -> _String}
	
	... where the TestID is only indicated if known.
	
	\related 'CreateUnitTests
	
	\maintainer danielb
*)
FunctionCallsTestedByFile[funcSymbol_, file_] :=
	Module[{positions, uses, tests},
		
		With[{testIdSymbol = testIDSymbol[]},
		
			If [!FileExistsQ[file], Print["InputsTestedByFile: Missing file: " <> file]; Return[$Failed]];
			
			(* Read in the test file and grab the Tests. *)
			tests =
				Cases[
					ReadList[file, HoldComplete[Expression]],
					HoldComplete[(s_Symbol)[___]] /; SymbolName[s] === "Test"
				];
			
			positions =
				Position[
					tests,
					e:HoldPattern[funcSymbol][___],
					Infinity,
					Heads -> True
				][[All, 1]];
			 
			uses =
				Part[
					tests,
					#
				] & /@ positions;
			
			Replace[
				uses,
				HoldComplete[
					(s_Symbol)[
						e_,
						(* Apparently needs Shortest, otherwise is greedy and eats the TestID option we're interested in. *)
						Shortest[___],
						Repeated[("TestID" | testIdSymbol) -> testId_, {1, 1}],
						(* Apparently needs Shortest, otherwise is greedy and eats the TestID option we're interested in. *)
						Shortest[___]
					]
				] /; SymbolName[s] === "Test"
				   :>
					{
						HoldComplete[e],
						KeepRuleIfNotSequence["TestId" -> testId]
					},
				{1}
			]
			
		]
	]

(* Apparently M10 defines TestID in System`. *)
testIDSymbol[] :=
	If [Floor[$VersionNumber] === 9,
		ToExpression["MUnit`TestID"]
		,
		ToExpression["System`TestID"]
	]

(*!
	\function KeepRuleIfNotSequence
	
	\calltable
		KeepRuleIfNotSequence[rule] '' echoes the rule unless the rule's RHS is Sequence[], in which case Sequence[] is returned. Useful when doing pattern replacement to only add an option/rule if the related match was found.
	
	Example:
	
	{"a", "b"} /.
		{x_, Repeated[y_, {0, 1}]} :>
			{
				x,
				KeepRuleIfNotSequence["OptionalSecondElement" -> y]
			}
			
	===
	
	{"a", "OptionalSecondElement" -> "b"}
	
	Example:
	
	{"a"} /.
		{x_, Repeated[y_, {0, 1}]} :>
			{
				x,
				KeepRuleIfNotSequence["OptionalSecondElement" -> y]
			}
			
	===
	
	{"a"}

	Unit tests:

	RunUnitTests[WUtils`WUtils`KeepRuleIfNotSequence]

	\maintainer danielb
*)
Attributes[KeepRuleIfNotSequence] = {HoldAllComplete};
KeepRuleIfNotSequence[rule_] :=
	Module[{},
		If [!MatchQ[HoldComplete[rule], HoldComplete[Rule[_String]]],
			rule
			,
			Unevaluated[Sequence[]]
		]
	]

(*!
	\function TestIDsInTestFile
	
	\calltable
		TestIDsInTestFile[testFile] '' returns the TestIDs in a test file.
	
	Unit tests:

	RunUnitTests[WUtils`WUtils`TestIDsInTestFile]

	\maintainer danielb
*)
TestIDsInTestFile[testFile_] :=
	Block[{tests, testIds},
		(* Read in the test file and grab the Tests. *)
		tests =
			Cases[
				ReadList[testFile, HoldComplete[Expression]],
				HoldComplete[(s_Symbol)[___]] /; SymbolName[s] === "Test"
			];
			
		With[{testIdSymbol = testIDSymbol[]},
			Cases[
				tests,
				HoldPattern[testIdSymbol -> testId_] :> testId,
				Infinity
			]
		]
	];

(*!
	\function GetTestUiMetadata
	
	\calltable
		GetTestUiMetadata[cellObject] '' given a code CellObject (corresponding to a test in a notebook), looks at the previous cell to see if its a test UI. If it is, the test metadata is returned, providing the TestID, etc. If no test UI is found, then None is returned.
	   
	GetTestUiMetadata[myCellObject]
	
	===
	
	{"TestId" -> "ContextToFile-20150126-QUFW1C"}
	
	\related 'GetPreviousCell
	
	\maintainer danielb
*)
GetTestUiMetadata[cellObject_] :=
	Module[{data},
		data =
			Flatten[
				Cases[
					NotebookRead[GetPreviousCell[cellObject]],
					{"TestUiMarker", args___} :> {args},
					Infinity
				]
			];
			
		If [data === {},
			None
			,
			data
		]
	]

(*!
	\function GetPreviousCell
	
	\calltable
		GetPreviousCell[cellObject] '' given a CellObject, returns the previous CellObject in the notebook, or None if there is no previous cell.
	
	\maintainer danielb
*)
GetPreviousCell[cellObject_] :=
	With[{cells = Cells[InputNotebook[]]},
		With[{pos = Position[Cells[InputNotebook[]], cellObject]},
			If [pos =!= {},
				If [pos[[1, 1]] =!= 1,
				   cells[[pos[[1, 1]] - 1]]
				   ,
				   None
				]
				,
				$Failed
			]
		]
	]

(*!
	\function TabsOrSpaces
	
	\calltable
		TabsOrSpaces[context] '' given a context, returns either 'Tabs' or 'Spaces' to indicate what should be used for indentation. Makes use of $useTabsOrSpaces.
	
	Unit tests:

	RunUnitTests[WUtils`WUtils`TabsOrSpaces]

	\maintainer danielb
*)
TabsOrSpaces[context_] :=
	Module[{tabsOrSpaces},
		tabsOrSpaces = GetVariablePossiblyFromParentPackage[context, "$useTabsOrSpaces"];
		
		If [MissingQ[tabsOrSpaces],
			"Spaces"
			,
			tabsOrSpaces[[2]]
		]
	];

(*!
	\function LooksLikeCallSignature
	
	\calltable
		LooksLikeCallSignature[heldFuncCall, funcSymbol] '' given a function call wrapped in HoldComplete, returns True if it looks like the function call is actually a call signature.
	
	Examples:
	
	LooksLikeCallSignature[HoldComplete[CouldBeWLSymbolQ[str]], CouldBeWLSymbolQ] === True

	Unit tests:

	RunUnitTests[LooksLikeCallSignature]

	\related 'CreateUnitTests
	
	\maintainer danielb
*)
LooksLikeCallSignature[heldFuncCall_, funcSymbol_] :=
	(
	MatchQ[heldFuncCall, HoldComplete[funcSymbol[___Symbol]]] &&
	heldFuncCall /. HoldComplete[funcSymbol[args___]] :>
		(
		MatchQ[List[args], List[___Symbol]] &&
		StringMatchQ[
			ToString[List[args]],
			"{" ~~
			WhitespaceCharacter... ~~
			WLSymbolPattern[] ~~
			Repeated["," ~~ WhitespaceCharacter... ~~ WLSymbolPattern[], {0, Infinity}] ~~
			WhitespaceCharacter... ~~
			"}"
		]
		)
	)

(*!
	\function GetExpectedExpressionAndPossiblyModifyHeldFunctionCall
	
	\calltable
		GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[heldFunction] '' evaluates the held function call to determine the expected test result. In some cases, such as for things wrapped in TestHead, we adjust both the expected expression as well as the held function call.

	Examples:
	
	GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[HoldComplete[1 + 1]] === {2, HoldComplete[1 + 1]}

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetExpectedExpressionAndPossiblyModifyHeldFunctionCall]

	\maintainer danielb
*)
GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[heldFunction_] := {ReleaseHold[heldFunction], heldFunction}

GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[HoldComplete[TestHead[inner_]]] :=
	{
		True,
		With[{head = Head[ReleaseHold[inner]]},
			HoldComplete[
				MatchQ[inner, Blank[head]]
			]
		]
	}
	
GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[HoldComplete[AsTest[inner_]]] :=
	{
		ReleaseHold[inner],
		HoldComplete[inner]
	}
	
GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[HoldComplete[MUnit`TestExecute[inner_]]] :=
	{
		Null,
		HoldComplete[
			CompoundExpression[inner]
		]
	}

(*!
	\function EvaluateAndGetMessages
	
	\calltable
		EvaluateAndGetMessages[e] '' evaluates 'e' and returns a list of any messages that were issued.

	Examples:
	
	EvaluateAndGetMessages[1/0] === {ComplexInfinity, {Hold[Power::infy]}}

	Unit tests:

	RunUnitTests[WUtils`WUtils`EvaluateAndGetMessages]

	\maintainer danielb
*)
Attributes[EvaluateAndGetMessages] = {HoldFirst};
EvaluateAndGetMessages[e_] :=
	Block[{res, messages},
		messages =
			Flatten[
				Reap[
					(* #1 is the message text
					   #2 is Hold[_MessageName]
					   #3 is Hold[Message[_MessageName, args]] *)
					Internal`HandlerBlock[
						{
							"MessageTextFilter",
							Function[{text, messageName, messageWithArgs}, Sow[messageName, "Messages"]]
						},
						res = e
					],
					"Messages"
				][[2]],
				1
			];
		
		{res, messages}
	];

(*!
	\function RemoveHoldFromIndentedString
	
	\calltable
		RemoveHoldFromIndentedString[str, holdSymbol] '' given an expression that has been rendered as an indented string, and while is wrapped in a hold symbol, remove the hold symbol and adjust the indenting so that we are left with the inner expression.
	
	Example:
	
	RemoveHoldFromIndentedString[
		"HoldComplete[\n	Inner[\n		1,\n		2\n	]\n]",
		"HoldComplete"
	]
	
	===
	
	"Inner[\n	1,\n	2\n]"
	
	\maintainer danielb
*)
Clear[RemoveHoldFromIndentedString];
RemoveHoldFromIndentedString[str_, holdSymbol_String:"HoldComplete"] :=
	StringReplace[
		str,
		{
			(* Simple case: Single line. *)
			StartOfString ~~ preIndent : (WhitespaceCharacter...) ~~ ToString[holdSymbol] ~~ "[" ~~ inner : (Except[WhitespaceCharacter] ~~ ___) ~~ "]" ~~ EndOfString :>
				preIndent <> inner,
				
			(* Complex case: Multi-line. *)
			StartOfString ~~ preIndent : (WhitespaceCharacter...) ~~ ToString[holdSymbol] ~~ "[\n" ~~ indent:(Longest[WhitespaceCharacter..]) ~~ rest__ ~~ "\n" ~~ WhitespaceCharacter... ~~ "]" ~~ EndOfString :>
				StringReplace[
					preIndent <> rest,
					"\n" <> indent :> "\n" <> preIndent
				]
		}
	]

(*!
	\function AddTestToFile
	
	\calltable
		AddTestToFile[e, file] '' given an expression to evaluate and a test file, adds a test to the file. Uses the evaluated form of the expression as the expected value. Also adds a test cell to the notebook above the code cell that corresponds to the test to provide buttons like 'Update', 'Open in Workbench', etc.
	
	Likes in Utility.m so that it can be used by VaActions.m without Needs'ing Testing.m, which
	would load Examples.m, which would load Alpha. (and we don't want VaActions to load Alpha)
	
	\related 'CreateTest
	
	\maintainer danielb
*)
Attributes[AddTestToFile] = {HoldFirst};
Clear[AddTestToFile];
Options[AddTestToFile] =
{
	"Expected" -> Automatic,	(*< the expected expression. *)
	"ExpectedMessages" -> {},	(*< the expected messages, if any. *)
	"Contexts" -> {},		   (*< contexts to place on $ContextPath when rendering the test as a string. (affects which symbols are rendered with their full context) *)
	"Comment" -> None,		  (*< comment for this test. *)
	"CellObject" -> None,	   (*< the CellObject this test was extracted from. If specified, we set the TestID metadata of the cell to associated it with the test we create. *)
	"FunctionSymbol" -> None,   (*< the function being tested. *)
	"UseTabs" -> False			(*< Use tabs for indentation? *)
};
AddTestToFile[e_, file_, OptionsPattern[]] :=
	Module[{newTest, existing, expected = OptionValue["Expected"], cellObject,
			testDetails = NewHeldVar["testDetails"], cellId},
		
		If [!FileExistsQ[file], Print["AddTestToFile: Missing file: " <> file]; Return[$Failed]];
		
		existing = Import[file, "Text"];
		
		If [expected === Automatic,
			(* Lets 'e' evaluate. (the thing we're testing) *)
			expected = e;
			(* Use HoldComplete incase 'e' is Sequence[]. *)
			If [HoldComplete[e] === HoldComplete[$Failed],
				Print["Skipped: ", RemoveHoldFromIndentedString[ToString[HoldComplete[e], InputForm]], ": Returns $Failed"];
				Return[$Failed];
			];
		];
		
		cellObject = OptionValue["CellObject"]; 
		
		newTest =
			CreateTest[
				e,
				file,
				"Expected" -> expected,
				"ExpectedMessages" -> OptionValue["ExpectedMessages"],
				"Contexts" -> OptionValue["Contexts"],
				"Comment" -> OptionValue["Comment"],
				"TestDetails" -> testDetails,
				"UseTabs" -> OptionValue["UseTabs"]
			];
		
		If [cellObject =!= None,
			
			(* Mark the cell with a CellID. *)
			cellId = AddCellID[cellObject];
			
			(* Have to re-look-up code cell, since we just NotebookWrote a new one. *)
			cellObject = GetCodeCell[cellId];
			
			(* Add the TestId *)
			NotebookWrite[
				cellObject,
				Sett[NotebookRead[cellObject], "TestId" -> GetHeldVarKeyValue[testDetails, "TestId"]]
			];
			
			(* Have to re-look-up code cell, since we just NotebookWrote a new one. *)
			cellObject = GetCodeCell[cellId];
			
			(* Add a UI above the code cell to associate with its new test ID and to provide test related
			   functionality. *)
			SelectionMove[cellObject, Before, Cell];
			InsertTestCell[
				GetHeldVarKeyValue[testDetails, "TestId"],
				cellId,
				OptionValue["FunctionSymbol"],
				"SubTest" -> NotebookSubTest[]
			];
			
			(* Put the two test cells in the same group for improved
			   appearance of notebook. *)
			GroupTwoCells[cellObject];
		];
		
		If [StringLength[existing] <= StringLength[newTest] + 2 ||
			(* Quick check to make sure we're not adding a test multiple times
			   in a row. This only checks the end of the file, not further above. *)
			StringTake[existing, -StringLength[newTest]] =!= newTest,
			
			Export[file, existing <> "\n\n" <> newTest, "String"]
		];
	]

(*!
	\function CreateTest
	
	\calltable
		CreateTest[e, file] '' given an expression and a test file, creates a test. Uses the evaluated form of the expression as the expected value. Uses the file as the base name of the test ID. (doesn't modify the test file)
	
	Example:
	
	CreateTest[
	   1 + 1,
	   ToFileName[{$TemporaryDirectory}, "MyTestFile.m"]
	]
	
	===
	
	"Test[
		1 + 1
		,
		2
		,
		TestID -> "MyTestFile-20150120"
	]"

	Unit tests:

	RunUnitTests[WUtils`WUtils`CreateTest]

	\related 'AddTestToFile
	
	\maintainer danielb
*)
Attributes[CreateTest] = {HoldFirst};
Clear[CreateTest];
Options[CreateTest] =
{
	"Expected" -> Automatic,	(*< the expected expression. *)
	"ExpectedMessages" -> {},	(*< the expected messages, if any. *)
	"Contexts" -> {},		   (*< contexts to place on $ContextPath when rendering the test as a string. (affects which symbols are rendered with their full context) *)
	"Comment" -> None,		  (*< comment for this test. *)
	"TestId" -> Automatic,	  (*< the test ID to use. If none is specified, then one will be generated. *)
	"TestDetails" -> None,	  (*< if a held variable, the variable will be populated with key/value pairs representing test details, such as "TestId" *)
	"UseTabs" -> False			(*< Use tabs for indentation? *)
};
CreateTest[e_, file_, OptionsPattern[]] :=
	Module[{expected, commentStr = "", testId, res},
		
		expected =
			If [OptionValue["Expected"] =!= Automatic,
				OptionValue["Expected"]
				,
				e
			];
		
		If [OptionValue["TestId"] =!= Automatic,
			testId = OptionValue["TestId"];
			If [!StringQ[testId],
				Print["CreateTest: Invalid TestId: ", InputForm[testId]];
				Return[$Failed];
			];
			,
			testId = FileBaseName[file] <> "-" <> testIdDateForm[] <> "-" <> RandomDigitsOrLetters[];
		];
		
		If [OptionValue["TestDetails"] =!= None,
			SetHeldVarKeyValue[OptionValue["TestDetails"], "TestId" -> testId];
		];
			
		If [OptionValue["Comment"] =!= None,
			commentStr = "(* " <> OptionValue["Comment"] <> " *)\n";
		];
		
		res =
		Block[{$ContextPath = Join[{"System`", "Global`"}, OptionValue["Contexts"]]},
			commentStr <>
"Test[
" <> RemoveHoldFromIndentedString[Indent2[HoldComplete[e], 1, 4, "RemoveContexts" -> False]] <> "
	,
" <>
		With[{expected2 = MakeExpectedValueSystemIndependent[expected]},
			(* HoldComplete encase Sequence[] *)
			If [!FreeQ[HoldComplete[expected2], HeldExpectedValue],
				With[{expected3 = HoldComplete[expected2]},
					RemoveHoldFromIndentedString[
						Indent2[expected3 /. HeldExpectedValue[inner_] :> inner, 1, 4, "RemoveContexts" -> False]
					]
				]
				,
				(* HoldComplete encase Sequence[] *)
				RemoveHoldFromIndentedString[
					Indent2[HoldComplete[expected2], 1, 4, "RemoveContexts" -> False]
				]
			]
		] <>
		If [Length[OptionValue["ExpectedMessages"]] > 0,
			"\n	,\n	" <> ListOfHeldMessagesToString[OptionValue["ExpectedMessages"]]
			,
			""
		] <>
"
	,
	TestID -> \"" <> testId <> "\"
]"
		];
		
		If [TrueQ[OptionValue["UseTabs"]],
			res = StringReplace[res, "	" -> "\t"];
		];
		
		res
	]

testIdDateForm[] :=
	Block[{pad, year, month, day},
		{year, month, day} = Date[][[{1, 2, 3}]];
		pad = PaddedForm[#, 2, NumberSigns -> {"", ""}, NumberPadding -> {"0", ""}] &;
		ToString @ StringForm["`1``2``3`", year, pad @ month, pad @ day]
	]

RandomDigitsOrLetters[] :=
	ToUpperCase[
		StringJoin[
			If [# >= 10,
				FromCharacterCode[# + 87]
				,
				ToString @ #
			] & /@ RandomInteger[{0, 35}, 6]
		]
	]

(*!
	\function SetHeldVarKeyValue
	
	\calltable
		SetHeldVarKeyValue[heldVar, key -> value] '' given a held variable, sets the given key to the given value within it. If the heldVar is None, then no operation is performed. This can be useful for returning extra key/value pairs from a function via an option that specifies a held variable. If it is None, then that implies that the extra return values are not desired.
	
	Examples:
	
	Module[{heldVar = NewHeldVar["testVar"]},
		SetHeldVarKeyValue[heldVar, "MyKey" -> "MyValue"];
		ReleaseHold[heldVar]
	]

	Unit tests:

	RunUnitTests[WUtils`WUtils`SetHeldVarKeyValue]

	\related 'GetHeldVarKeyValue 'NewHeldVar
	
	\maintainer danielb
*)
SetHeldVarKeyValue[heldVar_, key_ -> value_] :=
	Module[{},
		If [heldVar =!= None,
			heldVar /. HoldComplete[var_] :>
				(
				If [!ValueQ[var],
					var = {};
				];
				var = Sett[var, key -> value];
				)
		];
	]

(*!
	\function ListOfHeldMessagesToString
	
	\calltable
		ListOfHeldMessagesToString[messages] '' converts a list of held message names to a string.

	Examples:
	
	ListOfHeldMessagesToString[{Hold[Power::infy], Hold[Power::infy]}] === "{Power::infy, Power::infy}"

	Unit tests:

	RunUnitTests[WUtils`WUtils`ListOfHeldMessagesToString]

	\maintainer danielb
*)
ListOfHeldMessagesToString[messages_] :=
	Block[{},
		StringReplace[
			ToString[messages, InputForm],
			"Hold[" ~~ Shortest[inner__] ~~ "]" :> inner
		]
	];

(*!
	\function AddCellID
	
	\calltable
		AddCellID[cellObject] '' annotates the Cell object that corresponds with the given CellObject with a CellID.

	Examples:
	
	AddCellID[cellObject] === 3243243
	
	\maintainer danielb
*)
AddCellID[cellObject_] :=
	Module[{id},
		NotebookWrite[
			cellObject,
			Sett[NotebookRead[cellObject], "CellID" -> (id = RandomInteger[100000000])]
		];
		
		id
	]

(*!
	\function GetCodeCell
	
	\calltable
		GetCodeCell[cellId] '' given a CellID, returns the given code cell.
	
	\maintainer danielb
*)
GetCodeCell[cellId_] :=
	Module[{matchingCells},
		
		matchingCells = Select[GetCodeCellObjects[], Gett[NotebookRead[#], CellID] === cellId &];
		
		If [matchingCells === {},
			$Failed
			,
			If [Length[matchingCells] > 1,
				Print["Warning: GetCodeCell: Multiple cells with CellID: ", cellId];
			];
			First[matchingCells]
		]
	]

(*!
	\function GetHeldVarKeyValue
	
	\calltable
		GetHeldVarKeyValue[heldVar, key] '' given a held variable, gets the given key. If the heldVar is None, then Missing[] is returned.
	
	Examples:
	
	Module[{heldVar = NewHeldVar["testVar"]},
		SetHeldVarKeyValue[heldVar, "MyKey" -> "MyValue"];
		GetHeldVarKeyValue[heldVar, "MyKey"]
	]

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetHeldVarKeyValue]

	\related 'SetHeldVarKeyValue
	
	\maintainer danielb
*)
GetHeldVarKeyValue[heldVar_, key_] :=
	Module[{},
		If [heldVar =!= None,
			Gett[ReleaseHold[heldVar], key]
			,
			Missing[]
		]
	]

(*!
	\function NotebookSubTest
	
	\calltable
		NotebookSubTest[] '' returns the sub-test file for this function notebook, or None if there isn't one.
	
	\maintainer danielb
*)
NotebookSubTest[] :=
	Block[{metadata},
		metadata = GetCellMetadata[InputNotebook[], "CreateIssueNotebookArgs"];
		If [ListQ[metadata],
			Gett[metadata, {"NotebookGeneratorArgs", "SubTest"}] /. Missing[] :> None
			,
			None
		]
	];

(*!
	\function GetCellMetadata
	
	\calltable
		GetCellMetadata[cellObjectOrNotebook, key] '' given a cell object, returns the TaggingRules value for the given key, or Missing[] if not set.
	
	Note: Can also be used to get a notebook's metadata.
	
	Examples:
	
	GetCellMetadata[
		myCellObject,
		"MyKey"
	]
	
	\related 'SetCellMetadata
	
	\maintainer danielb
*)
Clear[GetCellMetadata];
GetCellMetadata[cellObjectOrNotebook_, key_] :=
	Module[{val},
		val = CurrentValue[cellObjectOrNotebook, {TaggingRules, key}];
		If [val === Inherited,
			Missing[]
			,
			val
		]
	]

(*!
	\function InsertTestCell
	
	\calltable
		InsertTestCell[testId, cellId, funcSymbol, file] '' given a test ID, insert a cell into the notebook at the current selection position to indicate the test's ID and provide action buttons. This cell will be used when the "Add Tests to File" button is clicked to determine which code cells already correpond to tests in the file.
	
	Examples:
	
	InsertTestCell[
		"ContextToFile-20150126-QUFW1C",
		myCellId,
		ContextToFile
	]
	
	\related 'CreateUnitTests
	
	\maintainer danielb
*)
Clear[InsertTestCell];
Clear[InsertTestCell];
Options[InsertTestCell] =
{
	"SubTest" -> None					(*< The sub-test file, if any *)
};
InsertTestCell[testId_, cellId_, funcSymbol_, OptionsPattern[]] :=
	Module[{ui, test}, 
		With[{dynamicOutputVar = DynamicOutputSectionVar[], subTest = OptionValue["SubTest"]},
			NotebookWrite[
				InputNotebook[],
				Cell[
					BoxData[
						ToBoxes[
							ui =
							Framed[
								Grid[
									{
									{
									SmartButton[
										"Update"
										,
										
										(* I was marking the cell with something that could programmatically
										   be found to extract the test ID, but on second thought, I don't
										   think this is required because the below buttons can be setup
										   to receive the test ID as one of their arguments. *)
										{"TestUiMarker", "TestId" -> testId};
										
										test =
										   CellObjectToTest[
												funcSymbol,
												GetCodeCell[cellId],
												testId
											];
										
										RedirectPrintsAndMessagesToDynamicOutputSection[
											If [test === $Failed,
												Print["Couldn't find test's code cell."];
												,
												UpdateTest[
													UnitTestFilename[funcSymbol, "SubTest" -> subTest],
													testId,
													test
												]
											],
											dynamicOutputVar
										]
										,
										"Height" -> 25
									]
									,
									SmartButton[
										"Open in Workbench"
										,
										RedirectPrintsAndMessagesToDynamicOutputSection[
											OpenFileInWorkbench[
												UnitTestFilename[funcSymbol, "SubTest" -> subTest],
												"Substring" -> testId
											],
											dynamicOutputVar
										]
										,
										"Height" -> 25
									]
									,
									Row[
										{
										testId
										},
										ImageSize -> {Full, Automatic},
										Alignment -> {Right, Center}
									]
									}
									}
								],
								ImageSize -> {Full, Automatic},
								FrameStyle -> GrayLevel[0.8],
								FrameMargins -> 10
							];
							
							DynamicOutputSection[ui, dynamicOutputVar]
						]
					]
					,
					"TestUI",
					(*CellAutoOverwrite -> False,*)
					GeneratedCell -> False,
					CellMargins -> {{65, 0}, {0, 0}}
				]
			]
		]
	]

(*!
	\function DynamicOutputSectionVar
	
	\calltable
		DynamicOutputSectionVar[] '' creates a new variable to use as the reference to a dynamic output section. Returns it held. (so that once it takes on values, we don't lose our reference to the symbol itself)
	
	Examples:
	
	DynamicOutputSectionVar[] === HoldComplete[WUtils`WUtils`Private`DynamicOutputSectionVariable21637]
	
	\related 'DynamicOutputSection
	
	\maintainer danielb
*)
DynamicOutputSectionVar[] :=
	With[{var = Unique["WUtils`WUtils`Private`DynamicOutputSectionVariable" <> StringJoin[ToString /@ (Date[] /. n_Real :> Round[n])]]},
		HoldComplete[var]
	]

(*!
	\function CellObjectToTest
	
	\calltable
		CellObjectToTest[funcSymbol, cellObject, testId] '' given a code cell object, convert it into a Test.
	
	This is used when we want to update the test file for an already-existant
	test. We re-generate the test from the code cell during that process.
	
	\related 'GetFunctionUsesFromNotebook
	
	\maintainer danielb
*)
Clear[CellObjectToTest];
CellObjectToTest[funcSymbol_, cellObject_, testId_] :=
	Module[{codeCellExpressions, uses, file, funcUse, expectedExpression, heldFunctionCall, messages},
		
		file = UnitTestFilename[funcSymbol, "SubTest" -> OptionValue["SubTest"]];
		If [file === $Failed, Return[$Failed]];
		
		codeCellExpressions = GetCodeCellExpressions[{cellObject}];
		
		uses = GetFunctionUsesFromNotebookHelper[funcSymbol, codeCellExpressions, "IncludeAdditionalTestExpressions" -> True];
		
		If [Length[uses] >= 1,
			
			funcUse = uses[[1]];
			
			
			heldFunctionCall = funcUse[[1]];
			
			messages =
				EvaluateAndGetMessages[
					{
						expectedExpression,
						heldFunctionCall
					} =
						GetExpectedExpressionAndPossiblyModifyHeldFunctionCall[heldFunctionCall]
				][[2]];
			
			FunctionUse[heldFunctionCall, Sequence @@ funcUse[[2;;]]]
				/. FunctionUse[HoldComplete[e_], args___] :>
				(
				CreateTest[
					e,
					file,
					(* Pass along arguments like "Comment". *)
					Sequence @@
						FilterRules[
							{args},
							Options[CreateTest]
						],
					"TestId" -> testId,
					"Expected" -> expectedExpression,
					"ExpectedMessages" -> messages
				]
				)
			,
			$Failed
		]
	]

(*!
	\function UpdateTest
	
	\calltable
		UpdateTest[file, testId, testStr] '' given a test file, a test Id, and the updated test as a string, replace the given test in the given test file. Returns $Failed if the replacement could not be performed.
	
	Examples:
	
	UpdateTest[
		UnitTestFilename[ContextToFile],
		"ContextToFile-20150221-QQ56FF",
		"REPLACED!"
	]
	
	\maintainer danielb
*)
UpdateTest[file_, testId_, testStr_] :=
	Module[{commentPattern, check, contents, testPos, commentPositions, gapStr, testIdPosition, bestStart, bestEnd},
		
		If [!StringQ[testStr],
			Print["UpdateTest: The test should be a string. Specified test is: ", InputForm[testStr]];
			Return[$Failed];
		];
		
		If [!FileExistsQ[file],
			Print["UpdateTest: File not found: ", file];
			Return[$Failed, Module];
		];
		
		check = Check[contents = Import[file, "Text"], $Failed];
		If [check === $Failed,
			Print["UpdateTest: Messages while reading: ", file, ". Replacement not performed."];
			Return[$Failed, Module];
		];
		
		testIdPosition = StringPosition[contents, "TestID" ~~ Characters[" \t"]... ~~ "->" ~~ Characters[" \t"]... ~~ "\"" ~~ testId ~~ "\""];
			
		If [testIdPosition === {},
			Print["UpdateTest: Couldn't find test in file to update: ", testId];
			Return[$Failed, Module];
			,
			If [Length[testIdPosition] > 1,
				Print["UpdateTest: Found multiple matches of the target test. Aborting."];
				Return[$Failed, Module];
			];
		];
		
		testIdPosition = testIdPosition[[1]];
		
		bestStart = GetNearestPrecedingSpan[testIdPosition, StringPosition[contents, "\n" ~~ Characters[" \t"]... ~~ ("Test[" | "VerificationTest[")]];
		bestEnd = GetNearestTrailingSpan[testIdPosition, StringPosition[contents, "]"]];
		
		testPos = {bestStart[[1]], bestEnd[[2]]};
		
		If [StartsWithComment[testStr],
			commentPattern =
				RegularExpression["\\(\\* *((?:.|\\n)*?) *\\*\\)"];
				
			(* Comment positions, sorted by how close they are to our test. *)
			commentPositions =
				SortBy[
					StringPosition[contents, commentPattern],
					With[{dist = testPos[[1]] - #[[2]]},
						If [dist < 0,
							Infinity
							,
							dist
						]
					] &
				];
			
			(* Include the preceding comment? *)
			If [Length[commentPositions] > 0 && commentPositions[[1, 2]] < testPos[[1]],
	
				gapStr = StringTake[contents, commentPositions[[1, 2]] + 1 ;; testPos[[1]] - 1];
				
				If [StringMatchQ[gapStr, "\n\t "...],
					testPos = {commentPositions[[1, 1]], testPos[[2]]};
				];
			];
		];
		
		(* Make sure we don't eat up newlines. *)
		If [StringTake[contents, {testPos[[1]]}] === "\n",
			testPos = {testPos[[1]] + 1, testPos[[2]]}
		];
		
		Export[
			file,
			StringTake[contents, {1, testPos[[1]] - 1}] <>
			testStr <>
			StringTake[contents, {testPos[[2]] + 1, - 1}],
			"Text"
		];
	]

(*!
	\function GetNearestPrecedingSpan
	
	\calltable
		GetNearestPrecedingSpan[span, spans] '' returns the span that is the closest preceding span to 'span'.

	Examples:
	
	GetNearestPrecedingSpan[{10, 20}, {{1, 3}, {4, 8}, {12, 14}, {23, 25}, {33, 55}}]

	===

	{4, 8}

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetNearestPrecedingSpan]

	\maintainer danielb
*)
GetNearestPrecedingSpan[span_, spans_] :=
	SortBy[
		spans,
		With[{dist = span[[1]] - #[[2]]},
			If [dist < 0,
				Infinity
				,
				dist
			]
		] &
	]

(*!
	\function GetNearestTrailingSpan
	
	\calltable
		GetNearestTrailingSpan[span, spans] '' returns the span that is the closest preceding span to 'span'.

	Examples:
	
	GetNearestTrailingSpan[{10, 20}, {{1, 3}, {4, 8}, {12, 14}, {23, 25}, {33, 55}}]

	===

	{23, 25}

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetNearestTrailingSpan]

	\maintainer danielb
*)
GetNearestTrailingSpan[span_, spans_] :=
	SortBy[
		spans,
		With[{dist = #[[2]] - span[[2]]},
			If [dist < 0,
				Infinity
				,
				dist
			]
		] &
	]

(*!
	\function StartsWithComment
	
	\calltable
		StartsWithComment[str] '' returns true if the given string starts with a comment.

	Unit tests:

	RunUnitTests[WUtils`WUtils`StartsWithComment]

	\related '
	
	\maintainer danielb
*)
StartsWithComment[str_String] :=
	StringMatchQ[str, WhitespaceCharacter... ~~ "(*"]

(*!
	\function RedirectPrintsAndMessagesToDynamicOutputSection
	
	\calltable
		RedirectPrintsAndMessagesToDynamicOutputSection[e, var] '' given an expression to evaluate, evaluates it while capturing any Print output or message output, redirecting that output instead to a dynamic output section.
	
	Examples:
	
	With[{var = DynamicOutputSectionVar[]},
		DynamicOutputSection[
			SmartButton[
				"Click Me",
				RedirectPrintsAndMessagesToDynamicOutputSection[
					Print["Hello", " ", "again"];,
					var
				]
			],
			var
		]
	]

	Unit tests:

	RunUnitTests[WUtils`WUtils`RedirectPrintsAndMessagesToDynamicOutputSection]

	\related 'DynamicOutputSection
	
	\maintainer danielb
*)
Attributes[RedirectPrintsAndMessagesToDynamicOutputSection] = {HoldAllComplete};
Clear[RedirectPrintsAndMessagesToDynamicOutputSection];
Options[RedirectPrintsAndMessagesToDynamicOutputSection] =
{
	"ResetOutput" -> True,	  (* When this function evaluates its first argument, should it first reset the dynamic output section? For example, if this is running as part of a button's action, then we probably want to clear the output from the previous run. *)
	"IgnoreMessages" -> False   (* If True, then we'll actually ignore messages and only output Prints. *)
};
RedirectPrintsAndMessagesToDynamicOutputSection[e_, var_, OptionsPattern[]] :=
	Module[{},
		
		If [var === Null,
			(* This is a way for the caller to say, "actually, just evaluate this
			   because the output display isn't hooked up right now". *)
			Return[e];
		];
		
		If [TrueQ[OptionValue["ResetOutput"]],
			var /. HoldComplete[v_] :> (v = "")  
		];
		
		If [TrueQ[OptionValue["IgnoreMessages"]],
			(* Redirect Print *)
			Block[{
					Print = (DynamicOutputSectionPrint[var, ##]&),
					$Messages = {}
				  },
				e
			]
			,
			Quiet[
				(* Redirect Messages *)
				Internal`HandlerBlock[
					{
						"MessageTextFilter",
						(
						HoldComplete[##] /. HoldPattern[Message][_, args___] :>
							With[{res = 
									(
									With[{str = StringForm[#, args]},
										(* By the time this gets run, it's inside of the Block
										   below that redirects Print. *)
										(* For some reason, if we don't add the comma and double
										   quote as a second argument, the StringForm doesn't
										   end up evaluating. Not sure why. *)
										Print[str, ""]
									]
									)
								 },
								 
								 res /; True
							]
						) &
					},
					(* Redirect Print *)
					Block[{
							Print = (DynamicOutputSectionPrint[var, ##]&),
							$Messages = {}
						  },
						e
					]
				],
				(* Not sure why this occurs, but I'm tired of seeing it, and
				   I haven't noticed any functional issues with it. To reproduce
				   this message:
					ReloadVirtualAssistantFiles[];
					With[{var = DynamicOutputSectionVar[]},
						DynamicOutputSection[
							RedirectPrintsAndMessagesToDynamicOutputSection[
								RunUnitTests[DismissableOutput],
								var
							],
							var
						]
					]
				*)
				Internal`HandlerBlock::noevent
			]
		]
	]

(*!
	\function DynamicOutputSectionPrint
	
	\calltable
		DynamicOutputSectionPrint[heldVar, newOutput] '' prints some output to the dynamic output section.
	
	Examples:
	
	DynamicOutputSection["Here", HoldComplete[myVar]]
	DynamicOutputSectionPrint[HoldComplete[myVar], "Hello world!"];
	DynamicOutputSectionPrint[HoldComplete[myVar], "Hello world!"];
	
	\related 'DynamicOutputSection
	
	\maintainer danielb
*)
Clear[DynamicOutputSectionPrint];
DynamicOutputSectionPrint[heldVar_, newOutput___] :=
	Module[{newOutputRow},
			
		If [Length[{newOutput}] > 1,
			(* Like Print, if we are given multiple arguments,
			   concatenate them together into a Row. *)
			newOutputRow = Row[{newOutput}]
			,
			newOutputRow = newOutput;
		];
		
		heldVar /. HoldComplete[var_] :>
			(
			If [!ValueQ[var] || var === "",
				var = newOutputRow;
				,
				var = Column[{var, newOutputRow}];
			]
			)
	]

(*!
	\function SmartButton
	
	\calltable
		SmartButton[text, func] '' Creates a button of a better default height, and auto-sizes the width. Uses the "Method" -> "Queued" so that if it takes more than 5 seconds, it wont silently fail.
	
	\maintainer danielb
*)
Clear[SmartButton];
Options[SmartButton] =
{
	"Height" -> 40,			 (*< the height of the button *)
	"Width" -> Automatic		(*< the width of the button *)
};
Attributes[SmartButton] = {HoldAllComplete};
SmartButton[text_, func_, OptionsPattern[]] :=
	Button[
		text,
		func,
		ImageSize ->
			{
				If [OptionValue["Width"] === Automatic,
					If [!StringMatchQ[$SystemID, "Mac*"],
						35 + 7.5 * StringLength[text]
						,
						(* Mac sems to need more width per text. *)
						45 + 10 * StringLength[text]
					]
					,
					OptionValue["Width"]
				]
				,
				OptionValue["Height"]
			},
		(* Otherwise, if the computation takes more than 5 seconds or so, it is
		   silently killed. *)
		Method -> "Queued"
	]

(*!
	\function DynamicOutputSection
	
	\calltable
		DynamicOutputSection[primaryDisplay, heldVar] '' given some expression, returns it as its output for display in the notebook, but puts it inside of a Dynamic column and associates the second row with a variable that can be set to change the contents of the second row.
	
	I'm using this so that a row of buttons at the top of the notebook
	can produce output when clicked that is displayed, but which can be dismissed
	with an "OK" button, and which gets replaced if and when another button
	is clicked that has output.
	
	Examples:
	
	With[{var = DynamicOutputSectionVar[]},
		DynamicOutputSection[
			SmartButton["Click Me", DynamicOutputSectionPrint[var, "Test"]],
			var
		]
	]
	
	\related FindNotebookDynamicOutputVariable
	
	\maintainer danielb
*)
Clear[DynamicOutputSection];
Options[DynamicOutputSection] =
{
	"NakedSection" -> False,			(*< If True, then there is no "OK" button nor frame around the dynamic output section. *)
	"MaxHeight" -> Automatic			(*< The maximum height of the output section in pixels. If its content is larger then this, then a vertical scroll bar will be used. *)
};
DynamicOutputSection[primaryDisplayIn_, heldVar_, OptionsPattern[]] :=
	Module[{primaryDisplay = primaryDisplayIn
			(*prevValue = Null*)
			},

		heldVar /. HoldComplete[var_] :>
			( 
			primaryDisplay = primaryDisplay /. "DynamicOutputSectionVariable" :> var;
			var = "";
	
			(* Not sure why this "With" is required, but without it,
			   CreateIssueNotebook ends up with an unevaluated variable
			   displayed, rather than the contents of primaryDisplay. *)
			With[{primaryDisplay2 = primaryDisplay}, 
				Dynamic[
					
					Column[
						{
							primaryDisplay2
							,
							(* This 'var;' needs to be here so that once the kernel has been quit and
							   restarted, notebooks that display this dynamic content will see changes
							   to 'var'. Otherwise, because of how M figures out what variables to track,
							   it won't track 'var' on kernel restart, even with 'var' in TrackedSymbols.
							   Lou explained this in an email. *)
							var;
							If [ValueQ[var] && var =!= "",
								
								If [TrueQ[OptionValue["NakedSection"]],
									Framed[#, FrameStyle -> None, FrameMargins -> 2]
									,
									Framed[#, FrameStyle -> LightGray, Background -> White, FrameMargins -> 15]
								] & @
								
								Grid[
									{
										{
											Pane[
												var,
												"ImageSize" -> {Full, {0, OptionValue["MaxHeight"]}},
												Scrollbars -> Automatic,
												FrameMargins -> 0
											]
										},
										If [TrueQ[OptionValue["NakedSection"]],
											Sequence @@ {}
											,
											{Button["OK", var = "", ImageSize -> {100, 33}]}
										]
									},
									Spacings -> {0, 1},
									Alignment -> {Left, Top}
								]
								,
								Sequence @@ {}
							]
						}
					],
					TrackedSymbols :> {var}
				]
			]
			)
	]

(*!
	\function GroupTwoCells
	
	\calltable
		GroupTwoCells[firstCellObject] '' given a cell object, put it and the preceding cell into a group.
	
	\maintainer danielb
*)
GroupTwoCells[firstCellObject_] :=
	Module[{nb = InputNotebook[]},
		SelectionMove[firstCellObject, All, Cell];
		FrontEndTokenExecute[nb, "SelectPreviousLine"];
		FrontEndTokenExecute[nb, "CellGroup"];
	]

(*!
	\function EditFunctionMathdoc
	
	\calltable
		EditFunctionMathdoc[funcSymbol, testExpression, testExpectedExpression] '' edits the Mathdoc for the given function in the context of unit tests being created for the function. Adds instructions for how to run the unit tests, and adds an example input/output.
	
	testExpression: 			An example call to this function (wrapped in HoldComplete)
	testExpectedExpression:		The expected expression of the test when evaluated
	
	\related 'GetFunctionMathdoc
	
	\maintainer danielb
*)
Clear[EditFunctionMathdoc];
Options[EditFunctionMathdoc] =
{
	"SubstringMustNotExist" -> None,	(*< a substring that, if found in the Mathdoc, will imply that the edit should not occur. *)
	"ReplaceExampleIfExists" -> False   (*< replace the example even if it already exists? *)
};
EditFunctionMathdoc[funcSymbol_Symbol, testExpression_, testExpectedExpression_, OptionsPattern[]] :=
	Module[{mathdoc, file, source, newMathdoc},
		
		file = SymbolToFile[funcSymbol];
		If [file === $Failed, Print["EditFunctionMathdoc: Couldn't determine source file for function: ", funcSymbol]; Return[$Failed]];
		
		source = Import[file, "Text"];
		
		mathdoc = GetFunctionMathdoc[source, SymbolName[funcSymbol]];

		(* What to do if there is no Mathdoc? For now we
		   abort. *)
		If [mathdoc === Missing[],
			Return[$Failed];
		];
		
		newMathdoc = mathdoc;
		
		(* Add example input/output of the function. *)
		If [!MathdocContainsExampleQ[mathdoc] || TrueQ[OptionValue["ReplaceExampleIfExists"]],
			testExpression /. HoldComplete[e_] :>
				(
				newMathdoc =
					SetMathdocExample[
						newMathdoc,
						e,
						testExpectedExpression
					]
				);
		];
		
		(* Add instructions for running the unit tests. *)
		newMathdoc =
			EditFunctionMathdocHelper[
				newMathdoc,
				Block[{$Context = "Global`", $ContextPath = {"Global`", "System`"}},
					If [!StringFreeQ[ToString[funcSymbol], "CalculateParse`"],
						"	Unit tests:\n\n	RunUnitTests[" <> ToString[funcSymbol] <> "]\n\n"
						,
						(* RunUnitTests only exists in CalculateParse for now. *)
						"	Unit tests: " <> SymbolName[funcSymbol] <> ".mt\n\n"
					]
				],
				{
					"\\maintainer",
					"\\related"
				},
				"SubstringMustNotExist" -> "Unit tests:"
			];
		   
		newMathdoc = RemoveRelatedIfNotUsed[newMathdoc];
			
		StringReplaceInFiles[
			mathdoc,
			newMathdoc,
			file,
			"ErrorIfNotFound" -> True,
			(* Necessary so that we don't have \r\n vs \n confusion. *)
			"ImportAsTest" -> True
		]
	]

(*!
	\function GetFunctionMathdoc
	
	\calltable
		GetFunctionMathdoc[funcSymbol] '' given a function symbol, returns its Mathdoc comment, or $Failed if not successful.
		GetFunctionMathdoc[source, functionName] '' given some M source and a function name, returns the function's MathDoc, or Missing[] if not found.
	
	Examples:
	
	GetFunctionMathdoc[SymbolToFile]
	
	GetFunctionMathdoc[
		GetPackageSource["WUtils`WUtils`"],
		"GetFunctionMathdoc"
	]
	
	\maintainer danielb
*)
GetFunctionMathdoc[source_, functionName_] :=
	Module[{res},
		
		res =
			StringCases[
				source,
				"(*!" ~~ WhitespaceCharacter.. ~~
				"\\function" ~~ WhitespaceCharacter.. ~~ functionName ~~ WordBoundary ~~ Shortest[__] ~~
				"\n*)" 
			];
			
		If [res === {},
			Missing[]
			,
			res[[1]]
		]
	]

GetFunctionMathdoc[funcSymbol_] :=
	Module[{package},
		
		package = Context[funcSymbol];
		If [package === $Failed, Return[$Failed]];
		
		GetFunctionMathdoc[GetPackageSource[package], SymbolName[funcSymbol]]
	]

(*!
	\function GetPackageSource
	
	\calltable
		GetPackageSource[package] '' given an M package, reads and returns its source code as a string.
	
	Example:
	
	GetPackageSource["WUtils`WUtils`"]
	
	\maintainer danielb
*)
GetPackageSource[package_String] :=
	Module[{file},
		file = ContextToFile[package];
		If [file === $Failed, Return[$Failed]];
		StringReplace[Import[file, "String"], "\r\n" :> "\n"]
	]

GetPackageSource["Global`"] := ""
GetPackageSource["System`"] := ""

(*!
	\function MathdocContainsExampleQ
	
	\calltable
		MathdocContainsExampleQ[mathdoc] '' returns True if the given mathdoc contains an example.
	
	Examples:
	
	MathdocContainsExampleQ[mathdoc] === TODO

	Unit tests:

	RunUnitTests[WUtils`WUtils`MathdocContainsExampleQ]

	\related '
	
	\maintainer danielb
*)
MathdocContainsExampleQ[mathdoc_] :=
	Module[{exampleLabelPos, nextLine, nextLinePos},
		
		exampleLabelPos =
			StringPosition[mathdoc, "Examples:" | "Example:" | "\\example"];
			
		If [exampleLabelPos =!= {},
			
			nextLinePos = GetNextLine[mathdoc, exampleLabelPos[[1, 2]]];
			nextLine = StringTake[mathdoc, nextLinePos];
			
			If [!StringFreeQ[nextLine, WLSymbolPattern[] ~~ "["] &&
				StringFreeQ[nextLine, "TODO"],
				
				(* We found what looks to be a function call after the "Example:"
				   text, and it isn't marked with TODO, so we'll presume
				   this means there's an existing example. *)
				True
				,
				False
			]
			,
			(* Couldn't find a label like "Examples:" (etc) in the Mathdoc. *)
			False
		]
	]

(*!
	\function GetNextLine
	
	\calltable
		GetNextLine[str, pos] '' given a string and a position within that string, returns the next line. Skips past any empty or whitespace-only lines.
	
	Examples:
	
	GetNextLine["abc\ndef\nghi", 6] === "ghi"

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetNextLine]

	\maintainer danielb
*)
GetNextLine[str_, posIn_] :=
	Module[{char, pos = posIn, state, isNewlineChar, strLen, isWhitespaceChar},
		
		(* Finite state machine *)
		(* 0 -> Start state *)
		(* 1 -> Found newline(s) *)
		state = 0;
		
		strLen = StringLength[str];
		
		While[True,
			char = StringTake[str, {pos}];
			isNewlineChar = MatchQ[StringTake[str, {pos}], "\n" | "\r"];
			isWhitespaceChar = StringMatchQ[StringTake[str, {pos}], WhitespaceCharacter];
			
			Which[
				(state === 0 || state === 1) && isNewlineChar,
				state = 1;
				,
				state === 1 && !isWhitespaceChar,
				Return[GetLineAtPos[str, pos], Module];
			];
			
			If [pos === strLen,
				Return[{pos + 1, pos}, Module];
			];
			
			++pos;
		];
	]

(*!
	\function GetLineAtPos
	
	\calltable
		GetLineAtPos[str, pos] '' given a string and a position within that string, returns the position of the line. If the current position is a newline, returns the previous line.
	
	Examples:
	
	GetLineAtPos["abc\ndef\nghi", 6] === {5, 7}

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetLineAtPos]

	\maintainer danielb
*)
GetLineAtPos[str_, posIn_] :=
	Module[{pos = posIn},
		
		(* If the current position is a newline, returns the previous line. *)
		If [MatchQ[StringTake[str, {pos}], "\n" | "\r"],
			If [pos === 1 || MatchQ[StringTake[str, {pos - 1}], "\n" | "\r"],
				(* On the trailing newline of an empty line. *)
				(* We use this to mean "empty" *)
				Return[{pos + 1, pos}];
			];
			--pos;
		];
		
		{
			With[{prevNewlinePos = PreviousNewlineChar[str, pos]},
				If [prevNewlinePos === None,
					1
					,
					prevNewlinePos + 1
				]
			]
			,
			With[{nextNewlinePos = NextNewlineChar[str, pos]},
				If [nextNewlinePos === None,
					StringLength[str]
					,
					nextNewlinePos - 1
				]
			]
		}
	]

(*!
	\function SetMathdocExample
	
	\calltable
		SetMathdocExample[mathdoc, expression, output] '' given some Mathdoc, adds the given example expression and its corresponding output to demonstrate how the function works.

	Unit tests:

	RunUnitTests[WUtils`WUtils`SetMathdocExample]

	\related 'MathdocContainsExampleQ 'EditFunctionMathdocHelper
	
	\maintainer danielb
*)
Attributes[SetMathdocExample] = {HoldAllComplete};
SetMathdocExample[mathdoc_, expression_, output_] :=
	Module[{exampleLabelPos, stringToInsert, newMathdoc, nextLinePos, nextLine, existingExample},

		stringToInsert = ExampleToString[expression, output];

		exampleLabelPos =
			StringPosition[mathdoc, "Examples:" | "Example:" | "\\example"];
			
		If [exampleLabelPos =!= {},
			
			(* Although there is no example, there is a placeholder label
			   for where the example should go. *)
			   
			nextLinePos = GetNextLine[mathdoc, exampleLabelPos[[1, 2]]];
			nextLine = StringTake[mathdoc, nextLinePos];
			
			(* If the line following the example label has a
			   "TODO", then replace that line with our example. *)
			If [!StringFreeQ[nextLine, WordBoundary ~~ "TODO" ~~ WordBoundary],
				
				newMathdoc = StringReplacePart[mathdoc, stringToInsert, nextLinePos];
				,
				
				existingExample =
					StringCases[
						mathdoc,
						"Examples:" | "Example:" | "\\example" ~~ WhitespaceCharacter... ~~
						StartOfLine ~~ WhitespaceCharacter... ~~
						(* Function call *)
						(LetterCharacter... ~~ (LetterCharacter | DigitCharacter)... ~~ bracketed:("[" ~~ Shortest[___] ~~ "]") /; Length[StringPosition[bracketed, "["]] === Length[StringPosition[bracketed, "]"]]) ~~ WhitespaceCharacter... ~~
						"===" ~~ WhitespaceCharacter... ~~
						(
							(* Function call *)
							(LetterCharacter... ~~ (LetterCharacter | DigitCharacter)... ~~ bracketed2:("[" ~~ Shortest[___] ~~ "]") /; Length[StringPosition[bracketed2, "["]] === Length[StringPosition[bracketed2, "]"]])
							|
							(* Value on a single line *)
							Except["[" | "]" | "\n"]..
						)
					];
					
				If [ListQ[existingExample] && Length[existingExample] === 1,
					(* Replace the existing example. *)
					newMathdoc = StringReplace[mathdoc, existingExample :> StringTake[mathdoc, exampleLabelPos[[1]]] <> "\n\n" <> stringToInsert];
					,
					(* Otherwise, if we can't find a TODO line to replace,
					   just insert the example after the example label. *)
					newMathdoc =
						StringInsert[
							mathdoc,
							"\n\n" <> stringToInsert,
							exampleLabelPos[[1, 2]] + 1
						];
				];
			]
			
			,
			
			(* There is no placeholder label. *)
			
			newMathdoc =
				EditFunctionMathdocHelper[
					mathdoc,
					"	Example:\n\n" <> stringToInsert <> "\n\n",
					(* Put the examples above the first of these that are found,
					   or failing, that, at the very end. *)
					{
						"\\maintainer",
						"\\related"
					}
				];
		];

		newMathdoc
	]

(*!
	\function ExampleToString
	
	\calltable
		ExampleToString[expression, output] '' given a test expression and its expected output, produce the string that can be added to a Mathdoc comment to demonstrate the example.
	
	Examples:
	
	ExampleToString[Plus[1, 1], 2] === "	Plus[1, 1] === 2"

	Unit tests:

	RunUnitTests[WUtils`WUtils`ExampleToString]

	\related 'SetMathdocExample
	
	\maintainer danielb
*)
Clear[ExampleToString];
Attributes[ExampleToString] = {HoldAllComplete}
ExampleToString[expression_, output_] :=
	Module[{expressionStr, outputStr},
		
		expressionStr = Indent2[HoldComplete[expression], 1, 4, "RemoveHold" -> True];
		
		outputStr = Indent2[HoldComplete[output], 0, 4, "RemoveHold" -> True];
		
		If [StringFreeQ[expressionStr, "\n"] &&
			StringLength[expressionStr] <= 80 &&
			StringFreeQ[outputStr, "\n"] &&
			StringLength[outputStr] <= 80,
			
			(* Looks like we can put it on a single line. *)
			expressionStr <> " === " <> outputStr
			,
			(* We'll put it on multile lines *)
			expressionStr <>
			"\n\n	===\n\n" <>
			Indent2[HoldComplete[output], 1, 4, "RemoveHold" -> True]
		]
		
	]

(*!
	\function EditFunctionMathdocHelper
	
	\calltable
		EditFunctionMathdocHelper[mathdoc, newContent, aboveList] '' edits the Mathdoc, inerting the new content above all matches from 'aboveList'.
	
	Examples:
	
	EditFunctionMathdoc[
		GetFunctionMathdoc[
			Import[SymbolToFile[ContextToFile], "Text"],
			"ContextToFile"
		],
		"Unit tests:\n\nRunUnitTests[CouldBeWLSymbolQ]",
		{
			"\\maintainer",
			"\\related"
		}
	]
	
	\related 'EditFunctionMathdoc 'GetFunctionMathdoc
	
	\maintainer danielb
*)
Clear[EditFunctionMathdocHelper];
Options[EditFunctionMathdocHelper] =
{
	"SubstringMustNotExist" -> None	 (*< a substring that, if found in the Mathdoc, will imply that the edit should not occur. *)
};
EditFunctionMathdocHelper[mathdoc_String, newContent_String, aboveList_List, OptionsPattern[]] :=
	Module[{followingSectionPos, beforeText, afterText},
		
		If [OptionValue["SubstringMustNotExist"] =!= None &&
			!StringFreeQ[mathdoc, OptionValue["SubstringMustNotExist"]],
			
			(* The key string already exists. Don't edit the Mathdoc. *)
			Return[mathdoc];
		];
		
		followingSectionPos =
			GetMinStringPos[
				mathdoc,
				aboveList
			];
			
		If [followingSectionPos === None,
			followingSectionPos = StringLength[mathdoc - 1];
		];
		
		beforeText = StringTrim[StringTake[mathdoc, followingSectionPos - 1]];
		
		afterText = StringTake[mathdoc, {followingSectionPos, -1}];
		
		beforeText <> "\n\n" <>
		newContent <>
		If [followingSectionPos === None,
			""
			,
			"	"
		] <>
		afterText
	]

(*!
	\function RemoveRelatedIfNotUsed
	
	\calltable
		RemoveRelatedIfNotUsed[mathdoc] '' removes the "related '" boilerplate text from the mathdoc if it didn't end up being filled out.

	Unit tests:

	RunUnitTests[WUtils`WUtils`RemoveRelatedIfNotUsed]

	\maintainer danielb
*)
RemoveRelatedIfNotUsed[mathdoc_] :=
	StringReplace[
		mathdoc,
		Repeated["\n" ~~ Characters[" \t"]..., {0, 1}] ~~
		"\n" ~~ (preWs:Characters[" \t"]...) ~~
		"\\related" ~~ Characters[" \t"]... ~~ "'" ~~ Characters[" \t"]... ~~ "\n" ~~ WhitespaceCharacter... ~~
		post:___
		:>
			If [post === "*)",
				"\n" <> post
				,
				"\n\n" <> preWs <> post
			]
	]

(*!
	\function StringReplaceInFiles
	
	\calltable
		StringReplaceInFiles[from, to, files] '' perform the given string replacement on the given files. The 'from' can be a string pattern.
	
	WARNING: With great power comes great responsibility. Use with caution.
	
	Example:
	
	With[{file = FileNameJoin[{$TemporaryDirectory,"StringReplaceInFiles.m"}]},
		Export[file, "test", "String"];
		StringReplaceInFiles["test", "hello", file];
		With[{res = Import[file, "String"]},
			DeleteFile[file];
			res
		]
	]

	Unit tests:

	RunUnitTests[WUtils`WUtils`StringReplaceInFiles]

	\related 'StringReplace
	
	\maintainer danielb
*)
Clear[StringReplaceInFiles];
Options[StringReplaceInFiles] =
{
	"FilePattern" -> "*.*",			 (*< the types of files to search. *)
	"ErrorIfNotFound" -> False,		 (*< display an error if the text wasn't found? *)
	"NonGreedyStartOfMatch" -> False,   (*< avoid matching the first thing possible. Rather, match the latest thing possible. This is helpful when the pattern contains a __ or ___ and the earliest possible match would end up doing the wrong thing. *)
	"ImportAsTest" -> False				(*< Import[..., "Text"]? Otherwise we use Import[..., "String"] so that we don't loose newlinese from the end of a file. *)
};
StringReplaceInFiles[from_, to_String, files_, OptionsPattern[]] :=
	Module[{contents, check, pos, doReplacement},
		
		If [!MatchQ[files, {__String} | _String],
			Print["StringReplaceInFiles: Invalid 'files' argument: ", files];
			Return[$Failed];
		];
		
		(* Ensure all files exist. *)
		Function[{file},
			If [!FileExistsQ[file],
				Print["StringReplaceInFiles: File not found: ", file];
				Return[$Failed, Module];
			];
		] /@ Flatten[{files}];
		
		Function[{file},
			check = Check[contents = Import[file, If [TrueQ[OptionValue["ImportAsTest"]], "Text", "String"]], $Failed];
			If [check === $Failed,
				Print["StringReplaceInFiles: Messages while reading: ", file, ". Replacement not performed on that file."];
				,
				doReplacement = True;
				
				If [TrueQ[OptionValue["ErrorIfNotFound"]],
					pos = StringPosition[contents, from];
					
					If [pos === {},
						Print["StringReplaceInFiles: Couldn't find string to replace in file: ", file];
						doReplacement = False;
					];
				];
				
				If [TrueQ[doReplacement],

					If [TrueQ[OptionValue["NonGreedyStartOfMatch"]],
						check = Check[contents = StringReplace[contents, Longest[pre___] ~~ from :> pre <> to], $Failed];
						,
						check = Check[contents = StringReplace[contents, from :> to], $Failed];
					];
					
					If [check === $Failed,
						Print["StringReplaceInFiles: Messages while performing string replacement: ", file, ". Replacement not performed on that file."];
						,
						Export[file, contents, If [TrueQ[OptionValue["ImportAsTest"]], "Text", "String"]];
					];
				]; 
			];
		] /@ Flatten[{files}];
	]

(*!
	\function RunUnitTestsInNotebook
	
	\calltable
		RunUnitTestsInNotebook[funcSymbol] '' given a function symbol, runs its unit tests in the current notebook. Looks for the code cell that has RunUnitTests and evaluates it.
	
	Used by the "Run Tests" button in function notebooks created by Va["create function notebook"].
	
	\related 'CreateIssueNotebook
	
	\maintainer danielb
*)
RunUnitTestsInNotebook[funcSymbol_] :=
	Module[{cell},
		cell = GetCodeCellObjectsContaining[RunUnitTests];
		If [Length[cell] === 0,
			Print["Couldn't find cell containing call to RunUnitTests."];
			Return[$Failed];
			,
			cell = cell[[1]];
			
			SelectionMove[cell, All, Cell];
			
			SelectionEvaluate[InputNotebook[]];
		];
	]

(*!
	\function GetCodeCellObjectsContaining
	
	\calltable
		GetCodeCellObjectsContaining[pattern] '' returns the CellObjects in the current notebook whos expressions contain the given pattern.
	
	GetCodeCellObjectsContaining[MyExpression[_, _]]
	
	\related 'RunUnitTestsInNotebook
	
	\maintainer danielb
*)
GetCodeCellObjectsContaining[pattern_] :=
	Module[{codeCellObjects},
		codeCellObjects = GetCodeCellObjects[];
		
		Select[
			codeCellObjects,
			(
			Position[
				CodeCellObjectToExpression[#],
				pattern
			] =!= {}
			)
			&
		]
	]

(*!
	\function CodeCellObjectToExpression
	
	\calltable
		CodeCellObjectToExpression[cellObject] '' given a CellObject, returns its expression wrapped in HoldComplete.
	
	\maintainer danielb
*)
CodeCellObjectToExpression[cellObject_] :=
	Module[{e},
		e =
			ToExpression[
				NotebookRead[cellObject] /.
					Cell[BoxData[c_], ___] :> c,
				StandardForm,
				HoldComplete
			];
			
		If [MatchQ[e, _List],
			(* Get rid of 'Null' entries in code cells, which come after lines that have a semi-colon
			   at the end. *)
			DeleteCases[e, HoldComplete[Null]]
			,
			e
		]
	]

End[]

EndPackage[]