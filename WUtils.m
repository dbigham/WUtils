BeginPackage["WUtils`WUtils`"]

Needs["JLink`"];

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

CreateIssueNotebook::usage = "CreateIssueNotebook  "

GetClipboard::usage = "GetClipboard  "

ResolveIssueNotebook::usage = "ResolveIssueNotebook  "

IssuesDirectoryDefined::usage = "IssuesDirectoryDefined  "

AlternateNotebookDirectories::usage = "AlternateNotebookDirectories  "

DownValuesToList::usage = "DownValuesToList  "

GetSymbolContext::usage = "GetSymbolContext  "

StringDropByDelimiter::usage = "StringDropByDelimiter  "

NotebookDirectoryForFunction::usage = "NotebookDirectoryForFunction  "

NotebookDirectoryForContext::usage = "NotebookDirectoryForContext  "

DownValuesToRules::usage = "DownValuesToRules  "

NotebookTypeToDirectory::usage = "NotebookTypeToDirectory  "

OpenNotebook::usage = "OpenNotebook  "

CouldBeWLSymbolQ::usage = "CouldBeWLSymbolQ  "

ExtractUsesOfFunction::usage = "ExtractUsesOfFunction  "

FindMatchingBrace::usage = "FindMatchingBrace  "

GetLeftWhitespace::usage = "GetLeftWhitespace  "

GetReloadLine::usage = "GetReloadLine  "

ReloadFunction::usage = "ReloadFunction  "

GetFunctionArgumentNames::usage = "GetFunctionArgumentNames  "

FunctionCaptureButton::usage = "FunctionCaptureButton  "

EnableFunctionCapture::usage = "EnableFunctionCapture  "

ArgListToBindingList::usage = "ArgListToBindingList  "

CaptureFunctionCall::usage = "CaptureFunctionCall  "

CreateCapturedFunctionCell::usage = "CreateCapturedFunctionCell  "

GetFunctionCaptureNotebook::usage = "GetFunctionCaptureNotebook  "

ReplaceSymbolsUsingPatterns::usage = "ReplaceSymbolsUsingPatterns  "

HeldExpressionToString::usage = "HeldExpressionToString  "

RegisterFunctionCaptureNotebook::usage = "RegisterFunctionCaptureNotebook  "

DisableFunctionCapture::usage = "DisableFunctionCapture  "

EditFunction::usage = "EditFunction  "

GetLineNumber::usage = "GetLineNumber  "

EditUnitTests::usage = "EditUnitTests  "

NewFunctionCell::usage = "NewFunctionCell  "

MoveNotebook::usage = "MoveNotebook  "

NotebookX::usage = "NotebookX  "

GetScreenDimensions::usage = "GetScreenDimensions  "

EnsureDefined::usage = "EnsureDefined  "

DockedToolbar::usage = "DockedToolbar  "

ContextDirectory::usage = "ContextDirectory  "

InsertCodeCell::usage = "InsertCodeCell  "

CreateNotebookItem::usage = "CreateNotebookItem  "

ExtractedDockedContents::usage = "ExtractedDockedContents  "

CreateNotebook2::usage = "CreateNotebook2  "

DeCamelCase::usage = "DeCamelCase  "

CamelCaseQ::usage = "CamelCaseQ  "

CreateWorkingNotebook::usage = "CreateWorkingNotebook  "

ReplaceSymbols::usage = "ReplaceSymbols  "

HeldSymbolName::usage = "HeldSymbolName  "

RenderHeldListOfCodeLines::usage = "RenderHeldListOfCodeLines  "

SetCellMetadata::usage = "SetCellMetadata  "

RenderHeldFunctionIntoMultipleLines::usage = "RenderHeldFunctionIntoMultipleLines  "

SelectFileInExplorer::usage = "SelectFileInExplorer  "

GetSymbolPackage::usage = "GetSymbolPackage  "

KeyValueGet::usage = "KeyValueGet  "

Gett::usage = "Gett  "

KeyValueSet::usage = "KeyValueSet  "

Sett::usage = "Sett  "

RemoveIndentation::usage = "RemoveIndentation  "

CreateFunctionInFile::usage = "CreateFunctionInFile  "

ReloadFiles::usage = "ReloadFiles  "

CreateFunction::usage = "CreateFunction  "

CouldBeFunctionCall::usage = "CouldBeFunctionCall  "

FunctionWithNoArgs::usage = "FunctionWithNoArgs  "

AppendToFile::usage = "AppendToFile  "

GivePrivateSymbolsADefiniteContext::usage = "GivePrivateSymbolsADefiniteContext  "

MakePrivateSymbolContextDefinite::usage = "MakePrivateSymbolContextDefinite  "

FileToCustomPrivateContext::usage = "FileToCustomPrivateContext  "

WorkOn::usage = "WorkOn  "

RunUnitTests::usage = "RunUnitTests  "

RunUnitTestsUi::usage = "RunUnitTestsUi  "

RunTestFile::usage = "RunTestFile  "

RunTestFiles::usage = "RunTestFiles  "

UnitTestFilenames::usage = "UnitTestFilenames  "

UnitTestFilenamesImpl::usage = "UnitTestFilenamesImpl  "

HasUnitTests::usage = "HasUnitTests  "

RenameSymbolOrContext::usage = "RenameSymbolOrContext  "

RenameSymbolInTestFile::usage = "RenameSymbolInTestFile  "

StringReplaceUsesOfSymbol::usage = "StringReplaceUsesOfSymbol  "

RenameSymbolInNotebook::usage = "RenameSymbolInNotebook  "

ComposeEmail::usage = "ComposeEmail  "

ComposeEmailViaGmail::usage = "ComposeEmailViaGmail  "

UrlEncode::usage = "UrlEncode  "

FileSearchUI::usage = "FileSearchUI  "

FilesContaining::usage = "FilesContaining  "

SpacedRow::usage = "SpacedRow  "

WithTemporaryFiles::usage = "WithTemporaryFiles  "

TemporaryFile::usage = "TemporaryFile  "

ReplaceHeldExpressions::usage = "ReplaceHeldExpressions  "

TemporaryFilesBlock::usage = "TemporaryFilesBlock  "

TemporaryDirectory::usage = "TemporaryDirectory  "

StringToSymbol::usage = "StringToSymbol  "

CapturePrint::usage = "CapturePrint  "

TestHead::usage = "TestHead  "

EditFile::usage = "EditFile  "

ResolveFile::usage = "ResolveFile  "

Person::usage = "Person  "

SetEvaluationTarget::usage = "SetEvaluationTarget  "

EvaluateEvaluationTarget::usage = "EvaluateEvaluationTarget  "

DeleteCurrentNotebook::usage = "DeleteCurrentNotebook  "

CreateProject::usage = "CreateProject  "

CreateSourceFile::usage = "CreateSourceFile  "

MessageFail::usage = "MessageFail  "

CreateDirectoryIfDoesntExist::usage = "CreateDirectoryIfDoesntExist  "

WriteFileIndented::usage = "WriteFileIndented  "

EnsureFileLoaded::usage = "EnsureFileLoaded  "

Second::usage = "Second  "

WriteFile::usage = "WriteFile  "

FocusInputFieldDelayed::usage = "FocusInputFieldDelayed  "

NotEmpty::usage = "NotEmpty  "

EmptyQ::usage = "EmptyQ  "

LoadFile::usage = "LoadFile  "

FirstIndex::usage = "FirstIndex  "

ToCamelCase::usage = "ToCamelCase  "

CreateInputField::usage = "CreateInputField  "

SimpleReap::usage = "SimpleReap  "

ReplaceRange::usage = "ReplaceRange  "

AddOptions::usage = "AddOptions  "

LastStringPosition::usage = "LastStringPosition  "

InsertString::usage = "InsertString  "

AddOptionsHelper::usage = "AddOptionsHelper  "

Gettt::usage = "Gettt  "

Begin["`Private`"]

(* Handy for disabling Print statements. Ensures that their arguments will no
   longer evaluate when they are disabled so that they don't slow the code down. *)
Attributes[XPrint] = {HoldAllComplete};

(* Used by createWorkingNotebookReplacements. Need to be defined
   prior to other functions using naked symbols below like
   'CodeString', otherwise they'll be private when we're
   expecting them to not be private. *)
WUtils`WUtils`Section;
WUtils`WUtils`Subsection;
WUtils`WUtils`Subitem;
WUtils`WUtils`Code;
WUtils`WUtils`CodeString;
WUtils`WUtils`Output;
WUtils`WUtils`BoxesCell;

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
	- If you have something like Func[1][[1]], then the [[1]] is ignored because
	  this function is happy to have found Func[1].
	
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
commentPattern[] := RegularExpression["\\(\\*\\s*(.*?)\\s*\\*\\)"];

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
		
		dependencies = FunctionDependencies[func, "SymbolsToPrune" -> OptionValue["SymbolsToPrune"]];
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
		"Global`",
		"JLink`",
		"NETLink`",
		"FrontEnd`",
		"DataPaclets`",
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
			ImageSize -> {{800, 6000}, {100000, 100}}
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
	
	GetLineNumberOfPos["abc\ndef", 5] === 2

	\maintainer danielb
*)
GetLineNumberOfPos[str_String, pos_Integer] :=
	Length[
		StringCases[
			StringTake[str, {1, pos}],
			"\n"
		]
	] + 1

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
		FileNameDrop[FindFile["WUtils`WUtils`"], -1]
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
							(DateDifference[reloadFunctionCreationTimestamp, FileDate[file, "Modification"]] /. Quantity[d_, _] :> d) > 0 &&
							(
								(* Either not yet reloaded. *)
								!MatchQ[prevReloadTimestamp[file], _DateObject | _List] ||
								(* Or reloaded, but prior to the last modification. *)
								(DateDifference[prevReloadTimestamp[file], FileDate[file, "Modification"]] /. Quantity[d_, _] :> d) > 0
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
NewHeldVar[] := NewHeldVar["Unique"]
NewHeldVar[baseName_] :=
	(* We don't just use unique, we also use the current timestamp
	   to lessen the likelihood of collisions. For example, if someone
	   emailed you a notebook with dynamic modules that contained instances
	   of these held variables that collided with ones used by dynamic modules
	   in other notebooks currently open. *)
	With[{var = Unique["WUtils`WUtils`Private`NewVar`" <> baseName <> ToString[Ceiling[AbsoluteTime[]]]]},
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
			If [ModificationAgeInMinutes[file] < Quantity[10, "Minute"],
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
			If [ModificationAgeInMinutes[file] < Quantity[10, "Minute"],
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
					MyFunc[],
					1 + 1
				]
			]
		}
	]
	
	===
	
	{
		HoldComplete["Comment" -> Comment["Test comment."]],
		HoldComplete[MyFunc[]],
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

	Unit tests: GetFunctionUsesFromNotebookHelper.mt

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
	
	That allows a package such as WUtils` to define a unit test directory,
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
	Module[{relativePath},
		
		relativePath = MakePathRelativeToPaths[file];
		
		If [!StringQ[relativePath],
			Print["UnitTestTemplate: Couldn't determine relative path for file: " <> file];
			Return[$Failed];
		];

"(* Tests for: " <> Context[funcSymbol] <> Last[StringSplit[ToString[funcSymbol], "`"]] <> "

   Author: " <> Username[] <> "\n*)"
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
		With[{expected2 = expected},
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
	][[1]]

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
	][[1]]

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
					"	Unit tests:\n\n	RunUnitTests[" <> ToString[funcSymbol] <> "]\n\n"
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

(*!
	\function CreateIssueNotebook
	
	\calltable
		CreateIssueNotebook[] '' creates a notebook. The name of the notebook is gotten from the clipboard if it isn't specified explicitly.
	
	\related 'CreateNotebook
	
	\maintainer danielb
*)
Clear[CreateIssueNotebook];
Options[CreateIssueNotebook] =
{
	"Name" -> Automatic,					(*< the name of the notebook. If Automatic, the clipboard will be considered. *)
	"Type" -> Automatic,					(*< the type of notebook to create. *)
	"CreateFile" -> True,				   (*< actually create the notebook? Otherwise, just creates the notebook but doesn't actually save it. *)
	"ReplaceInputCellWithNotebookLinkUponLinguisticsBeingDefined" -> False,
											(*< If the user enters a linguistic like "new nb 'My Notebook'", and they then define linguistics for that notebook, should we replace the cell of the notebook that prompted the creation of the notebook with a new Mini VA UI that has the linguistic for that notebook? *)
	"NotebookGeneratorArgs" -> {}			(*< Arguments for the particular type of notebook. *)
};
CreateIssueNotebook[opts:OptionsPattern[]] :=
	Module[{name = OptionValue["Name"],
			type = OptionValue["Type"],
			typeArg, clipboard,
			contents = Automatic,
			dir = Automatic,
			createSubDirectory = True, dockedContents, nb, dynamicOutputVar, path, title},
		
		ReloadFiles[];
		
		If [name === Automatic,
			clipboard = GetClipboard[];
			If [StringQ[clipboard] && StringLength[clipboard] >= 1 && StringLength[clipboard] <= 80,
				name = clipboard;
				,
				respond["Please copy the notebook name to your clipboard first."];
				Return[$Failed, Module];
			];
		];
		
		If [!StringFreeQ[name, " "],
			(* If there are spaces in the name, set the title of the notebook
			   explicitly to that, and set the name to be a camel cased version. *)
			title = name;
			name = toCamelCase[title];
			,
			title = Automatic;
		];
		
		(* Can be used to redirect output (ex. generated when clicking a button) to a particular place in the notebook. *)
		dynamicOutputVar = DynamicOutputSectionVar[];
		
		Switch[type,
			"Sub",
			(* A 'sub' notebook of the current notebook. *)
			dir = NotebookDirectory[InputNotebook[]];
			createSubDirectory = True;
			,
			"Function",
			With[{resolvedNotebook = ResolveIssueNotebook[name]},
				If [StringQ[resolvedNotebook],
					dir = DirectoryName[resolvedNotebook];
					,
					With[{notebookGeneratorArgs = OptionValue["NotebookGeneratorArgs"]},
						With[{funcName = If [!MissingQ[Lookup[notebookGeneratorArgs, "Function"]], ToString[Lookup[notebookGeneratorArgs, "Function"]], name]},
							dir = NotebookDirectoryForFunction[ToExpression[GetSymbolContext[funcName] <> funcName]];
						];
					]
				]
			]
		];
		
		If [dir === Automatic,
			dir = NotebookTypeToDirectory[type];
		];
		
		(* If they specified an actual file name with extension as the notebook name,
		   remove the extension, because it will be added back on later. *)
		If [StringMatchQ[name, __ ~~ ".nb"],
			name = StringTake[name, {1, -4}];
		];
		

		If [dir === Automatic,
			If [!IssuesDirectoryDefined[], Return[$Failed]];
			dir = Global`$NotebookDirectory;
		];
		
		path = ToFileName[{dir}, name <> ".nb"];
		
		If [FileExistsQ[path],
			(* The notebook already exists in the main issues directory. Open it instead. *)
			Return[OpenNotebook[path]];
		];
		
		typeArg = Sequence[];
		If [type =!= Automatic,
			(* Otherwise, perhaps notebooks of this type are generated dynamically. *)
			contents = NotebookGenerator[name, type, "DynamicOutputVar" -> dynamicOutputVar, Sequence @@ OptionValue["NotebookGeneratorArgs"]];
		];
		
		If [contents === $Failed, Return[$Failed]];
		
		(* The default notebook contents if the notebook type isn't something like
		   "bug notebook" or "function notebook". *)
		If [contents === Automatic,
			contents =
				{
					defaultNotebookButtons[dynamicOutputVar]
					(* TODO: UI for specifying linguistics for this notebook *)
				};
		];
		
		{contents, dockedContents} = ExtractedDockedContents[contents];
		
		If [TrueQ[OptionValue["ReplaceInputCellWithNotebookLinkUponLinguisticsBeingDefined"]],
			SelectionMove[FrontEndExecute[FrontEndToken["FindEvaluatingCell"]], All, Cell];
			
			NotebookDelete[First[SelectedCells[]]];
			
			(* TODO: Display a linguistic UI *)
			
			SelectionMove[InputNotebook[], All, Cell];
			$MiniVaUiCell = First[SelectedCells[]];
		];
		
		nb =
			CreateNotebook2[
				name,
				typeArg,
				"Title" -> title,
				"Type" -> OptionValue["Type"],
				"Directory" -> dir,
				"CreateSubDirectory" -> createSubDirectory,
				"DockedContents" -> dockedContents,
				"Contents" -> contents,
				"CreateFile" -> OptionValue["CreateFile"]
			];
						
		If [TrueQ[OptionValue["CreateFile"]],
			path = NotebookFileName[nb];
			(* TODO: Set context to represent that we just created a notebook. *)
		];
		 
		SetCellMetadata[nb, "CreateIssueNotebookArgs" -> {opts}];
			
		nb
	];

(* NOTE: I think I've observed an issue whereby this can seeminly hang sometimes. (if the clipboard doesn't contain simple text, perhaps?) *)
GetClipboard[] :=
	(
		(* For now we'll strip double quotes if they are present, such 
		   as copying an M string into the clipboard and wanting the
		   contents of the string to be used, not the literal string. *)
		LoadJavaClass["com.danielbigham.Util"];
		With[{clipboard = Util`getClipboard[]},
			If [StringMatchQ[clipboard, "\"" ~~ ___ ~~ "\""],
				ToExpression[clipboard]
				,
				clipboard
			]
		]
	)

(*!
	\function toCamelCase
	
	\calltable
		toCamelCase[str] '' converts a string to camel case by removing spaces and upper casing the appropriate letters.

	Examples:
	
	toCamelCase["just testing"] === "JustTesting"
	
	\maintainer danielb
*)
toCamelCase[str_] := StringReplace[str,  StartOfString | Whitespace .. ~~ a_ :> ToUpperCase[a]]

(*!
	\function ResolveIssueNotebook
	
	\calltable
		ResolveIssueNotebook[name] '' given a notebook name, try and resolve the file's path. If a directory contains multiple matching notebooks, a list is returned. If the notebook can't be found, $Failed is returned.
	
	Examples:
	
	ResolveIssueNotebook["MyNotebook"]
	
	\related 'OpenIssueNotebook
	
	\maintainer danielb
*)
ResolveIssueNotebook[nameIn_] :=
	Module[{dir, dir2, name = nameIn, file, nameWithoutExtension},
		
		If [!IssuesDirectoryDefined[], Return[$Failed]];
		dir = Global`$NotebookDirectory;

		If [!StringMatchQ[name, __ ~~ ".nb"],
			nameWithoutExtension = name;
			name = name <> ".nb";
			,
			nameWithoutExtension = StringReplace[name, s__ ~~ ".nb" ~~ EndOfString :> s];
		];
		
		(* Consider whether this is a sub-notebook. ie. A notebook that is
		   within a subdirectory of the current notebook. *)
		If [InputNotebook[] =!= $Failed,
			dir2 = Quiet[NotebookDirectory[InputNotebook[]], NotebookDirectory::nosv];
			If [dir2 =!= $Failed,
				file = FileNameJoin[{dir2, nameWithoutExtension, name}];
				If [!FileExistsQ[file],
					file = FileNameJoin[{dir2, name}];
				];
			];
		];
		
		If [!StringQ[file] || !FileExistsQ[file],
			(* Check the issues directory. *)
			file = ToFileName[{dir}, name];
		];
		
		(* If that doesn't exist, try inside of a directory of the same name
		   as the notebook. *)
		If [!FileExistsQ[file] || DirectoryQ[file],
			dir = ToFileName[{dir, nameWithoutExtension}];
			file = ToFileName[dir, name];
		];
		
		If [!FileExistsQ[file],
			(* If there is a single notebook in the directory, open it. *)
			With[{notebooks = FileNames["*.nb", dir]},
				If [Length[notebooks] === 1,
					file = notebooks[[1]];
					,
					If [Length[notebooks] > 1,
						file = notebooks;
					];
				];
			];
		];
		
		If [!FileExistsQ[file],
			
			(* Inspect NotebookTypeToDirectory for other directories that
			   notebooks might be in. *)
			Block[{},
				Function[{dir2},
					
					(* First check the issues directory. *)
					file = ToFileName[{dir2}, name];
					
					If [FileExistsQ[file],
						Return[Null, Block];
					];
					
					dir = ToFileName[{dir2}, nameWithoutExtension];
					
					(* If that doesn't exist, try inside of a directory of the same name
					   as the notebook. *)
					If [DirectoryQ[dir],
						dir = ToFileName[{dir2, nameWithoutExtension}];
						file = ToFileName[dir, name];
						If [FileExistsQ[file],
							Return[Null, Block];
						];
					];
					
				] /@ AlternateNotebookDirectories[];
			]
		];
		
		If [FileExistsQ[file] || ListQ[file],
			Return[file]
			,
			$Failed
		]
	]

(*!
	\function IssuesDirectoryDefined
	
	\calltable
		IssuesDirectoryDefined[] '' ensure the $NotebookDirectory is defined, exists, and is a directory.
	
	This global variable defines where we should create new working notebooks.
	(and the directories that contain them)
		
	\maintainer danielb
*)
IssuesDirectoryDefined[] :=
	If [!StringQ[Global`$NotebookDirectory],
		Print["$NotebookDirectory must be defined. It specifies where new notebooks/directories should be created."];
		False
		,
		If [!FileExistsQ[Global`$NotebookDirectory],
			Print["$NotebookDirectory does not exist. (" <> Global`$NotebookDirectory <> ")"];
			False
			,
			If [!DirectoryQ[Global`$NotebookDirectory],
				Print["$NotebookDirectory exists but is not a directory. (" <> Global`$NotebookDirectory <> ")"];
				False
				,
				True
			]
		]
	];

(*!
	\function AlternateNotebookDirectories
	
	\calltable
		AlternateNotebookDirectories[] '' returns the list of notebook directories implied by NotebookTypeToDirectory. ie. What directories might a notebook be in if it were created by the "new <type> notebook ..." linguistic?
	
	\related 'CreateIssueNotebook 'OpenIssueNotebook
	
	\maintainer danielb
*)
AlternateNotebookDirectories[] :=
	Append[
		Cases[
			Join[
				DownValuesToList[NotebookTypeToDirectory][[All, 2]],
				DownValuesToList[NotebookDirectoryForContext][[All, 2]]
			],
			_String
		],
		"E:\\Users\\Daniel\\Desktop\\NotebooksWithLinguistics"
	];

(*!
	\function DownValuesToList
	\maintainer danielb 
	
	\calltable
		DownValuesToList[symbol_, selector_] '' given a symbol that has down values, this function converts the down values into lists of pairs. Each pair consists of a key and a value. Optionally, a function can be passed in that selects which key/value pairs to export.
*)
Clear[DownValuesToList];
Options[DownValuesToList] =
{
	"Selector" -> ((#[[1]] =!= Blank[])&),	  (*< a function that, given a key/value pair, returns true if it should be included. *)
	"Transform" -> Identity					 (*< a function that is passed a key/value pair and modifies it before storage. ex. Rounding off decimal places. *)
};
DownValuesToList[symbol_, OptionsPattern[]] :=
	Sort[
		OptionValue["Transform"] /@
			(
			{#[[1]], ReleaseHold[#[[2]]]} & /@
				Select[
					(* Get the key as a list. *)
					Flatten[
						With[{rawKey = Replace[#[[1]], a_[b_[x___]] :> List[x]]},
							(* If the key is a list of size one (ie. one arg to the down value),
							   then don't store the key as a list. *)
							Function[{thisRawKey},
								With[{key = If [Length[rawKey] === 1, thisRawKey[[1]], thisRawKey]}, 
									{
										(* The key. *)
										key,
										(* The value associated with the key. We need to wrap it in hold for now incase
										   it's not inert. We'll remove the hold later. *)
										Extract[#, 2, Hold]
									}
								]
							] /@ ExpandAlternatives[rawKey]
						] & /@ DownValues[symbol]
						,
						1
					]
					,
					OptionValue["Selector"]
				]
			)
	]

(*!
	\function ExpandAlternatives
	\maintainer danielb 
	
	\calltable
		ExpandAlternatives[eIn_] '' given an expression, returns a list of expressions whereby Alternatives have been expanded.
		
	Example:
	
	ExpandAlternatives[{"a" | "b", "c"}] = {{"a", "c"}, {"b", "c"}}
*)
ExpandAlternatives[eIn_] :=
	Module[{e = {eIn}, pos},
		pos = Position[e, _Alternatives];
		If [pos === {},
			e
			,
			With[{alt = List @@ Extract[e, pos[[1]]]},
				Flatten[ExpandAlternatives /@ (First[ReplacePart[e, pos[[1]] -> #]] & /@ alt), 1]
			]
		]
	]

(*!
	\function GetSymbolContext
	
	\calltable
		GetSymbolContext[symbolName] '' given a symbol (in string form), returns its likely context, which might be a Private context.
	
	The function works by first checking if the symbol is Global` or resolvable
	by the $ContextPath. If not, it checks whether it is a Private symbol
	in one of the packages on the $ContextPath.

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetSymbolContext]

	\related 'GetSymbolPackage
	
	\maintainer danielb
*)
GetSymbolContext[symbolName_String] :=
	Module[{context, expr, isGlobal},

		isGlobal = Names["Global`" <> symbolName] =!= {};
		If [TrueQ[isGlobal],
			Return["Global`"];
		];

		expr = ToExpression[symbolName, StandardForm, HoldComplete];

		(* Check whether it's on the $ContextPath. *)
		expr /. HoldComplete[sym_] :>
			(
				context = Context[sym];
				
				If [context === "Global`",
					(* If the symbol shows up as in Global`, but it
					   wasn't there when we checked earlier, then
					   that means we created it. Remove it. *)
					If [!TrueQ[isGlobal],
						Remove[sym];
					]
					,
					(* We found the symbol on the $ContextPath. *)
					Return[context];
				];
			);

		(* Otherwise, we haven't been able to find the
		   symbol. As a last ditch effort, examine the
		   private contexts... *)
		Function[{thisContext},
			With[{possiblePrivateSymbol = thisContext <> "Private`" <> symbolName},
				If [Names[possiblePrivateSymbol] =!= {},
					Return[StringDropByDelimiter[possiblePrivateSymbol, "`"] <> "`", Module];
				];
			];
			(* New package format. *)
			With[{possiblePrivateSymbol = thisContext <> "PackagePrivate`" <> symbolName},
				If [Names[possiblePrivateSymbol] =!= {},
					Return[StringDropByDelimiter[possiblePrivateSymbol, "`"] <> "`", Module];
				];
			];
		] /@ $ContextPath;

		None
	];

StringDropByDelimiter[str_, delim_] := StringJoin[Riffle[Drop[StringSplit[str, delim], -1], delim]]

(*!
	\function NotebookDirectoryForFunction
	
	\calltable
		NotebookDirectoryForFunction[sym] '' given a function symbol, what directory should we put working notebooks?
	
	\related 'NotebookDirectoryForContext
	
	\maintainer danielb
*)
NotebookDirectoryForFunction[sym_] :=
	Module[{res, contexts, symbolContext},
		
		symbolContext = Context[sym];
		
		res = NotebookDirectoryForContext[symbolContext];
		
		If [res === Automatic,
			contexts = Cases[DownValuesToRules[NotebookDirectoryForContext][[All, 1]], _String];
			Function[{context},
				If [StringStartsQ[symbolContext, context],
					Return[NotebookDirectoryForContext[context], Module];
				];
			] /@ contexts;
			
			Automatic
			,
			res
		]
	]

(*!
	\function NotebookDirectoryForContext
	
	\calltable
		NotebookDirectoryForContext[context] '' given a context, what directory should we put working notebooks?

	Can be used to override the default notebook directory for
	working notebooks. For example, I'm working on a project
	where I have files in my personal CVS directory, and I'd
	prefer function notebooks to go in a directory there, so
	I can set a down value appropriately.
	
	\related 'NotebookDirectoryForFunction
	
	\maintainer danielb
*)
NotebookDirectoryForContext[context_] := Automatic

DownValuesToRules[f_Symbol] := Replace[DownValues[f], (Verbatim[HoldPattern][_[a_]] :> b_) :> (a :> b), {1}]

NotebookTypeToDirectory[_] := Automatic;

(*!
	\function OpenNotebook
	
	\calltable
		OpenNotebook[file] '' opens the given notebook. If the notebook looks like a name, we try to resolve the path of the notebook.
		
	\maintainer danielb
*)
Clear[OpenNotebook];
Options[OpenNotebook] =
{
	"Evaluate" -> False,	(*< evaluate the notebook's contents? Not compatible with non-default MathematicaVersion option. *)
	"Return" -> False		(*< return the notebook? *)
};
OpenNotebook[pathIn_, opts:OptionsPattern[]] :=
	Module[{nb = Null, path},
			
		path = pathIn;
		
		If [!FreeQ[path, $PathnameSeparator] && !FileExistsQ[path],
			Print["OpenNotebook: File not found: ", path];
			Return[$Failed];
		];
			
		If [StringFreeQ[path, $PathnameSeparator],
			(* Perhaps it's a bare file name without a directory. Try and intelligently
			   guess the full path. *)
			With[{issueNotebookPath = ResolveIssueNotebook[path]},
				If [issueNotebookPath =!= $Failed,
					path = issueNotebookPath;
				];
			];
		];
		
		nb = NotebookOpen[path];
		
		If [TrueQ[OptionValue["Evaluate"]],
			SelectionMove[nb, All, Notebook];
			SelectionEvaluate[nb];
		];
		
		If [TrueQ[OptionValue["Return"]],
			nb
		]
	];

PrintAndDisplayError[args___] :=
	With[{str = StringJoin[ToString /@ args]},
		Print[str];
		MessageDialog[str, "WindowTitle" -> "Error"];
	];

Options[NotebookGenerator] =
{
	"DynamicOutputVar" -> Null,	 (*< If button output should be redirected to a dynamic section in the notebook. Note that Null is a special value used by RedirectPrintsAndMessagesToDynamicOutputSection to mean "don't redirect". *)
	"Function" -> Automatic,		(*< Override the default function symbol. *)
	"SubTest" -> None				(*< If this is a sub test notebook, then the name of the sub-test. *)
};

(* Down values can be specified for this function so that if a person creates
   a notebook of a given type, we can specify what that should do. *)
NotebookGenerator[name_, "Standard", OptionsPattern[]] := Automatic;
NotebookGenerator[name_, type_, OptionsPattern[]] := Automatic;

(* "Function" notebooks are used to do development on a function. *)
NotebookGenerator[nameIn_, "Function", OptionsPattern[]] :=
	Module[{package, reloadLine = "", funcSymbol = None, context, argNames, argNamesStr = "",
			optionsButton = Sequence @@ {}, editFunctionButton = Sequence @@ {},
			buttons, buttonsRow = Sequence @@ {}, mathdoc,
			functionUses = {},
			createTestsButton = Sequence @@ {}, runTestsButton,
			runTestsCell = Sequence @@ {},
			newCellButton = Sequence @@ {},
			editTestsButton = Sequence @@ {},
			captureButton = Sequence @@ {},
			moveNotebookButtons,
			funcName,
			notebookName},
		
		notebookName = funcName = nameIn;
		If [OptionValue["Function"] =!= Automatic,
			funcSymbol = OptionValue["Function"];
			With[{sym = funcSymbol}, funcName = SymbolName[sym]; context = Context[sym];];
			,
			If [!CouldBeWLSymbolQ[funcName],
				Speak["Failed"];
				Return[$Failed];
			];
			
			context = GetSymbolContext[funcName];
			
			If [context === None,
				Speak["Couldn't find function"];
				Return[$Failed];
			];
		];

		package = context;
		If [StringMatchQ[context, __ ~~ "`Private`"],
			package = StringDropByDelimiter[context, "`"] <> "`"
		];
		
		
		If [package =!= None,
			
			(* Get the function's MathDoc comment so that we
			   can extract any possible example uses of the function. *)
			With[{packageSource = GetPackageSource[package]},
				If [packageSource === $Failed,
					mathdoc = $Failed;
					,
					mathdoc =
						GetFunctionMathdoc[
							packageSource,
							funcName
						];
				];
			];
			
			If [!MatchQ[mathdoc, $Failed | Missing[]],
				
				functionUses = ExtractUsesOfFunction[mathdoc, funcName];
			   
				(* Add context. *)
				If [!StringFreeQ[Context[funcSymbol], "`Private`" | "`PackagePrivate`"],
					functionUses =
						Function[{functionUse},
							StringReplace[
								functionUse,
								StartOfString ~~ funcName ~~ "[" :> (context <> funcName <> "[")
							]
					   ] /@ functionUses;
				];
				
				(* Take the function uses, which are strings, and turn them into
				   expressions, for the benefit of being able to call Indent2 on
				   them so that they are nicely indented in the function notebook.
				   This is useful, for example, if we needed to add the private context
				   in front of the function symbol, which can result in a single-line
				   function use looking very ugly and wanting to be indented / turned
				   into multiple lines. *)
				functionUses =
					Function[{functionUse},
						If [Quiet[ToExpression[functionUse, StandardForm, HoldComplete]] =!= $Failed,
							RemoveHoldFromIndentedString[
								Indent2[
									ToExpression[functionUse, StandardForm, HoldComplete],
									"RemoveContexts" -> False
								]
							]
							,
							(* Couldn't seem to parse the function use as WL. Just use raw string. *)
							functionUse
						]
					] /@ functionUses;
					
				(* The TODO example usage we add can result in duplicates.
				   Get rid of them. *)
				functionUses = DeleteDuplicates[functionUses];
			];
		];
		
		If [context =!= None,
		
			funcSymbol = Symbol[context <> funcName];
		   
			reloadLine = GetReloadLine[funcSymbol];
			
			If [functionUses === {},
				argNames = GetFunctionArgumentNames[funcSymbol];
				If [argNames =!= {},
					argNamesStr = StringJoin[Riffle[ToString /@ argNames[[1]], ", "]];
					If [!StringFreeQ[ToString[funcSymbol], "`Private`" | "`PackagePrivate`"],
						functionUses = {context <> funcName<>"[" <> argNamesStr <> "]"};
						,
						functionUses = {funcName<>"[" <> argNamesStr <> "]"};
					]
					,
					(* For example, if it's a System` function and we can't introspect DownValues. *)
					functionUses = {funcName<>"[]"};
				];
			];
		];
		
		With[{funcSymbol = funcSymbol, dynamicOutputVar = OptionValue["DynamicOutputVar"],
			  subTest = OptionValue["SubTest"]},
			If [funcSymbol =!= None,
				
				captureButton =
					Row[{
						FunctionCaptureButton[funcSymbol],
						SmartButton[
							"?",
							(* Display help *)
							TODO,
							"Width" -> 30
						]
					}];
				
				editFunctionButton =
					Row[{
						SmartButton[
							"Edit Function",
							EditFunction[funcSymbol]
						],
						SmartButton[
							"?",
							(* Display help *)
							TODO,
							"Width" -> 30
						]
					}];
					
				editTestsButton =
					If [OptionValue["SubTest"] =!= None,
						SmartButton[
							"Edit Tests",
							EditUnitTests[funcSymbol, subTest]
						]
						,
						SmartButton[
							"Edit Tests",
							EditUnitTests[funcSymbol]
						]
					];
				
				If [Options[funcSymbol] =!= {},
					optionsButton =
						SmartButton[
							"Options",
							Print[Indent2[Options[funcSymbol]]];
						];
				];
				
				newCellButton =
					SmartButton[
						"New Test",
						RedirectPrintsAndMessagesToDynamicOutputSection[
							NewFunctionCell[funcSymbol],
							dynamicOutputVar
						]
					];
					
				createTestsButton =
					SmartButton[
						"Add Tests to File",
						RedirectPrintsAndMessagesToDynamicOutputSection[
							CreateUnitTests[funcSymbol, "SubTest" -> subTest],
							dynamicOutputVar
						]
					];
					
				runTestsButton =
					SmartButton[
						"Run Tests",
						RunUnitTestsInNotebook[funcSymbol];
					];
					
				runTestsCell =
					CodeString[
						reloadLine <> "RunUnitTests[" <> ToString[funcSymbol] <> If [OptionValue["SubTest"] =!= None, ", \"SubTest\" -> " <> ToString[OptionValue["SubTest"], InputForm], ""] <> "]"
					];
			];
			
			moveNotebookButtons =
				Row[{
					SmartButton[
						"<",
						MoveNotebook["Left"]
					],
					SmartButton[
						">",
						MoveNotebook["Right"]
					]
				}];
			
			buttons =
				Riffle[
					{
						newCellButton,
						createTestsButton,
						runTestsButton,
						optionsButton,
						editTestsButton,
						editFunctionButton,
						captureButton,
						moveNotebookButtons
					},
					" "
				];
			
			If [buttons =!= {},
				buttonsRow = DockedToolbar[Row[buttons], dynamicOutputVar]
			];
		];
		
		{
			(* We use "Docked" as a way of indicating to CreateIssueNotebook
			   that it should convert this to docked a cell. *)
			buttonsRow
			,
			runTestsCell
			,
			(* This section cell was added so that when we add the test UI above
			   a code cell, it doesn't get clobbered by the output of the "RunUnitTests"
			   code cell above this point. The issue is that the test UI is an
			   "Output" cell, which as the behavior of getting overwritten when
			   a cell above it generates new output. *)
			TextCell["Tests", "Section"]
			,
			Sequence @@
			   Function[{functionUse},
				   CodeString[
reloadLine <>
functionUse
				   ]
				] /@ functionUses
		}
	];

(*!
	\function CouldBeWLSymbolQ
	
	\calltable
		CouldBeWLSymbolQ[str] '' Returns True if the given string could be a WL symbol.
	
	Examples:
	
	CouldBeWLSymbolQ["Test"] === True
	
	CouldBeWLSymbolQ["Test1"] === True
	
	CouldBeWLSymbolQ["1"] === False

	Unit tests:

	RunUnitTests[CouldBeWLSymbolQ]

	\maintainer danielb
*)
CouldBeWLSymbolQ[str_] :=
	(* TODO: Uhhh, what? Let's try to 'fix' this... *)
	(*StringQ[str]*)
	StringMatchQ[str, WLSymbolPattern[]]

(*!
	\function ExtractUsesOfFunction
	
	\calltable
		ExtractUsesOfFunction[source, functionName] '' given some M source and the name of a function, returns all uses of that function.
	
	Example:
	
	ExtractUsesOfFunction[
		GetFunctionMathdoc[
			GetPackageSource["WUtils`WUtils`"],
			"ExtractUsesOfFunction"
		],
		"ExtractUsesOfFunction"
	]
	
	\maintainer danielb
*)
ExtractUsesOfFunction[source_, functionName_] :=
	Module[{functionPositions, openingBracePos, closingBracePosition, res, leftWhitespace},
		
		functionPositions =
			StringPosition[
				source,
				functionName ~~ "["
			];
		
		res = {};
		
		Function[{functionPosition},
			
			openingBracePos = functionPosition[[2]];
			closingBracePosition = FindMatchingBrace[source, openingBracePos];
			
			If [closingBracePosition =!= Missing[],
				leftWhitespace = GetLeftWhitespace[source, functionPosition[[1]]];
				res = {res, StringTake[source, {functionPosition[[1]] - StringLength[leftWhitespace], closingBracePosition}]};
			];
		] /@ functionPositions;
		
		RemoveIndentation /@ Flatten[res]
	]

(*!
	\function FindMatchingBrace
	
	\calltable
		FindMatchingBrace[str, position] '' given a string and a position in that string that gives the position of an opening brace (ex. "["), returns the location of the matching closing brace.
	
	TODO: Should replace this with 'FindMatchingBracket', which is a more robust function that
		  properly deals with strings (which can contain false positive closing brackets), etc.
	
	Example:
	
	FindMatchingBrace["abc[blah];", 4] === 9
	
	FindMatchingBrace["abc[blah", 4] === Missing[]
	
	\related 'ExtractUsesOfFunction
	
	\maintainer danielb
*)
FindMatchingBrace[str_, position_] :=
	Module[{openingBrace, closingBrace, counter, char, res},
		openingBrace = StringTake[str, {position}];
		closingBrace = closingBraceLookup[openingBrace];
		If [!StringQ[closingBrace],
			Print["FindMatchingBrace: Unsupported: " <> ToString[openingBrace, InputForm]];
			Return[$Failed];
		];
		
		res = Missing[];
		counter = 1;
		
		Function[{pos},
			char = StringTake[str, {pos}];
			Which[
				char === openingBrace,
				++counter;
				,
				char === closingBrace,
				--counter;
			];
			If [counter === 0,
				res = pos;
				Return[res, Module];
			];
		] /@ Range[position + 1, StringLength[str]];
		
		res
	]
	
closingBraceLookup["["] = "]"
closingBraceLookup["{"] = "}"
closingBraceLookup["("] = ")"

(*!
	\function GetLeftWhitespace
	
	\calltable
		GetLeftWhitespace[str_, pos_] '' given a string and a position in that string, returns the spaces and tabs immediately to the left.
	
	Example:

	GetLeftWhitespace["xyz\n   abc", 8] === "   "
	
	\maintainer danielb
*)
GetLeftWhitespace[str_, pos_] :=
	StringCases[StringTake[str, pos - 1], RegularExpression["[ \t]*$"]][[1]]

(*!
	\function GetReloadLine
	
	\calltable
		GetReloadLine[funcSymbol] '' given a function symbol, returns the line of code to use to reload its file/package. (line of code as a string)
	
	Examples:
	
	GetReloadLine[WUtils`WUtils`SymbolToFile] === "ReloadParserFiles[];\n"
	
	\related 'ReloadFunction
	
	\maintainer danielb
*)
GetReloadLine[funcSymbol_] :=
	Module[{package, context},
		
		(* Why would I turn a symbol into a string just to turn around and
		   try and figure out what it's context was? Confused. *)
		(*package = GetSymbolPackage[ToString[funcSymbol]];*)
		
		context = Context[funcSymbol];
		
		If [StringMatchQ[context, __ ~~ "`Private`"],
			package = StringDropByDelimiter[context, "`"] <> "`";
			,
			package = context;
		];
		
		With[{reloadFunc = ReloadFunction[funcSymbol]},
			
			If [reloadFunc === Missing[],
				"<< " <> package <> "\n"
				,
				ToString[reloadFunc] <> "[];\n" 
			]
		]
	]

(*!
	\function ReloadFunction
	
	\calltable
		ReloadFunction[funcSymbol] '' given a function, returns a held function call that can be used to reload that file (and perhaps other functions for the relevant product/feature) in an intelligent way. See also: Va["automatic file reloading"]
	
	Examples:
	
	ReloadFunction[SymbolToFile] === HoldComplete[ReloadParserFiles]

	Unit tests:

	RunUnitTests[ReloadFunction]
	
	\maintainer danielb
*)
ReloadFunction[funcSymbol_] :=
	Module[{contextParts, contextValueWasDefinedIn, value, context},
		
		context = Context[funcSymbol];
		If [context === $Failed, Return[$Failed]];
		
		contextParts = StringSplit[context, "`"];
		
		With[{tmp = GetVariablePossiblyFromParentPackage[context, "$ReloadFunction"]},
			If [tmp === Missing[],
				Return[Missing[]]
				,
				{contextValueWasDefinedIn, value} = tmp;
				
				value
			]
		]
	]

(*!
	\function GetFunctionArgumentNames
	
	\calltable
		GetFunctionArgumentNames[functionSymbol] '' given a function symbol, returns all sets of argument names that can be passed to it.
	
	Example:
	
	GetFunctionArgumentNames[CalculateTokenize] === {{"s", "opts"}}
	
	\maintainer danielb
*)
GetFunctionArgumentNames[functionSymbol_] :=
	Module[{downValues, args},
		downValues = DownValues[functionSymbol];
		DeleteDuplicates[
			Function[{downValue},
	
				(* Get the arguments from this down value. *)
				args = downValue[[1]] /. Verbatim[HoldPattern][_[args___]] :> {args};
	
				(* Remove blanks. *)
				args = args /. Verbatim[Pattern][s_Symbol, Blank[]] :> SymbolName[s];
				
				args = args /. Verbatim[Optional][s_, default_] :> s <> " : " <> ToString[default, InputForm];
				
				args = DeleteCases[args, Verbatim[OptionsPattern][], Infinity];
	
				args
	
			] /@ downValues
		]
	]

(*!
	\function FunctionCaptureButton
	
	\calltable
		FunctionCaptureButton[funcSymbol] '' given a function symbol, creates a button that can be used to enable/disable capturing calls to that function for display in the current notebook.
	
	\related 'EnableFunctionCapture
	
	\maintainer danielb
*)
FunctionCaptureButton[funcSymbol_] :=
	DynamicModule[{buttonLabel},
		
		buttonLabel = "Capture";
		
		SmartButton[
			Dynamic[buttonLabel],
			(
			If [buttonLabel === "Capture",
				EnableFunctionCapture[funcSymbol];
				RegisterFunctionCaptureNotebook[funcSymbol, InputNotebook[]];
				buttonLabel = "Release";
				,
				DisableFunctionCapture[funcSymbol];
				buttonLabel = "Capture";
			]
			),
			"Width" -> 100
		]
	]

(*!
	\function EnableFunctionCapture
	
	\calltable
		EnableFunctionCapture[funcSymbol] '' given a function symbol, enable function call capturing.
	
	Examples:
	
	DownValues[myFunc] = {HoldPattern[myFunc[myArg]] :> Print[myArg]};
	
	EnableFunctionCapture[myFunc];

	Unit tests:

	RunUnitTests[EnableFunctionCapture]

	\related 'DisableFunctionCapture
	
	\maintainer danielb
*)
Clear[EnableFunctionCapture];
Options[EnableFunctionCapture] =
{
	"AssociateWithInputNotebook" -> True		(*< associate the InputNotebook[] with the captured function? *)
};
EnableFunctionCapture[funcSymbol_, OptionsPattern[]] :=
	Module[{},
		
		If [!FreeQ[Attributes[funcSymbol], Protected],
			(* For now we won't worry about calling Protect again in the DisableFunctionCapture code. *)
			If [Unprotect[funcSymbol] === $Failed,
				Print["Couldn't Unprotect function '", funcSymbol, "'"];
				Return[$Failed];
			];
		];
		
		DownValues[funcSymbol] =
			Function[{downValue},
				addCaptureToDownValue[downValue]
			] /@ DownValues[funcSymbol];
			
		If [TrueQ[OptionValue["AssociateWithInputNotebook"]],
			RegisterFunctionCaptureNotebook[funcSymbol, InputNotebook[]]
		]
	]

(*!
	\function addCaptureToDownValue
	
	\calltable
		addCaptureToDownValue[downValue] '' given a down value, instrument it so that calls to it can be captured.
	
	Examples:
	
	addCaptureToDownValue[HoldPattern[myFunc[myArg]] :> Print[myArg]]
	
	===
	
	HoldPattern[myFunc[myArg]] :>
		CaptureFunctionCall[
			funcSymbol,
			HoldComplete[{myArg}],
			Print[myArg]
		]

	Unit tests:

	RunUnitTests[WUtils`WUtils`addCaptureToDownValue]

	\related 'EnableFunctionCapture
	
	\maintainer danielb
*)
addCaptureToDownValue[downValueIn_] :=
	Module[{downValue = downValueIn, optBinding},
		
		If [!FreeQ[downValueIn, CaptureFunctionCall],
			(* Already captured. *)
			downValueIn
			,
			
			(* If an OptionsPattern exists, ensure that it has a binding
			   so that we can capture its value. *)
			{optBinding, downValue} = getOptionsPatternBinding[downValue, "AddIfMissing" -> True];
			
			downValue =
			Replace[
				downValue,
				HoldPattern[RuleDelayed][
					HoldPattern[HoldPattern][funcSymbol_[args___]],
					impl_
				] :>
					With[{bindingList = ArgListToBindingList[HoldComplete[{args}]]},
						RuleDelayed[
							HoldPattern[funcSymbol[args]],
							CaptureFunctionCall[
								funcSymbol,
								bindingList,
								impl
							]
						]
					]
				,
				{0}
			]
		];
		
		(* UNDOME / Temporary: This is here until l-kernel helps me understand why
		   I am getting $ added to argument bindings. *)
		ReplaceSymbolsUsingPatterns[
			downValue,
			var__ ~~ "$" :> var
		]
	]

(*!
	\function getOptionsPatternBinding
	
	\calltable
		getOptionsPatternBinding[downValue] '' returns the symbol bound to the OptionsPattern argument held in HoldComplete. If there is no OptionsPattern argument, then None is returned.
	
	Return format: {optBinding, modifiedDownValue}
	
	Examples:
	
	getOptionsPatternBinding[
		HoldPattern[myFunc[myArg, opts:OptionsPattern[]]] :> Print[myArg, ": ", {opts}]
	]
	
	===
	
	HoldComplete[opts]

	Unit tests:

	RunUnitTests[WUtils`WUtils`getOptionsPatternBinding]

	\related 'addCaptureToDownValue
	
	\maintainer danielb
*)
Clear[getOptionsPatternBinding];
Options[getOptionsPatternBinding] =
{
	"AddIfMissing" -> False	 (*< add a binding to the OptionsPattern if one doesn't already exist. If this option is used, then the return value is of the form {optBinding, modifiedDownValue}. *)
};
getOptionsPatternBinding[downValue_, OptionsPattern[]] :=
	Module[{optBinding = None},
		
		(* Find the binding if it's present and, if so, set optBinding. *)
		Replace[
			downValue,
			HoldPattern[RuleDelayed][
				HoldPattern[HoldPattern][funcSymbol_[___, HoldPattern[Pattern][binding_, HoldPattern[OptionsPattern][]], ___]],
				impl_
			] :>
				(
				optBinding = HoldComplete[binding]
				)
			,
			{0}
		];
		
		If [optBinding === None && TrueQ[OptionValue["AddIfMissing"]],
			(* There wasn't a binding on the OptionsPattern, so
			   we'll add one. *)
			Return[addOptionsPatternBinding[downValue], Module]
		];
		
		{
			optBinding,
			downValue
		}
	]

(*!
	\function addOptionsPatternBinding
	
	\calltable
		addOptionsPatternBinding[downValue] '' given a down value that is assumed to have an OptionsPattern, but without a pattern binding on it, this function adds a pattern binding.
	
	Return format:
	
	{optBinding, modifiedDownValue}
	
	Examples:
	
	addOptionsPatternBinding[
		HoldPattern[myFunc[myArg, OptionsPattern[]]] :> Print[myArg]
	]
	
	===
	
	{
		HoldComplete[bindingAddedDynamicallyByGetOptionsPatternBinding1330],
		HoldPattern[
			myFunc[myArg, bindingAddedDynamicallyByGetOptionsPatternBinding1330$:OptionsPattern[]]
		] :>
			Print[myArg]
	}
	
	\related 'getOptionsPatternBinding
	
	\maintainer danielb
*)
addOptionsPatternBinding[downValue_] :=
	Module[{modifiedDownValue, optBinding},
		With[{newBinding = Unique["bindingAddedDynamicallyByGetOptionsPatternBinding"]},
			modifiedDownValue =
			Replace[
				downValue,
				HoldPattern[RuleDelayed][
					HoldPattern[HoldPattern][funcSymbol_[a___, HoldPattern[OptionsPattern][], b___]],
					impl_
				] :>
					(
					optBinding = HoldComplete[newBinding];
					RuleDelayed[
						HoldPattern[funcSymbol[a, PatternX[newBinding, OptionsPattern[]], b]],
						impl
					]
					) /. PatternX :> Pattern
				,
				{0}
			];
		];
		
		{
			optBinding,
			modifiedDownValue
		}
	]

(*!
	\function ArgListToBindingList
	
	\calltable
		ArgListToBindingList[heldArgList] '' given held arguments of the form HoldComplete[{args___}], return back the same form but with each argument replaced with its pattern binding.
	
	Examples:
	
	ArgListToBindingList[
		HoldComplete[
			{
			myArg1_Integer,
			myArg2_String,
			myArg3:OptionsPattern[]
			}
		]
	]
	
	===
	
	HoldComplete[
		{
		myArg1,
		myArg2,
		myArg3
		}
	]

	Unit tests:

	RunUnitTests[ArgListToBindingList]

	\related 'addCaptureToDownValue
	
	\maintainer danielb
*)
ArgListToBindingList[heldArgList_] :=
	Module[{},
		Replace[
			HeldListToListOfHeld[heldArgList],
			{
			HoldComplete[HoldPattern[Pattern][binding_, _]] :> HoldComplete[binding]
			}
			,
			{1}
		] // ListOfHeldToHeldList
	]

(*!
	\function CaptureFunctionCall
	
	\calltable
		CaptureFunctionCall[funcSymbol, args, impl] '' comment
	
	Examples:
	
	CaptureFunctionCall[funcSymbol, args, impl] === TODO
	
	\related '
	
	\maintainer danielb
*)
Clear[CaptureFunctionCall];
CaptureFunctionCall[funcSymbol_, args_, impl_] :=
	Module[{},
		
		(*Print["CaptureFunctionCall: ", funcSymbol, ", ", args];*)
		
		(* Protect against recursive capturing if we're debugging Indent2,
		   sincd the CreateCapturedFunctionCell calls Indent2. *)
		If [!TrueQ[$creatingCapturedFunctionCell],
			Block[{$creatingCapturedFunctionCell = True},
				CreateCapturedFunctionCell[funcSymbol, args];
			]
		];
		
		impl
	]

(*!
	\function CreateCapturedFunctionCell
	
	\calltable
		CreateCapturedFunctionCell[funcSymbol, args] '' given a function symbol and its arguments, print a code cell into the appropriate notebook showing that function call.
	
	Examples:
	
	CreateCapturedFunctionCell[
		mySymbol,
		HoldComplete[{1, 2, 3}]
	]
	
	\related 'CaptureFunctionCall
	
	\maintainer danielb
*)
CreateCapturedFunctionCell[funcSymbol_, HoldComplete[{args___}]] :=
	Module[{nb, functionCall, reloadLine},
		
		nb = GetFunctionCaptureNotebook[funcSymbol];
		
		(* Move to the end of the notebook. *)
		SelectionMove[Cells[nb][[-1]], Next, Cell];
		
		functionCall = HoldComplete[funcSymbol[args]];
		
		reloadLine = GetReloadLine[funcSymbol];
		
		NotebookWrite[
			nb,
			Cell[
				RawBoxes[
					reloadLine <>
					RemoveHoldFromIndentedString[
						Indent2[
							functionCall,
							"RemoveContexts" -> False
						]
					]
				],
				"Code",
				InitializationCell -> False
			]
		];
	]

(*!
	\function GetFunctionCaptureNotebook
	
	\calltable
		GetFunctionCaptureNotebook[funcSymbol] '' given a function symbol, which notebook should captured function calls be reported in? If no notebook has been registered, then the InputNotebook[] is used.
	
	Examples:
	
	GetFunctionCaptureNotebook[SymbolToFile]
	
	\related 'RegisterFunctionCaptureNotebook
	
	\maintainer danielb
*)
GetFunctionCaptureNotebook[funcSymbol_] :=
	With[{nb = registeredFunctionCaptureNotebook[funcSymbol]},
		If [nb === Null,
			InputNotebook[]
			,
			nb
		]
	]

registeredFunctionCaptureNotebook[_] := Null;

(*!
	\function ReplaceSymbolsUsingPatterns
	
	\calltable
		ReplaceSymbolsUsingPatterns[e, symbolMapping] '' Given an expression and a list of replacements such as {RegularExpression[\"mySymbol.+\"] -> \"mySymbol\"}, makes the appropriate replacements. The LHS of each replacement rule can be a string expression.
	
	This can be useful when testing the output of a function that involves dynamically
	generated symbol. We can replace those dynamically generated symbols (which involve
	unpredictable numeric portions) into predictable symbol names that don't have numeric
	parts.
	
	Example:
	
	ReplaceSymbolsUsingPatterns[
		HoldComplete[{bindingAddedDynamicallyByGetOptionsPatternBinding1410$}],
		{
		RegularExpression["bindingAddedDynamicallyByGetOptionsPatternBinding.+"] -> "bindingAddedDynamicallyByGetOptionsPatternBinding"
		}
	]
	
	===
	
	HoldComplete[{bindingAddedDynamicallyByGetOptionsPatternBinding}]

	Unit tests:

	RunUnitTests[ReplaceSymbolsUsingPatterns]

	\maintainer danielb
*)
ReplaceSymbolsUsingPatterns[e_, symbolMappingIn_] :=
	Module[{symbolMapping, notApplicableString = ToString[Unique["notApplicableSentinel"]]},
		   
		(* So that passing in a single replacement rule works. *)
		symbolMapping = Flatten[{symbolMappingIn}];
		   
		symbolMapping =
			Append[
				(StartOfString ~~ #[[1]] ~~ EndOfString -> #[[2]]) & /@ symbolMapping,
				StartOfString ~~ ___ ~~ EndOfString -> notApplicableString
			];
		
		Replace[
			e,
			s_Symbol
			:>
			With[{
				 code =
					(
					Symbol[StringReplace[HeldExpressionToString[HoldComplete[s]], symbolMapping]]
					)
				 },
				code /; True
			]
			/;
				(
				StringReplace[HeldExpressionToString[HoldComplete[s]], symbolMapping] =!= notApplicableString
				)
			,
			Infinity,
			Heads -> True
		]
	]

(*!
	\function HeldExpressionToString
	
	\calltable
		HeldExpressionToString[e] '' given a held expression, convert it to a string and remove the hold.
	
	If you have a held expression that you don't want to evaluate, and you want
	to get the string representation of that expression (without the hold),
	this function does the job.
	
	If there's a more elegant way to do this kind of thing, I'm not familiar with it.
	
	Example:
	HeldExpressionToString[Hold[$myVar]] === "$myVar"
	
	\maintainer danielb
*)
HeldExpressionToString[e_] :=
	StringReplace[
		ToString[e, InputForm],
		("HoldComplete" | "Hold") ~~ "[" ~~ innerExpression___ ~~ "]" :> innerExpression
	]

(*!
	\function RegisterFunctionCaptureNotebook
	
	\calltable
		RegisterFunctionCaptureNotebook[funcSymbol, notebook] '' given a function symbol and notebooks, associates the function with the function with the notebook so that when function calls are captured (via the EnableFunctionCapture mechanism), code cells will be added to the notebook.
	
	Examples:
	
	RegisterFunctionCaptureNotebook[
		SymbolToFile,
		InputNotebook[]
	]
	
	\related 'EnableFunctionCapture 'CaptureFunctionCall
	
	\maintainer danielb
*)
RegisterFunctionCaptureNotebook[funcSymbol_, notebook_:If[Null =!= $FrontEnd, InputNotebook[], Null]] :=
	Module[{},
		registeredFunctionCaptureNotebook[funcSymbol] = notebook;
	]

(*!
	\function DisableFunctionCapture
	
	\calltable
		DisableFunctionCapture[funcSymbol] '' given a function symbol, disable function call capturing.
	
	Examples:
	
	DownValues[myFunc] =
	{
		HoldPattern[myFunc[myArg_]] :>
			CaptureFunctionCall[myFunc, HoldComplete[{myArg}], myArg + 1]
	}
	
	DisableFunctionCapture[myFunc];
	
	\related 'EnableFunctionCapture
	
	\maintainer danielb
*)
DisableFunctionCapture[funcSymbol_] :=
	Module[{},
		DownValues[funcSymbol] =
			Function[{downValue},
				removeCaptureFromDownValue[downValue]
			] /@ DownValues[funcSymbol];
	]

(*!
	\function removeCaptureFromDownValue
	
	\calltable
		removeCaptureFromDownValue[downValue] '' comment
	
	Examples:
	
	removeCaptureFromDownValue[
		HoldPattern[myFunc[myArg_]] :>
			CaptureFunctionCall[myFunc, HoldComplete[{myArg}], myArg + 1]
	]
	
	===
	
	HoldPattern[myFunc[myArg_]] :> myArg + 1

	Unit tests:

	RunUnitTests[WUtils`WUtils`removeCaptureFromDownValue]

	\related 'DisableFunctionCapture 'addCaptureToDownValue
	
	\maintainer danielb
*)
removeCaptureFromDownValue[downValue_] :=
	Module[{},
		Replace[
			downValue,
			RuleDelayed[
				HoldPattern[HoldPattern][funcSymbol_[args___]],
				CaptureFunctionCall[
					_,
					_,
					impl_
				]
			] :>
				RuleDelayed[
					HoldPattern[funcSymbol[args]],
					impl
				]
			,
			{0}
		]
	]

(*!
	\function EditFunction
	
	\calltable
		EditFunction[funcSymbol] '' given a function symbol, edit it in Workbench.
	
	Examples:
	
	EditFunction[EditFunction]
	
	\related 'OpenFileInWorkbench
	
	\maintainer danielb
*)
Clear[EditFunction];
EditFunction[funcSymbol_, fileIn_:Automatic] :=
	Module[{file, lineNum},
		
		file = fileIn;
		
		If [file === Automatic,
			file = SymbolToFile[funcSymbol];
			If [file === $Failed, Return[$Failed]];
		];
		
		lineNum = GetLineNumber[funcSymbol, file];
		If [lineNum === None,
			Print["Couldn't find the definition of ", funcSymbol, " in ", file, "."];
			$Failed
			,
			If [NumberQ[lineNum],
				OpenFileInWorkbench[file, "Line" -> lineNum]
				,
				$Failed
			]
		]
	]

(*!
	\function GetLineNumber
	
	\calltable
		GetLineNumber[funcSymbol, file] '' given a function symbol and the file that defines it, returns the line number where the definition occurs, or $Failed if it couldn't be determined.
	
	Examples:
	
	GetLineNumber[GetLineNumber, SymbolToFile[GetLineNumber]]
	
	\maintainer danielb
*)
Clear[GetLineNumber];
GetLineNumber[funcSymbol_, file_String] :=
	Module[{},
		If [!FileExistsQ[file], Print["Missing file: " <> file]; Return[$Failed]];
		GetLineNumberOfStringInFile[
			StartOfLine ~~ If [StringQ[funcSymbol], funcSymbol, SymbolName[funcSymbol]] <> "[",
			file
		]
	]

(*!
	\function EditUnitTests
	
	\calltable
		EditUnitTests[funcSymbol] '' opens the .mt file in Mathematica.
	
	Examples:
	
	EditUnitTests[CouldBeWLSymbolQ]
	
	\related 'CreateUnitTests 'RunUnitTests
	
	\maintainer danielb
*)
EditUnitTests[funcSymbol_Symbol, subTest_String:None] :=
	Module[{file},
		
		file = UnitTestFilename[funcSymbol, "SubTest" -> subTest];
		
		If [file === $Failed, Return[$Failed]];
		
		OpenFileInWorkbench[file]
	]

(*!
	\function NewFunctionCell
	
	\calltable
		NewFunctionCell[funcSymbol] '' given a function symbol, adds a new notebook code cell for running that function.
	
	Examples:
	
	NewFunctionCell[CouldBeWLSymbolQ]
	
	\related 'CreateIssueNotebook
	
	\maintainer danielb
*)
NewFunctionCell[funcSymbol_] :=
	Module[{reloadLine},
		
		reloadLine = GetReloadLine[funcSymbol];
		
		(* Move to the end of the notebook. *)
		SelectionMove[Cells[InputNotebook[]][[-1]], Next, Cell];
		
		NotebookWrite[
			InputNotebook[],
			Cell[
				RawBoxes[
					reloadLine <>
					ToString[funcSymbol] <> "[ARGS]"
				],
				"Code",
				InitializationCell -> False
			]
		];
		
		(* Is there a way to put the selection at the exact point between "[" and "]"? *)
		SelectionMove[Cells[InputNotebook[]][[-1]], Cell, Next, 0];
		
		NotebookFind[InputNotebook[], "ARGS", AutoScroll -> False];
	]

(*!
	\function MoveNotebook
	
	\calltable
		MoveNotebook[direction] '' moves the current notebook left/right. Useful if the notebook's width is half the width of the screen and you want to move it to the other side of the screen so that you have two notebooks side by side. Dragging a window to the precise spot on the other side of the screen takes a lot of effort. If you have two monitors, then moving to the right twice will end up putting the notebook on the next monitor, etc.
	
	Va["move right"]
	
	Example:
	
	MoveNotebook["Right"]
	
	\maintainer danielb
*)
MoveNotebook[direction_] :=
	With[{newX = moveNotebookHelper[direction, NotebookX[], getWindowWidth[], GetScreenDimensions[][[1]], Global`$NumberOfDisplays]},
		If [newX =!= $Failed,
			SetOptions[
				InputNotebook[],
				WindowMargins ->
					{
						newX,
						Gett[Options[InputNotebook[]], WindowMargins][[2]]
					}
			]
		]
	]

(*!
	\function NotebookX
	
	\calltable
		NotebookX[] '' returns the X coordinate of the current notebook.
	
	Example:
	
	NotebookX[] === 0
	
	\maintainer danielb
*)
NotebookX[] :=
	Module[{windowMargins, windowMarginsX, screenWidth,
			notebookWidth, notebookHeight, res},
		
		screenWidth = GetScreenDimensions[][[1]];
		
		{notebookWidth, notebookHeight} = notebookWidthHeight[];
		
		windowMargins = Gett[Options[InputNotebook[]], WindowMargins];
		
		windowMarginsX = windowMargins[[1]];
		
		Which[
			(* Relative to left side of screen. *)
			MatchQ[windowMarginsX, {n_, Automatic} /; NumberQ[n]],
			res = windowMarginsX[[1]]
			,
			(* Relative to right side of screen. *)
			MatchQ[windowMarginsX, {Automatic, n_} /; NumberQ[n]],
			res = screenWidth - windowMarginsX[[2]] - notebookWidth
			,
			True,
			Print["NotebookX: Unable to determine notebook's X coordinate. WindowMargins: ", InputForm[windowMargins]];
			res = $Failed;
		];
		
		res
	]

(*!
	\function GetScreenDimensions
	
	\calltable
		GetScreenDimensions[] '' returns the width and height of the screen.
	
	\maintainer danielb
*)
GetScreenDimensions[] := GetScreenDimensions[1]
GetScreenDimensions[screenNum_] :=
	{#[[1,1]], #[[2,1]]} & @ Map[Differences, "ScreenArea" /. SystemInformation["Devices", "ScreenInformation"][[screenNum]]]
	
(* Returns {notebookWidth, notebookHeight} *)
notebookWidthHeight[] = Gett[Options[InputNotebook[]], WindowSize]

(*!
	\function getWindowWidth
	
	\calltable
		getWindowWidth[] '' returns the width of the notebook's window.
	
	\maintainer danielb
*)
getWindowWidth[] := CurrentValue[InputNotebook[], WindowSize][[1]]

(*!
	\function moveNotebookHelper
	
	\calltable
		moveNotebookHelper[dir, windowXPos, windowWidth, screenWidth, numDisplays] '' given a direction to move the window, and the current state of the window, plus information about the width of the display, etc, returns the new window margins for the window.

	Examples:
	
	moveNotebookHelper["Right", 0, 500, 1000, 1] === {Automatic, 0.}

	Unit tests:

	RunUnitTests[WUtils`WUtils`moveNotebookHelper]

	\maintainer danielb
*)
moveNotebookHelper[dir_, windowXPos_, windowWidth_, screenWidth_, numDisplays_] :=
	Module[{xPos, windowIsFullScreen, moveDistance},
		
		xPos = notebookXInUnitsOfScreenWidthRoundedToHalfs[windowXPos, windowWidth, screenWidth];
		
		windowIsFullScreen = windowWidth / screenWidth >= 0.98;
		
		If [TrueQ[windowIsFullScreen],
			moveDistance = 1;
			,
			moveDistance = 0.5;
		];
		
		xPos =
			Switch[
				dir,
				
				"Left",
				xPos - moveDistance
				,
				"Right",
				xPos + moveDistance
				,
				_,
				Print["MoveNotebook: Invalid direction: ", dir];
				Return[$Failed, Module];
			];
		
		If [xPos < 0,
			(* Don't move the notebook off-screen left. *)
			xPos = 0;
		];
		
		If [xPos >= 1,
			(* Don't move the notebook off-screen right. *)
			If [EnsureDefined[Global`$NumberOfDisplays, "It specifies the number of computer displays your system has, and is required so that MoveNotebook can avoid moving a notebook off-screen to the right."] === $Failed, Return[$Failed, Module]];
			 
			If [xPos >= numDisplays,
				(* Don't move the notebook off-screen right. *)
				(* But do move it to the same xPos it was, which
				   will ensure it's lined up with the screen's edge
				   if it isn't already. *)
				xPos -= moveDistance;
			];
		];
			
		toWindowMargins[xPos, screenWidth]
	]

notebookXInUnitsOfScreenWidthRoundedToHalfs[windowX_, windowWidth_, screenWidth_] :=
	N[Round[notebookXInUnitsOfScreenWidth[windowX, windowWidth, screenWidth] * 2] / 2]

(*!
	\function notebookXInUnitsOfScreenWidth
	
	\calltable
		notebookXInUnitsOfScreenWidth[windowX, windowWidth, screenWidth] '' returns the X coordinate of the notebook in units of screen widths. In the case where the notebook isn't half the width of the screen, the units are such that 0 means left-border-of-screen, 0.5 means that the window's right border is on the right-border-of-screen, and 1 means that the window's left border is on the left-border-of-screen-2.

	Example:

	notebookXInUnitsOfScreenWidth[0, 500, 1000] === 0.

	Unit tests:

	RunUnitTests[WUtils`WUtils`notebookXInUnitsOfScreenWidth]

	\related 'MoveNotebook 'notebookOnLeftQ 'notebookXInUnitsOfScreenWidthRoundedToHalfs
	
	\maintainer danielb
*)
notebookXInUnitsOfScreenWidth[windowX_, windowWidth_, screenWidth_] :=
	Module[{screenWidthLessWindowWidth,
			screenNum, windowXRelativeToScreen,
			windowXAsRatioOfGap, debugFlag = False},
		
		screenWidthLessWindowWidth = screenWidth - windowWidth;
		
		If [screenWidthLessWindowWidth === 0,
			(* Special case. Window width is exactly the screen width. *)
			N[windowX / screenWidth]
			,
			(* Which screen the window's left border is on. 0-based index. *)
			screenNum = Floor[windowX / screenWidth];
			
			(* The x-coordinate of the window on the screen it's on. *)
			windowXRelativeToScreen = windowX - screenNum * screenWidth;
			
			(* The X coordinate of the window relative to the gap
			   around the window on the screen. *)
			windowXAsRatioOfGap = windowXRelativeToScreen / screenWidthLessWindowWidth;
			
			If [debugFlag, Print["windowXRelativeToScreen: ", windowXRelativeToScreen]];
			If [debugFlag, Print["screenWidthLessWindowWidth: ", screenWidthLessWindowWidth]];
			If [debugFlag, Print["windowXAsRatioOfGap: ", N[windowXAsRatioOfGap]]];
			
			Which[
				(windowXRelativeToScreen / screenWidth) >= 0.95,
				(* Window is just left of the left border of screen. *)
				If [debugFlag, Print["Case A"]];
				screenNum + 1
				,
				windowXAsRatioOfGap >= 0 && windowXAsRatioOfGap <= 1.1,
				(* Window is within (or almost within) the bounds of the
				   current screen. *)
				If [debugFlag, Print["Case B"]];
				screenNum + windowXAsRatioOfGap * 0.5
				,
				True,
				(* Otherwise, the window's right edge spans over into
				   the screen to the right. *)
				If [debugFlag, Print["Case C"]];
				screenNum +
				0.5 +
				((windowXRelativeToScreen - screenWidthLessWindowWidth) / windowWidth) * 0.5
			]
		]
	]

(*!
	\function EnsureDefined
	
	\calltable
		EnsureDefined[var, desc] '' ensures the given variable is defined. If not, an error is printed and $Failed is returned

	Examples:
	
	EnsureDefined[myUndefinedVar, "Description of variable goes here."] === Null

	Unit tests:

	RunUnitTests[WUtils`WUtils`EnsureDefined]

	\maintainer danielb
*)
Attributes[EnsureDefined] = {HoldAllComplete};
EnsureDefined[var_, desc_] :=
	Module[{},
		If [!ValueQ[var],
			Print[
				"The variable ",
				(* Make sure that things like "Global`" aren't on the context path
				   so that it's clear what context the variable is in. *)
				Block[{$ContextPath = {"System`"}, $Context = "Private`"},
					RemoveHoldFromIndentedString[ToString[HoldComplete[var]]]
				],
				" must be defined. ",
				desc
			];
			Return[$Failed]
		];
	]

(*!
	\function toWindowMargins
	
	\calltable
		toWindowMargins[xPos, screenWidth] '' given a desired notebook 'xPos', which is the X coordinate in units of screen width rounded to the nearest half, convert it to a usable WindowMargins X value.
	
	Example:
	
	toWindowMargins[0.5, 1920] === {Automatic, 0.}
	
	toWindowMargins[1, 1920] === {1920, Automatic}

	Unit tests:

	RunUnitTests[WUtils`WUtils`toWindowMargins]

	\maintainer danielb
*)
Clear[toWindowMargins];
toWindowMargins[xPos_, screenWidth_] :=
	Module[{notebookOnLeftQ},
		notebookOnLeftQ = Abs[xPos - Round[xPos]] <= 0.000001;
		
		If [notebookOnLeftQ,
			{xPos * screenWidth - 7, Automatic}
			,
			{Automatic, (1 - (xPos + 0.5)) * screenWidth - 7}
		]
	]

(*!
	\function DockedToolbar
	
	\calltable
		DockedToolbar[content, dynamicOutputVar] '' returns content that can be placed in the notebook's toolbar, wrapped with an appropriate dynamic output section
	
	Examples:
	
	DockedToolbar[
		rowOfButtons,
		HoldComplete[myDynamicOutputVar]
	]
	
	\related 'CreateIssueNotebook
	
	\maintainer danielb
*)
DockedToolbar[content_, dynamicOutputVar_] :=
	Module[{},
	   "Docked"[
		   DynamicOutputSection[
			   content,
			   dynamicOutputVar,
			   (* We need to specify a maximum height, otherwise, if there is too much
				  output, the toolbar will become higher than the entire notebook, and
				  there will be no way to dismiss the dynamic output. And at that point,
				  the notebook is unusable. *)
			   "MaxHeight" -> 300
		   ]
	   ]
	]

(*!
	\function defaultNotebookButtons
	
	\calltable
		defaultNotebookButtons[dynamicOutputVar] '' returns the default toolbar buttons for a notebook, containing buttons such as "Open Dir".
	
	\related 'CreateIssueNotebook
	
	\maintainer danielb
*)
Clear[defaultNotebookButtons];
defaultNotebookButtons[dynamicOutputVar_] :=
	Module[{openDirButton, moveNotebookButtons, buttons,
			buttonsRow = Sequence @@ {},
			codeCellButton, bulletButton},
							
		openDirButton =
			SmartButton[
				"Open Dir",
				SystemOpen[ContextDirectory[]]
			];
			
		codeCellButton =
			SmartButton[
				"Code Cell",
				InsertCodeCell[]
			];
			
		bulletButton =
			SmartButton[
				"Bullet",
				CreateNotebookItem[]
			];
			
		moveNotebookButtons =
			Row[{
				SmartButton[
					"<",
					MoveNotebook["Left"]
				],
				SmartButton[
					">",
					MoveNotebook["Right"]
				]
			}];
		
		buttons =
			Grid[
				{
					{
						Row[
							Riffle[
								{
									openDirButton,
									codeCellButton,
									bulletButton
								},
								" "
							]
						],
						Row[
							{
							moveNotebookButtons
							},
							ImageSize -> {Full, Automatic},
							Alignment -> {Right, Center}
						]
					}
				}
			];
			
		If [buttons =!= {},
			buttonsRow = DockedToolbar[buttons, dynamicOutputVar]
		];
				
		buttonsRow
	]

(*!
	\function ContextDirectory
	
	\calltable
		ContextDirectory[] '' the directory most likely wrt context.
	
	\maintainer danielb
*)
ContextDirectory[] :=
	Module[{dir},
		dir = Quiet[Check[NotebookDirectory[InputNotebook[]], $Failed]]
	]

(*!
	\function InsertCodeCell
	
	\calltable
		InsertCodeCell[] '' inserts a code cell into the notebook.
	
	\maintainer danielb
*)
InsertCodeCell[] :=
	Module[{},
		NotebookWrite[
			InputNotebook[],
			Cell[
				RawBoxes[""],
				"Code",
				InitializationCell -> False
			]
		];
	]

(*!
	\function CreateNotebookItem
	
	\calltable
		CreateNotebookItem[] '' replaces the current cell in the notebook with an Item cell. (bulleted list)
	
	Since right clicking on the cell and selecting "Style" -> "Item" is inefficient.
	
	\maintainer danielb
*)
CreateNotebookItem[] :=
	Module[{},
		NotebookWrite[InputNotebook[], Cell["replaceme", "Item"]];
		NotebookFind[InputNotebook[], "replaceme", AutoScroll -> False];
	]

(*!
	\function ExtractedDockedContents
	
	\calltable
		ExtractedDockedContents[notebookContents] '' given a list that represents notebook contents, extract/remove the one of form "Docked"[_] if preesnt.
	
	Examples:
	
	ExtractedDockedContents[
		{
			"Docked"["dockedContents"],
			"notebookContents1",
			"notebookContents2"
		}
	]
	
	===
	
	{
		{
			"notebookContents1",
			"notebookContents2"
		},
		"dockedContents"
	}

	Unit tests:

	RunUnitTests[WUtils`WUtils`ExtractedDockedContents]

	\related 'CreateIssueNotebook
	
	\maintainer danielb
*)
ExtractedDockedContents[notebookContentsIn_] :=
	Module[{dockedContents, notebookContents = notebookContentsIn},
		
		dockedContents = None;
		
		Cases[
			notebookContents,
			"Docked"[d_] :>
				(
				dockedContents = d;
				notebookContents = DeleteCases[notebookContents, "Docked"[_], {1}];
				),
			{1}
		];
		
		{
			notebookContents,
			dockedContents
		}
	]

Options[CreateNotebook2] =
{
	"Title" -> Automatic,				(*< The title of the notebook. If not specified, the notebook name will be used. *)
	"Type" -> Automatic,				(*< The type of notebook to create." *)
	"NotebookSubtype" -> Automatic,		(*< A sub-type to use when constructing the filename. For example, if the notebook title is "DSL-311" and the sub-type is "BETA", then the notebook filename will be "DSL-311 BETA.nb". *)
	"Directory" -> Automatic,			(*< The directory in which to create the notebook. *)
	"Contents" -> Automatic,			(*< The contents can be specified manually. *)
	"DockedContents" -> None,			(*< The docked/toolbar contents. *)
	"TemplateNotebook" -> None,			(*< The template notebook to make a copy of. *)
	"Evaluator" -> Automatic,			(*< Specifies the kernel that the notebook should use. *)
	"CreateFile" -> True,				(*< Should the notebook be saved? Or just created and opened? This option is not compatible with non-default values of MathematicaVersion, and it doesn't open the notebook if it already exists. It's useful for testing this function. *)
	"CreateSubDirectory" -> False,		(*< Should we create a sub-directory of the same name as the notebook and place the notebook inside? This can be a good idea for 'issue' notebooks because it allows multiple notebooks per issue, etc. *)
	"PreTitleContents" -> {}			(*< Notebook contents that should be put above the title? *)
};
CreateNotebook2[name_String, opts:OptionsPattern[]] :=
	Module[{dir, path, title, template = OptionValue["TemplateNotebook"],
			contents = {Code[{}]}, notebook, yOffset},
		
		If [TrueQ[OptionValue["CreateFile"]],
			If [OptionValue["Directory"] =!= Automatic,
				dir = OptionValue["Directory"];
				,
				If [!IssuesDirectoryDefined[], Return[$Failed]];
				dir = Global`$NotebookDirectory;
			];
			
			path = ToFileName[{dir}, name <> ".nb"];
			
			If [FileExistsQ[path],
				(* The notebook already exists in the main issues directory. Open it instead. *)
				Return@OpenNotebook[path];
			];
			
			If [OptionValue["NotebookSubtype"] =!= Automatic,
				path = ToFileName[{dir}, name <> " " <> OptionValue["NotebookSubtype"] <> ".nb"];
				
				If [FileExistsQ[path],
					(* The notebook already exists in the main issues directory. Open it instead. *)
					Return@OpenNotebook[path];
				];
			];
	
			(* By default, create a directory of the same name as the notebook and
			   place the notebook in that directory. *)
			If [TrueQ[OptionValue["CreateSubDirectory"]],
				dir = ToFileName[{dir, name}];
				If [!FileExistsQ[dir],
					CreateDirectory[dir];
					If [!FileExistsQ[dir],
						Print["CreateNotebook: Couldn't create directory: ", dir];
						Return[$Failed];
					];
				];
			];
			
			path = ToFileName[{dir}, name <> ".nb"];
			
			If [FileExistsQ[path],
				(* The notebook already exists. Open it instead. *)
				Return@OpenNotebook[path];
			];
			
			(* If a sub-type was given, name the notebook accordingly. This is
			   useful so that it's possible to have multiple notebooks for
			   any given issue, distinguished by sub-type. *)
			If [OptionValue["NotebookSubtype"] =!= Automatic,
				path = ToFileName[{dir}, name <> " " <> OptionValue["NotebookSubtype"] <> ".nb"];
				
				If [FileExistsQ[path],
					(* The notebook already exists. Open it instead. *)
					Return@OpenNotebook[path];
				];
			];
		];
		
		If [OptionValue["Contents"] =!= Automatic,
			contents = OptionValue["Contents"];
		];
		
		If [OptionValue["Title"] =!= Automatic,
			title = OptionValue["Title"];
			,
			title = name;
			
			If [OptionValue["Type"] =!= "Function",
				(* When / how often will we want to de-camel-case the notebook title? *)
				title = DeCamelCase[name];
			];
		];
		
		If [OptionValue["Title"] =!= Automatic,
			title = OptionValue["Title"];
		];
		
		With[{contents = contents},
			If [template =!= None,
				
				If [!StringQ[template],
					Print["CreateNotebook: Invalid template notebook filename: ", template];
					Return[$Failed];
				];				
				
				If [!FileExistsQ[template],
					Print["CreateNotebook: Template notebook doesn't exist: ", template];
					Return[$Failed];
				];
				
				CopyFile[template, path];
				Return@OpenNotebook[path];
				,
				If [version === "10" && Floor[$VersionNumber] === 9,
					(* So that M10 notebooks line up vertically with M9 notebooks on the screen. *)
					yOffset = 58;
					,
					yOffset = 0;
				];
				
				notebook =
				CreateWorkingNotebook[
					contents,
					title,
					If [TrueQ[OptionValue["CreateFile"]],
						"File" -> path
						,
						Sequence @@ {}
					],
					"LeftHalfOfScreen" -> True,
					"EvaluateImmediately" -> False,
					"Evaluator" -> If [OptionValue["Evaluator"] === Automatic, CurrentValue[InputNotebook[], Evaluator], "Local"],
					"YOffset" -> yOffset,
					"PreTitleContents" -> OptionValue["PreTitleContents"]
				];
				
				If [OptionValue["DockedContents"] =!= None,
					CurrentValue[
						notebook,
						DockedCells
					] =
					{
						Cell[
							BoxData[
								ToBoxes[
									OptionValue["DockedContents"]
								]
							],
							"DockedCell"
						]
					};
				];
			]
		];
		
		notebook
	]

(*!
	\function DeCamelCase
	
	\calltable
		DeCamelCase[string] '' given a string, if it is camel cased, then it will be de-camel-cased, words separated with spaces.

	Examples:
	
	DeCamelCase["JustTesting"] === "Just Testing"

	Unit tests:

	RunUnitTests[WUtils`WUtils`DeCamelCase]

	\maintainer danielb
*)
DeCamelCase[stringIn_] :=
	Module[{string = stringIn},
		If [!CamelCaseQ[string],
			string
			,
			StringJoin[
				Riffle[
					StringCases[
						stringIn,
						{
							RegularExpression["[A-Z][a-z]+"],
							RegularExpression["[A-Z]+(?![a-z])"],
							lower:RegularExpression["(?<![A-Z])[a-z][a-z]+"] :>
								ToUpperCase[StringTake[lower, 1]] <>
								StringTake[lower, {2, -1}],
							RegularExpression["[0-9]+"]
						}
					],
					" "
				]
			]
		]
	];

(*!
	\function CamelCaseQ
	
	\calltable
		CamelCaseQ[string] '' returns True if the given string is camel cased.

	Examples:
	
	CamelCaseQ["JustTesting"] === True

	Unit tests:

	RunUnitTests[WUtils`WUtils`CamelCaseQ]

	\maintainer danielb
*)
CamelCaseQ[string_] :=
	Module[{caseIndications, caseIndicationsGrouped},
		
		If [!StringMatchQ[string, LetterCharacter ~~ (LetterCharacter | DigitCharacter)..],
			Return[False];
		];
		
		caseIndications = UpperCaseQ /@ Characters[string];
		
		caseIndicationsGrouped = Split[caseIndications];
		
		MatchQ[
			DeleteDuplicates /@ caseIndicationsGrouped,
			(* ex. "JustTesting" *)
			{
				Repeated[{False}, {0, 1}],
				Repeated[
					PatternSequence[{True}, {False}],
					{1, Infinity}
				],
				Repeated[{True}, {0, 1}]
			}
		]
	];

(*!
	\function CreateWorkingNotebook
	
	\calltable
		CreateWorkingNotebook[contents_, title_] '' Open a new notebook and create cells in the notebook that correspond to 'contents'.
*)
Clear[CreateWorkingNotebook];
Options[CreateWorkingNotebook] =
{
	"Evaluator" -> "Local",
	"File" -> Null,
	"RightHalfOfScreen" -> False,	(*< should we put the notebook on the right half of the screen? Useful if the calling notebooks expects itself to be on the left half of the screen. *)
	"LeftHalfOfScreen" -> False,	(*< should we put the notebook on the left half of the screen? *)
	"EvaluateImmediately" -> True,	(*< should the notebook evaluate its contents immediately when it's created? *)
	"YOffset" -> 0,					(*< can be used to alter the notebook's Y position on the screen. Useful when creating M10 notebooks if you want them to be at the same Y position as M9 notebooks on the screen. *)
	"Metadata" -> None,				(*< metadata to associate with the notebook. (key/value pairs, where keys are strings) *)
	"PreTitleContents" -> {}		(*< Notebook contents that should be put above the title? *)
};
Attributes[CreateWorkingNotebook] = {HoldFirst};
CreateWorkingNotebook[contents_, title_:Null, OptionsPattern[]] :=
	Module[{notebook, screenWidth, screenHeight},
		
		{screenWidth, screenHeight} = GetScreenDimensions[];
		
		(* Lop off some screen height to account for Windows 7 taskbar, etc.
		   Not an exact science. *)
		screenHeight -= 40;
		
		notebook =
			CreateDocument[
				DocumentNotebook[
					{
						Sequence @@
							createWorkingNotebookReplacements[
								OptionValue["PreTitleContents"]
							],
						If [title =!= Null, TextCell[title, "Title"], Sequence @@ {}],
						Sequence @@
							createWorkingNotebookReplacements[contents]
					},
					Evaluator -> OptionValue["Evaluator"],
					If [TrueQ[OptionValue["RightHalfOfScreen"]],
						Sequence @@
						{
							WindowSize -> {screenWidth / 2 - 2, screenHeight - 12},
							WindowMargins -> {{screenWidth / 2 - 7.5, Automatic}, {0, Automatic}}
						}
						,
						If [TrueQ[OptionValue["LeftHalfOfScreen"]],
							Sequence @@
							{
								WindowSize ->
									{
										screenWidth / 2 - 2,
										screenHeight - 12
									},
								WindowMargins -> {{-7, Automatic}, {Automatic, OptionValue["YOffset"]}}
							}
							,
							Sequence @@
							{
								WindowSize -> {screenWidth, screenHeight},
								WindowMargins -> {{0, Automatic}, {-OptionValue["YOffset"], Automatic}}
							}
						]
					]
				]];
		
		If [TrueQ[OptionValue["EvaluateImmediately"]],
			SelectionMove[notebook, All, Notebook];
			FrontEndTokenExecute[notebook, "EvaluateCells"];
		];
		
		(* If any metadata was specified, set it. *)
		If [OptionValue["Metadata"] =!= None,
			(
				CurrentValue[notebook, {TaggingRules, #[[1]]}] = #[[2]]
			) & /@ OptionValue["Metadata"]
		];
			
		If [OptionValue["File"] =!= Null,
			NotebookSave[notebook, OptionValue["File"]];
		];
		
		notebook
	]

(*!
	\function createWorkingNotebookReplacements
	
	\calltable
		createWorkingNotebookReplacements[contents] '' replaces the syntax specification supported by CreateWorkingNotebook with that used by DocumentNotebook.
	
	Examples:
	
	createWorkingNotebookReplacements[{Output["Here"]}]

	===

	{TextCell["Here", "Output"]}

	Unit tests:

	RunUnitTests[WUtils`WUtils`createWorkingNotebookReplacements]

	\related 'CreateWorkingNotebook
	
	\maintainer danielb
*)
Attributes[Code] = {HoldAllComplete};
createWorkingNotebookReplacements[contents_] :=
	Module[{items},
		
		(* Transforms Hold[{a, b, c}] into {Hold[a], Hold[b], Hold[c]} *)
		items = Map[HoldComplete, HoldComplete[contents], {2}][[1]];
		
		(* Since these aren't System symbols (?), users of this function that
		   aren't in this package will end up giving these symbols some other context.
		   So, we need to map them back to our namespace. *)
		items =
			ReplaceSymbols[
				items,
				{
					"Section" -> "WUtils`WUtils`Section",
					"Subsection" -> "WUtils`WUtils`Subsection",
					"Subitem" -> "WUtils`WUtils`Subitem",
					"Code" -> "WUtils`WUtils`Code",
					"CodeString" -> "WUtils`WUtils`CodeString",
					"Output" -> "WUtils`WUtils`Output",
					"BoxesCell" -> "WUtils`WUtils`BoxesCell"
				}
			];
		
		Replace[items,
			{
			HoldComplete[Sequence[]] :> Sequence[],
			HoldComplete[Text[x_]] :> TextCell[x, "Text"],
			HoldComplete[Item[x_]] :> TextCell[x, "Item"],
			HoldComplete[Subitem[x_]] :> TextCell[x, "Subitem"],
			HoldComplete[Section[x_]] :> TextCell[x, "Section"],
			HoldComplete[Subsection[x_]] :> TextCell[x, "Subsection"],
			HoldComplete[Code[x_List, opts___]] :> RenderHeldListOfCodeLines[Hold[x]],
			HoldComplete[Code[]] :> TextCell["", "Code", InitializationCell->False],
			HoldComplete[CodeString[x_, opts___]] :> ExpressionCell[RawBoxes[x], "Code", opts, InitializationCell->False],
			HoldComplete[BoxesCell[x_, opts___]] :> ExpressionCell[RawBoxes[x], "Input", opts],
			HoldComplete[Output[x_]] :> TextCell[x, "Output"],
			HoldComplete[(head:(TextCell | ExpressionCell))[args__]] :> head[args],
			HoldComplete[x_] :> ExpressionCell[Defer[x], "Code", InitializationCell->False]
			}
			,
			1
		]
	]

(*!
	\function ReplaceSymbols
	
	\calltable
		ReplaceSymbols[e, symbolMapping] '' Given an expression and a list of replacements such as {\"mySymbol\" -> \"mySymbol2\"}, makes the appropriate replacements. The LHS of each replacement rule is the SymbolName of the desired symbol.
	
	Example:
	
	ReplaceSymbols[
		HoldComplete[
			{
				Global`Code[1 + 1]
			}
		],
		{
		"Code" -> "MyContext`Code"
		}
	]
	
	===
	
	HoldComplete[{MyContext`Code[1 + 1]}]

	Unit tests:

	RunUnitTests[ReplaceSymbols]

	\maintainer danielb
*)
ReplaceSymbols[e_, symbolMappingIn_] :=
	Module[{symbolMapping = Append[symbolMappingIn, s_ -> None]},
		Replace[
			e,
			s_Symbol
			:>
			With[{
				 code =
					(
					Symbol[HeldSymbolName[HoldComplete[s]] /. symbolMapping]
					)
				 },
				code /; True
			]
			/; ((HeldSymbolName[HoldComplete[s]] /. symbolMapping) =!= None)
			,
			Infinity,
			Heads -> True
		]
	]
	
(*!
	\function ReplaceSymbolsUsingPatterns
	
	\calltable
		ReplaceSymbolsUsingPatterns[e, symbolMapping] '' Given an expression and a list of replacements such as {RegularExpression[\"mySymbol.+\"] -> \"mySymbol\"}, makes the appropriate replacements. The LHS of each replacement rule can be a string expression.
	
	This can be useful when testing the output of a function that involves dynamically
	generated symbol. We can replace those dynamically generated symbols (which involve
	unpredictable numeric portions) into predictable symbol names that don't have numeric
	parts.
	
	Example:
	
	ReplaceSymbolsUsingPatterns[
		HoldComplete[{bindingAddedDynamicallyByGetOptionsPatternBinding1410$}],
		{
		RegularExpression["bindingAddedDynamicallyByGetOptionsPatternBinding.+"] -> "bindingAddedDynamicallyByGetOptionsPatternBinding"
		}
	]
	
	===
	
	HoldComplete[{bindingAddedDynamicallyByGetOptionsPatternBinding}]

	Unit tests:

	RunUnitTests[ReplaceSymbolsUsingPatterns]

	\maintainer danielb
*)
ReplaceSymbolsUsingPatterns[e_, symbolMappingIn_] :=
	Module[{symbolMapping, notApplicableString = ToString[Unique["notApplicableSentinel"]]},
		   
		(* So that passing in a single replacement rule works. *)
		symbolMapping = Flatten[{symbolMappingIn}];
		   
		symbolMapping =
			Append[
				(StartOfString ~~ #[[1]] ~~ EndOfString -> #[[2]]) & /@ symbolMapping,
				StartOfString ~~ ___ ~~ EndOfString -> notApplicableString
			];
		
		Replace[
			e,
			s_Symbol
			:>
			With[{
				 code =
					(
					Symbol[StringReplace[HeldExpressionToString[HoldComplete[s]], symbolMapping]]
					)
				 },
				code /; True
			]
			/;
				(
				StringReplace[HeldExpressionToString[HoldComplete[s]], symbolMapping] =!= notApplicableString
				)
			,
			Infinity,
			Heads -> True
		]
	]

(* Given a HoldComplete[_Symbol], returns the symbol name as a string. *)
HeldSymbolName[s_] := StringReplace[HeldExpressionToString[s], __ ~~ "`" ~~ rest:__ :> rest]

(*!
	\function GetSymbolContext
	
	\calltable
		GetSymbolContext[symbolName] '' given a symbol (in string form), returns its likely context, which might be a Private context.
	
	The function works by first checking if the symbol is Global` or resolvable
	by the $ContextPath. If not, it checks whether it is a Private symbol
	in one of the packages on the $ContextPath.
	
	Example:
	
	GetSymbolContext["GetSymbolContext"] === "WUnits`WUtils`"

	Unit tests:

	RunUnitTests[WUtils`WUtils`GetSymbolContext]

	\related 'GetSymbolPackage
	
	\maintainer danielb
*)
GetSymbolContext[symbolName_String] :=
	Module[{context, expr, isGlobal},

		isGlobal = Names["Global`" <> symbolName] =!= {};
		If [TrueQ[isGlobal],
			Return["Global`"];
		];

		expr = ToExpression[symbolName, StandardForm, HoldComplete];

		(* Check whether it's on the $ContextPath. *)
		expr /. HoldComplete[sym_] :>
			(
				context = Context[sym];
				
				If [context === "Global`",
					(* If the symbol shows up as in Global`, but it
					   wasn't there when we checked earlier, then
					   that means we created it. Remove it. *)
					If [!TrueQ[isGlobal],
						Remove[sym];
					]
					,
					(* We found the symbol on the $ContextPath. *)
					Return[context];
				];
			);

		(* Otherwise, we haven't been able to find the
		   symbol. As a last ditch effort, examine the
		   private contexts... *)
		Function[{thisContext},
			With[{possiblePrivateSymbol = thisContext <> "Private`" <> symbolName},
				If [Names[possiblePrivateSymbol] =!= {},
					Return[StringDropByDelimiter[possiblePrivateSymbol, "`"] <> "`", Module];
				];
			];
			(* New package format. *)
			With[{possiblePrivateSymbol = thisContext <> "PackagePrivate`" <> symbolName},
				If [Names[possiblePrivateSymbol] =!= {},
					Return[StringDropByDelimiter[possiblePrivateSymbol, "`"] <> "`", Module];
				];
			];
		] /@ $ContextPath;

		None
	];
	
(* Given a string and delimiter, drops the last part of the string as distinguished by the delimiter. *)
StringDropByDelimiter[str_, delim_] := StringJoin[Riffle[Drop[StringSplit[str, delim], -1], delim]]

(*!
	\function GetSymbolPackage
	
	\calltable
		GetSymbolPackage[symbolName] '' given a symbol (in string form), returns its likely package. Also works with Private symbol, so long as their context is on the $ContextPath.
	
	Example:
	
	GetSymbolPackage["GetSymbolPackage"] === "WUtils`WUtils`"
	
	\related 'GetSymbolContext
	
	\maintainer danielb
*)
Clear[GetSymbolPackage];
GetSymbolPackage[symbolName_String] :=
	Module[{context},
		context = GetSymbolContext[symbolName];
		If [StringMatchQ[context, __ ~~ "`Private`"],
			StringDropByDelimiter[context, "`"] <> "`"
			,
			context
		]
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
Clear[InsertStringAfterMatch];
Options[InsertStringAfterMatch] =
{
	"LastMatch" -> False	   (*< insert wrt the last match, rather than the first match. *)
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
Clear[InsertStringBeforeMatch];
Options[InsertStringBeforeMatch] =
{
	"LastMatch" -> False	   (*< insert wrt the last match, rather than the first match. *)
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
Clear[InsertStringInFile];
Options[InsertStringInFile] =
{
	"AfterMatch" -> False,	  (*< By default, this function inserts the string at the position of the match. If AfterMatch -> True, the insertion is performed at the end of the matched string. *)
	"LastMatch" -> False,	   (*< insert wrt the last match, rather than the first match. *)
	"DataString" -> None		(*< If the file contents are already in memory, they can be passed in. In that case, they will be returned rather than written to disk. *)
};
InsertStringInFile[file_, strToInsert_, strToMatch_, OptionsPattern[]] :=
	Module[{data},
		If [!FileExistsQ[file],
			Print["InsertStringInFile: File doesn't exist: ", file];
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
				Print["InsertStringInFile: Failed Insertion: ", file];
				$Failed
			]
		]
	]

(*!
	\function GetFunctionArgumentNames
	
	\calltable
		GetFunctionArgumentNames[functionSymbol] '' given a function symbol, returns all sets of argument names that can be passed to it.
	
	Example:
	
	GetFunctionArgumentNames[CalculateTokenize] === {{"s", "opts"}}
	
	\maintainer danielb
*)
GetFunctionArgumentNames[functionSymbol_] :=
	Module[{downValues, args},
		downValues = DownValues[functionSymbol];
		DeleteDuplicates[
			Function[{downValue},
	
				(* Get the arguments from this down value. *)
				args = downValue[[1]] /. Verbatim[HoldPattern][_[args___]] :> {args};
	
				(* Remove blanks. *)
				args = args /. Verbatim[Pattern][s_Symbol, Blank[]] :> SymbolName[s];
				
				args = args /. Verbatim[Optional][s_, default_] :> s <> " : " <> ToString[default, InputForm];
				
				args = DeleteCases[args, Verbatim[OptionsPattern][], Infinity];
	
				args
	
			] /@ downValues
		]
	]

(* Opens Windows Explorer and selects the given file. (obviously won't work for Mac) *)
SelectFileInExplorer[file_] := (Run["explorer.exe", "/select," <> file];)
	
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
	\function dependenciesOfFile
	
	\calltable
		dependenciesOfFile[file, dependencies] '' given a file and known dependency relationships, return the dependents of the given file.
	
	Doesn't go beyond a max depth to somewhat protect against circular dependencies.
	
	Example:
	
	dependenciesOfFile[
		"C:\\Temp\\a.m"
		,
		{
			"C:\\Temp\\a.m" -> "C:\\Temp\\b.m",
			"C:\\Temp\\b.m" -> "C:\\Temp\\c.m"
		}
	]
	
	===
	
	{"C:\\Temp\\b.m", "C:\\Temp\\c.m"}
	
	\related 'ProcessFileDependencies
	
	\maintainer danielb
*)
Clear[dependenciesOfFile];
dependenciesOfFile[file_, dependencies_, depth_:0] :=
	Module[{res, foundDependencies, nonDirectDependencies = {}},
		
		If [depth > 15,
			Return[{}];
		];
		
		res = {};
		
		foundDependencies = DeleteDuplicates[Select[dependencies, #[[1]] === file &][[All, 2]]];

		(* Dependencies of dependencies *)
		Function[{foundDependency},
			nonDirectDependencies = Join[nonDirectDependencies, dependenciesOfFile[foundDependency, dependencies, depth + 1]];
		] /@ foundDependencies;
		
		DeleteCases[Join[foundDependencies, nonDirectDependencies], file]
	]

(*!
	\function RenderHeldListOfCodeLines
	
	\calltable
		RenderHeldListOfCodeLines[heldLines_List] '' given Hold[_List], a held list of code statements, render them as a code ExpressionCell.
	
	\related CreateWorkingNotebook
*)
Clear[RenderHeldListOfCodeLines];
RenderHeldListOfCodeLines[heldLines_] :=
	Module[{items, deferredItems, res},
		items = HeldListToListOfHeld[heldLines] /. Hold[Global`MultiLineFunc[f_][args___]] :> RenderHeldFunctionIntoMultipleLines[Hold[f[args]]];
		deferredItems = If [FreeQ[#, AlreadyRendered], (Defer @@ #), #] & /@ items;
		
		res = 
			ExpressionCell[
				RawBoxes[
					RowBox@
					Riffle[
						If [FreeQ[#, AlreadyRendered],
							ToBoxes[#, StandardForm]
							,
							#
						] & /@ deferredItems,
						"\[IndentingNewLine]"
					]
				]
				,
				"Code"
				,
				InitializationCell -> False
			];
			
		res = res /. AlreadyRendered[x_] :> Sequence @@ x;
		
		res
	]
	
(*!
	\function RenderHeldFunctionIntoMultipleLines
	
	\calltable
		RenderHeldFunctionIntoMultipleLines[e_] '' Attempt at getting multi-line function calls to work wrt CreateWorkingNotebook.
		
	Wrap a function call like this:
	
	MultiLineFunc[MyFunc][
		1,
		2,
		3
	]
		
	\related RenderHeldListOfCodeLines,  CreateWorkingNotebook
*)
RenderHeldFunctionIntoMultipleLines[e_] :=
	Module[{args, head},
		args = (Defer @@ #) & /@ HeldListToListOfHeld[e /. Hold[_[args___]] :> Hold[{args}]];
		head = e /. Hold[head_[___]] :> head;
		
		AlreadyRendered[
			Join[
				{
				ToString[head],
				"[",
				"\[IndentingNewLine]"
				}
				,
				Riffle[
					Riffle[
						Riffle[
							ToBoxes[#, StandardForm] & /@ args,
							","
						],
						"\[IndentingNewLine]", 3
					],
					"	", {1, -2, 4}
				]
				,
				{
				"\[IndentingNewLine]",
				"]",
				";"
				}
			]
		]
	]

(*!
	\function GetScreenDimensions
	
	\calltable
		GetScreenDimensions[] '' returns the width and height of the screen.
	
	\maintainer danielb
*)
GetScreenDimensions[] := GetScreenDimensions[1]
GetScreenDimensions[screenNum_] :=
	{#[[1,1]], #[[2,1]]} & @ Map[Differences, "ScreenArea" /. SystemInformation["Devices", "ScreenInformation"][[screenNum]]]

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

(* Given a HoldComplete[_Symbol], returns the symbol name as a string. *)
HeldSymbolName[s_] := StringReplace[HeldExpressionToString[s], __ ~~ "`" ~~ rest:__ :> rest]

(*!
	\function RenderHeldListOfCodeLines
	
	\calltable
		RenderHeldListOfCodeLines[heldLines_List] '' given Hold[_List], a held list of code statements, render them as a code ExpressionCell.
	
	\related CreateWorkingNotebook
*)
Clear[RenderHeldListOfCodeLines];
RenderHeldListOfCodeLines[heldLines_] :=
	Module[{items, deferredItems, res},
		items = HeldListToListOfHeld[heldLines] /. Hold[Global`MultiLineFunc[f_][args___]] :> RenderHeldFunctionIntoMultipleLines[Hold[f[args]]];
		deferredItems = If [FreeQ[#, AlreadyRendered], (Defer @@ #), #] & /@ items;
		
		res = 
			ExpressionCell[
				RawBoxes[
					RowBox@
					Riffle[
						If [FreeQ[#, AlreadyRendered],
							ToBoxes[#, StandardForm]
							,
							#
						] & /@ deferredItems,
						"\[IndentingNewLine]"
					]
				]
				,
				"Code"
				,
				InitializationCell -> False
			];
			
		res = res /. AlreadyRendered[x_] :> Sequence @@ x;
		
		res
	]

(*!
	\function RenderHeldFunctionIntoMultipleLines
	
	\calltable
		RenderHeldFunctionIntoMultipleLines[e_] '' Attempt at getting multi-line function calls to work wrt CreateWorkingNotebook.
		
	Wrap a function call like this:
	
	MultiLineFunc[MyFunc][
		1,
		2,
		3
	]
		
	\related RenderHeldListOfCodeLines,  CreateWorkingNotebook
*)
RenderHeldFunctionIntoMultipleLines[e_] :=
	Module[{args, head},
		args = (Defer @@ #) & /@ HeldListToListOfHeld[e /. Hold[_[args___]] :> Hold[{args}]];
		head = e /. Hold[head_[___]] :> head;
		
		AlreadyRendered[
			Join[
				{
				ToString[head],
				"[",
				"\[IndentingNewLine]"
				}
				,
				Riffle[
					Riffle[
						Riffle[
							ToBoxes[#, StandardForm] & /@ args,
							","
						],
						"\[IndentingNewLine]", 3
					],
					"	", {1, -2, 4}
				]
				,
				{
				"\[IndentingNewLine]",
				"]",
				";"
				}
			]
		]
	]

(*!
	\function SetCellMetadata
	
	\calltable
		SetCellMetadata[cellObjectOrNotebook, key -> value] '' given a cell object, sets the TaggingRules value for the given key.
	
	Note: Can also be used to set a notebook's metadata.
	
	Examples:
	
	SetCellMetadata[
		myCellObject,
		"MyKey" -> "MyValue"
	]
	
	\related 'GetCellMetadata
	
	\maintainer danielb
*)
Clear[SetCellMetadata];
SetCellMetadata[cellObjectOrNotebook_, key_ -> value_] :=
	Module[{},
		
		{key -> value};
		
		SetOptions[
			cellObjectOrNotebook,
			TaggingRules ->
				Sett[
					Gett[NotebookRead[cellObjectOrNotebook], TaggingRules, {}],
					key -> value
				]
		]
	]

(*!
	\function KeyValueGet
	
	\calltable
		KeyValueGet[keyValues_, key_] '' given either a list of key/value pairs (as rules), or key/value pairs with a non-List head (ex. Subparse["MyKey" -> "MyVal", ...]), and given a key, return the value. If the key is not defined, return Missing[].
	
	Example:
	
	KeyValueGet[{"a" -> "b", "c" -> "d"}, "a"] = "b"
	
	\related 'KeyValueSet
	
	\maintainer danielb
*)
KeyValueGet[keyValues_List, key_, defaultValue_:Missing[]] :=
	With[{res = key /. List@@Cases[keyValues, _Rule]},
		If [res === key,
			defaultValue
			,
			res
		]
	]

KeyValueGet[keyValues_Association, key_, defaultValue_:Missing[]] :=
	Lookup[keyValues, key, defaultValue]

KeyValueGet[keyValues_List, keyList_List, defaultValue_:Missing[]] :=
	Module[{val},
		val = keyValues;
		Function[{key},
			val = KeyValueGet[val, key, defaultValue];
		] /@ keyList;
		val
	]

(*!
	\function KeyValueGet
	
	\calltable
		KeyValueGet[keyValues_, key_] '' given either a list of key/value pairs (as rules), or key/value pairs with a non-List head (ex. Subparse["MyKey" -> "MyVal", ...]), and given a key, return the value. If the key is not defined, return Missing[].
	
	Example:
	
	KeyValueGet[{"a" -> "b", "c" -> "d"}, "a"] = "b"
	
	\related 'KeyValueSet
	
	\maintainer danielb
*)
KeyValueGet[keyValues_List, key_, defaultValue_:Missing[]] :=
	With[{res = key /. List@@Cases[keyValues, _Rule]},
		If [res === key,
			defaultValue
			,
			res
		]
	]

KeyValueGet[keyValues_List, keyList_List, defaultValue_:Missing[]] :=
	Module[{val},
		val = keyValues;
		Function[{key},
			val = KeyValueGet[val, key, defaultValue];
		] /@ keyList;
		val
	]
	
Gett = KeyValueGet;

(*!
	\function KeyValueSet
	
	\calltable
		KeyValueSet[keyValues_, key_ -> value_] '' given either a list of key/value pairs (as rules), or key/value pairs with a non-List head (ex. Subparse["MyKey" -> "MyVal", ...]), and given a key + value, return the key value pairs with the given key set to the given value.
		KeyValueSet[keyValuesIn_, keyValueList_List] '' as bove, but setting multiple key/value pairs.
		KeyValueSet[keyValuesIn_, keyList_List -> value_] '' allows a list of keys to be specified, which are interpreted as a hierarchy.
	
	Example:
	
	KeyValueSet[{"a" -> "b", "c" -> "d"}, "a" -> "x"] = {"a" -> "x", "c" -> "d"}
	KeyValueSet[{"a" -> "b", "c" -> "d"}, {"a" -> "x", "b" -> "e"}] = {"a" -> "x", "c" -> "d", "b" -> "e"}
	KeyValueSet[{"a" -> 1, "b" -> {"c" -> 2}}, {"b", "c"} -> 3] = {"a" -> 1, "b" -> {"c" -> 3}}
	
	\related 'KeyValueGet
	
	\maintainer danielb
*)
Clear[KeyValueSet];
KeyValueSet[keyValues_, key_ -> value_] :=
	With[{pos = Position[keyValues, key, {2}]},
		If [pos === {},
			Append[keyValues, key -> value]
			,
			ReplacePart[keyValues, pos[[1, 1]] -> (key -> value)]
		]
	]
	
KeyValueSet[keyValuesIn_, keyValueList_List] :=
	Module[{keyValues = keyValuesIn},
		Function[{keyValue},
			keyValues = KeyValueSet[keyValues, keyValue]
		] /@ keyValueList;
		
		keyValues
	]
	
KeyValueSet[keyValues_, keyList_List -> value_] :=
	KeyValueSet[
		keyValues,
		With[{remainderKeyList = keyList[[2;;]], prev = KeyValueGet[keyValues, First[keyList]]},
			First[keyList] -> KeyValueSet[If [prev =!= Missing[], prev, {}], If [Length[remainderKeyList] === 1, First[remainderKeyList], remainderKeyList] -> value]
		]
	]

Sett = KeyValueSet

(*!
	\function RemoveIndentation
	
	\calltable
		RemoveIndentation[str] '' given a string, remove any top-level indentation
	
	RemoveIndentation["   Blah[\n		1\n	]"] === "Blah[\n	1\n]"
	
	\maintainer danielb
*)
RemoveIndentation[str_] :=
	Module[{topLevelIndent},
		topLevelIndent =
			StringReplace[
				StringCases[str, RegularExpression["^[ \t]*"]][[1]],
				(* 4 spaces per tab *)
				"\t" :> "	"
			];
			
		If [topLevelIndent === "",
			str
			,
			StringTake[
				StringReplace[
					StringReplace[str, "\t" :> "	"],
					"\n" <> topLevelIndent :> "\n"
				],
				{StringLength[topLevelIndent] + 1, -1}
			]
		]
	]

(*!
	\function CreateFunctionInFile
	
	\calltable
		CreateFunctionInFile[file, callSignature] '' given a call signature, creates a new function in the given file.
	
	Examples:
	
	CreateFunctionInFile[
		FindFile["WUtils`WUtils`"],
		"MyNewFunc[arg1, arg2, arg3]"
	]
	
	\maintainer danielb
*)
Clear[CreateFunctionInFile];
Clear[CreateFunctionInFile];
Options[CreateFunctionInFile] =
{
	"Description" -> None		(*< the plain-English description of what the function does. If specified, placed in the Mathdoc comment. *)
};
CreateFunctionInFile::cdc = "Couldn't determine context for file `1`";
CreateFunctionInFile[file_, callSignature_String, opts:OptionsPattern[]] :=
	Module[{code, res, funcName, context, newPackageFormatQ = False},
		
		
		context = FileToContext[file];
		
		If [!StringQ[context],
			Message[CreateFunctionInFile::cdc, file];
			Return[$Failed];
		];
		
		code = CreateFunction[callSignature, "Description" -> OptionValue["Description"], "UseTabs" -> (TabsOrSpaces[context] === "Tabs")];
		
		If [NewPackageFormatQ[file],
			newPackageFormatQ = True;
			AppendToFile[file, code <> "\n\n"];
			res = Null;
			,
			res =
			InsertStringInFile[
				file,
				code <> "\n\n",
				StartOfLine ~~ "End[]",
				"LastMatch" -> True
			];
		];
		
		If [res =!= $Failed,
			
			funcName = StringReplace[callSignature, f:WLSymbolPattern[] ~~ "[" ~~ __ :> f];

			If [TrueQ[GivePrivateSymbolsADefiniteContext[context] && !UpperCaseQ[StringTake[funcName, 1]]],
				MakePrivateSymbolContextDefinite[file, funcName];
			];
			
			(* The AutoExport mechanism, upon file reload, will export
			   any new function symbols if they start with a capital
			   letter. *)
			ReloadFiles[];

			If [StringQ[context],
				If [MemberQ[Names[context <> "*"], funcName | context <> funcName],
					res = WorkOn["Function", ToExpression[context <> funcName], "File" -> file];
					,
					If [MemberQ[Names[context <> "Private`*"], funcName | context <> "Private`" <> funcName],
						res = WorkOn["Function", ToExpression[context <> "Private`" <> funcName], "File" -> file];
						,
						(* New package format? *)
						If [TrueQ[newPackageFormatQ],
							
							WorkOn["Function", funcName, "File" -> file];
						];
					]
				];
				,
				res = $Failed
			];
		];
		
		res
	]

(*!
	\function ReloadFiles
	
	\calltable
		ReloadFiles[] '' run reload functions to load modified files.
	
	\maintainer danielb
*)
ReloadFiles[] :=
	If [ListQ[Global`$ReloadFunctions],
		#[] & /@ Global`$ReloadFunctions
	]

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
If [ListQ[Global`$ReloadFunctions],
	Global`$ReloadFunctions =
		DeleteDuplicates[
			Append[Global`$ReloadFunctions, ReloadWUtils]
		]
	];

(*!
	\function CreateFunction
	
	\calltable
		CreateFunction[functionAndArgsString] '' given a function call (as a string), create a corresponding new function with an appropriate top-of-function comment.
	
	Example:
	
	CreateFunction["performSrMap[sr, mapping]"]

	Unit tests:

	RunUnitTests[WUtils`WUtils`CreateFunction]

	\maintainer danielb
*)
Clear[CreateFunction];
Options[CreateFunction] =
{
	"Description" -> None,		  (*< the plain-English description of what the function does. If specified, placed in the Mathdoc comment. *)
	"CopyToClipboard" -> False,	 (*< Copy the resulting function to the clipboard? *)
	"UseTabs" -> False				(*< Use tabs for indentation? *)
};
CreateFunction[argIn_:$CodeAssistDefault, OptionsPattern[]] := 
	Module[{str, res, parts, functionName, args, functionAndArgs, fromClipboard = False,
			desc = "comment", examples},
		
		{functionAndArgs, fromClipboard} = getCodeAssistArg[argIn];
		
		res =
		If [CouldBeFunctionCall[functionAndArgs],
			
			parts = StringTrim /@ StringSplit[functionAndArgs, "[" | "," | "]"];
			
			functionName = parts[[1]];
			args = parts[[2;;]];
			
			If [OptionValue["Description"] =!= None,
				desc = OptionValue["Description"];
			];
			
			examples = "";
			
			If [!TrueQ[FunctionWithNoArgs[functionAndArgs]],
				examples = "\n\n	Examples:
	
	" <> functionAndArgs <> " === TODO";
			];
				
			str =
"(*!
	\\function " <> functionName <> "
	
	\\calltable
		" <> functionAndArgs <> " '' " <> desc <> examples <> "
	
	\\related '
	
	\\maintainer " <> Username[] <> "
*)
"
(* Better to use Block instead of Module, since it's faster. *)
<> functionName <> "[" <> StringJoin[Riffle[(# <> "_") & /@ args, ", "]] <> "] :=
	Block[{},
		TODO
	]" <>
(* Put a semi-colon at the end of the function incase this is a code file
   that is wrapped in some other code, such as a With, which I sometimes
   do. A bit sad to pollute normal files with unecessary semi-colons,
   but oh well. *)
";";
			str
			,
			If [codeAssistInvalidArguments[fromClipboard, functionAndArgs] === $Failed, Return[$Failed]];
		];
		
		If [TrueQ[OptionValue["UseTabs"]],
			res = StringReplace[res, "	" -> "\t"];
		];
		
		(* ex. Va["create function Blah[blah1, blah2] \"My description\""] *)
		If [TrueQ[OptionValue["CopyToClipboard"]],
			CopyToClipboard[res];
		];
		
		codeAssistPostlude[fromClipboard, res]
	]

(*!
	\function getCodeAssistArg
	
	\calltable
		getCodeAssistArg[arg] '' takes the argument passed to a code assist function, which should have a default of $CodeAssistDefault if none was specified. If none was specified, use the clipboard's value.
	
	Return value:
	
	{arg, fromClipboard}
	
	ex. {"myArg", True}
	
	\maintainer danielb
*)
getCodeAssistArg[argIn_] :=
	If [argIn === $CodeAssistDefault,
		{GetClipboard[], True}
		,
		{argIn, False}
	]

(*!
	\function CouldBeFunctionCall
	
	\calltable
		CouldBeFunctionCall[str] '' returns True if the string could be a function call, such as: "myFunction[arg1, arg2]"
	
	Example:
	
	CouldBeFunctionCall["myFunction[arg1, arg2]"] === True
	
	\maintainer danielb
*)
CouldBeFunctionCall[str_] := StringMatchQ[str, __ ~~ "[" ~~ ___ ~~ "]"]

(*!
	\function FunctionWithNoArgs
	
	\calltable
		FunctionWithNoArgs[functionAndArgs] '' returns True if the given function signature has zero arguments.

	Examples:
	
	FunctionWithNoArgs["myFunc[]"] === True

	Unit tests:

	RunUnitTests[WUtils`WUtils`FunctionWithNoArgs]

	\maintainer danielb
*)
FunctionWithNoArgs[functionAndArgs_] :=
	Module[{},
		StringMatchQ[
			functionAndArgs,
			___ ~~ "[]"
		]
	]

(*!
	\function codeAssistInvalidArguments
	
	\calltable
		codeAssistInvalidArguments[fromClipboard, arg] '' if the arguments gotten by a code assist are invalid, we return $Failed. If the arguments were gotten from the clipboard, we speak "Failed" to indicate failure to the user.
	
	\maintainer danielb
*)
codeAssistInvalidArguments[fromClipboard_, arg_] :=
	(
	If [TrueQ[fromClipboard], Speak["Failed"]];
	$Failed
	)

(*!
	\function codeAssistPostlude
	
	\calltable
		codeAssistPostlude[fromClipboard, res] '' if the code assist pertains to clipboard context, then put the result into the clipboard and speak "OK" to indicate success. (since the user may not be able to see anything on their screen to know whether the operation succeeded)
	
	\maintainer danielb
*)
codeAssistPostlude[fromClipboard_, res_] :=
	(
	If [TrueQ[fromClipboard],
		Speak["OK"];
		CopyToClipboard[res];
	];
	
	res
	)

(*!
	\function AppendToFile
	
	\calltable
		AppendToFile[file, str] '' append the given string to the given file

	Examples:
	
	WithTemporaryFiles[
		{myFile = "abc\n\n\n\n"},
		(
			AppendToFile[myFile, "NEW"];
			Import[myFile, "Text"]
		)
	]

	===

	"abc\n\nNEW"

	Unit tests:

	RunUnitTests[WUtils`WUtils`AppendToFile]

	\maintainer danielb
*)
Clear[AppendToFile];
Options[AppendToFile] =
{
	"NumberOfDelimitingNewlines" -> 2	   (* The number of newlines that should separate the previous last contents of the file and the newly appended string. *)
};
AppendToFile[file_, str_, OptionsPattern[]] :=
	Module[{existingContents, i, numNewlines, existingContentsWithoutTrailingNewlines},
		
		existingContents = Import[file, "Text"];
 
		i = StringLength[existingContents];
		While [i > 0 && StringTake[existingContents, {i}] === "\n",
			--i;
		];
		
		numNewlines = StringLength[existingContents] - i;
		
		existingContentsWithoutTrailingNewlines = StringTake[existingContents, {1, StringLength[existingContents] - numNewlines}];
		
		Export[
			file,
			existingContentsWithoutTrailingNewlines
			<>
			StringJoin[Table["\n", {OptionValue["NumberOfDelimitingNewlines"]}]]
			<>
			str
			<>
			"\n"
			,
			"Text"
		]
	];

(*!
	\function GivePrivateSymbolsADefiniteContext
	
	\calltable
		GivePrivateSymbolsADefiniteContext[context] '' given a context, returns True if private symbols defined in its corresponding files should be given a definite context. For example, private symbols in MachineLearning` by default are given an unpredictable context based on how ML code is loaded.
	
	\maintainer danielb
*)
GivePrivateSymbolsADefiniteContext[context_] :=
	Module[{val},
		val = GetVariablePossiblyFromParentPackage[context, "$GivePrivateSymbolsADefiniteContext"];
		
		If [MissingQ[val],
			False
			,
			val[[2]]
		]
	];

(*!
	\function MakePrivateSymbolContextDefinite
	
	\calltable
		MakePrivateSymbolContextDefinite[file, symbolName] '' adds a line to the file to give the private symbol a definite context. Useful in the special case that a non-default file loader is used which makes private symbol contexts unpredictable.
	
	\maintainer danielb
*)
Clear[MakePrivateSymbolContextDefinite];
Options[MakePrivateSymbolContextDefinite] =
{
	"DataString" -> None		(*< If the file contents are already in memory, they can be passed in. In that case, they will be returned rather than written to disk. *)
};
MakePrivateSymbolContextDefinite[file_, symbolName_String, OptionsPattern[]] :=
	Module[{privateContext},
		
		privateContext = FileToCustomPrivateContext[file];
		If [privateContext === $Failed,
			Return[$Failed];
		];
		
		With[{line = symbolName <> " = " <> privateContext <> symbolName <> ";"},
			InsertStringInFile[
				file,
				line <> "\n",
				StartOfLine ~~ WhitespaceCharacter... ~~ ("PackageScope" | "PackageExport" | "PackageImport") ~~ "[" ~~ Shortest[__] ~~ "]" ~~ RepeatedNull["\n"],
				"LastMatch" -> True,
				"AfterMatch" -> True,
				"DataString" -> OptionValue["DataString"]
			]
		]
	];

(*!
	\function FileToCustomPrivateContext
	
	\calltable
		FileToCustomPrivateContext[file] '' given a file, read it and look for the ThisFileImplementsContext[...] that indicates what private context should be assigned to private symbols we add to the file.
	
	\maintainer danielb
*)
Clear[FileToCustomPrivateContext];
Options[FileToCustomPrivateContext] =
{
	"FileContents" -> Automatic			(*< If the file contents are already in memory, they can be passed in. *)
};
FileToCustomPrivateContext[file_, OptionsPattern[]] :=
	Module[{contents, cases},
		If [OptionValue["FileContents"] === Automatic,
			contents = Import[file, "Text"];
			,
			contents = OptionValue["FileContents"];
		];
		cases = StringCases[contents, "ThisFileImplementsContext[\"" ~~ Shortest[inner__] ~~ "\"]" :> inner];
		If [cases === {},
			$Failed
			,
			cases[[1]]
		]
	];

(*!
	\function WorkOn
	
	\calltable
		WorkOn["Function", funcSymbol] '' initiates work on the given function. If an existing notebook exists for that function, it is opened. If not, it is created. Also opens the function in Workbench.
		WorkOn[project] '' given a project, can be used to tie into functionality that sets up your working environment, such as opening programs, opening source files in Workbench, navigating to folders in Wolfram Workbench, opening notebooks, arranging windows, etc.
	
	Examples:
	
	WorkOn["Function", WUtils`WUtils`SymbolToFile]
	
	\related 'EditFunction 'CreateIssueNotebook 'OpenIssueNotebook
	
	\maintainer danielb
*)
Clear[WorkOn];
Options[WorkOn] =
{
	"File" -> Automatic		 (*< The file that defines the function, or Automatic if this function should try and determine it. *)
};
WorkOn::gns = "Symbol `1` is in Global namespace."
WorkOn["Function", funcSymbol_, OptionsPattern[]] :=
	Module[{},
		
		If [Context[funcSymbol] === "Global`",
			Message[WorkOn::gns, funcSymbol];
			Return[$Failed];
		];
		
		If [OptionValue["File"] === Automatic,
			EditFunction[funcSymbol];
			,
			EditFunction[funcSymbol, OptionValue["File"]];
		];
		
		CreateIssueNotebook[
			(* Odd that we're turning the symbol back into a string just to
			   have it re-resolved into a symbol. *)
			"Name" -> If [StringQ[funcSymbol], funcSymbol, SymbolName[funcSymbol]],
			"Type" -> "Function"
		];
		
	]
	
WorkOn[a:"Function"] :=
	With[{clipboard = GetClipboard[]},
		If [CouldBeWLSymbolQ[clipboard],
			
			ReloadFiles[];
			
			WorkOn[
				a,
				StringToSymbol[clipboard]
			]
			,
			Print["Clipboard contents don't appear to be a function name: ", InputForm[clipboard]];
			$Failed
		]
	]

WorkOn[project_] :=
	Module[{},
		"WorkOn[" <> ToString[project, InputForm] <> "] hasn't been implemented yet. See: WUtils`WUtils`WorkOn"
	]
	
WorkOn["WUtils"] :=
	Module[{},
		(* Forces the folder to open. *)
		SelectFileInWorkbench[FindFile["WUtils`WUtils`"]];
		SelectFolderInWorkbench[FileNameDrop[FindFile["WUtils`WUtils`"], -1]];
	]

(*!
	\function RunUnitTests
	
	\calltable
		RunUnitTests[funcSymbol] '' given a function symbol, run its unit tests.
		RunUnitTests[context] '' given a context, run its unit tests.
	
	Examples:
	
	RunUnitTests[CanonicalLinguisticPattern]
	
	\related 'CreateUnitTests
	
	\maintainer danielb
*)

Clear[RunUnitTests];
Options[RunUnitTests] =
{
	"IncludeSlowTests" -> False,		(*< even run terribly slow "unit" tests? *)
	"RerunFailures" -> False,		   (*< only re-run failures? *)
	"SubTest" -> None					(*< Only run a particular sub-test file. *)
};

RunUnitTests[funcSymbol_Symbol, OptionsPattern[]] :=
	Module[{files},
		files = UnitTestFilename[funcSymbol, "SubTest" -> OptionValue["SubTest"], "ReturnList" -> True];
		
		If [MatchQ[files, {__String}],
			files = Select[files, FileExistsQ];
			If [files === {},
				Print["No tests have been added for ", ToString[funcSymbol], "."];
				,
				RunUnitTestsUi[files, "RerunFailures" -> OptionValue["RerunFailures"]]
			]
			,
			Print["No tests have been added for ", ToString[funcSymbol], "."];
		]
	]
	
(*!
	\function HasUnitTests
	
	\calltable
		HasUnitTests[funcSymbol] '' returns True if the given function has a unit test file.
	
	Examples:
	
	HasUnitTests[CanonicalLinguisticPattern] === True
	
	\related 'RunUnitTests
	
	\maintainer danielb
*)
HasUnitTests[funcSymbol_] :=
	Module[{file},
		file = UnitTestFilename[funcSymbol];
		If [!StringQ[file] || !FileExistsQ[file],
			False
			,
			True
		]
	]

RunUnitTests[context_String, OptionsPattern[]] :=
	Module[{files},
		If [FileExistsQ[context] && !DirectoryQ[context],
			RunUnitTestsUi[{context}, "RerunFailures" -> OptionValue["RerunFailures"]]
			,
			files = UnitTestFilenames[context, "IncludeSlowTests" -> OptionValue["IncludeSlowTests"]];
			If [!ListQ[files] || files === {},
				Print["No tests have been added for ", ToString[context], "."];
				,
				RunUnitTestsUi[files, "RerunFailures" -> OptionValue["RerunFailures"]]
			]
		]
	]

(*!
	\function RunUnitTestsUi
	
	\calltable
		RunUnitTestsUi[files] '' given one or more test files, run them and display the results, along with buttons for things like 'Rerun Tests', 'Rerun Failures', etc.
	
	\maintainer danielb
*)
Clear[RunUnitTestsUi];
Options[RunUnitTestsUi] =
{
	"RerunFailures" -> False			(*< only re-run failures? *)
};
RunUnitTestsUi[files_, OptionsPattern[]] :=
	Module[{dynamicOutputVar, res, outerDynamicOutputVar},
		With[{rerunFailures = OptionValue["RerunFailures"]},
			
			outerDynamicOutputVar = DynamicOutputSectionVar[];
			dynamicOutputVar = DynamicOutputSectionVar[];
			
			With[{parentDynamicOutputVar = WUtils`WUtils`Private`$ButtonDynamicOutputSectionVar},
				res =
				DynamicOutputSection[
					Row[{
						SmartButton[
							"Rerun Tests",
							RedirectPrintsAndMessagesToDynamicOutputSection[
								RunTestFiles[files, "RerunFailures" -> rerunFailures],
								dynamicOutputVar,
								"IgnoreMessages" -> True
							];
						],
						" ",
						SmartButton[
							"Rerun Failures",
							RedirectPrintsAndMessagesToDynamicOutputSection[
								RunTestFiles[files, "RerunFailures" -> rerunFailures],
								dynamicOutputVar,
								"IgnoreMessages" -> True
							];
						],
						" ",
						SmartButton[
							"OK",
							With[{var = parentDynamicOutputVar},
								If [MatchQ[var, _HoldComplete],
									(* If we're inside the output of a button, be polite and
									   set its dyanmic output to blank when the user clicks OK,
									   since deleting the evaluation cell would destroy the
									   button itself, and/or the larger UI it's a part of. *)
									SetHeldVar[var, ""];
									,
									NotebookDelete[EvaluationCell[]]
								];
							];
						]
					}],
					dynamicOutputVar,
					"NakedSection" -> True
				];
			];
			
			RedirectPrintsAndMessagesToDynamicOutputSection[
				RunTestFiles[files, "RerunFailures" -> rerunFailures],
				dynamicOutputVar,
				"IgnoreMessages" -> True
			];
			
			SetHeldVar[outerDynamicOutputVar, res];
			
			outerDynamicOutputVar /. HoldComplete[sym_] :>
				Dynamic[
					sym
				]
		]
	];

(*!
	\function RunTestFile
	
	\calltable
		RunTestFile[mtFile] '' runs a .mt file and returns the list of TestResult objects.
	
	\maintainer danielb
*)
RunTestFile[mtFile_] :=
	Block[{tests, expressions, res},
		If [!FileExistsQ[mtFile], Print["RunTestFile: File not found: ", mtFile]; Return[$Failed]];
		expressions = Quiet@ReadList[mtFile, Hold[Expression]];
		tests = Cases[expressions, Hold[_MUnit`Test|_MUnit`TestMatch|_VerificationTest], Infinity];
		res = ReleaseHold /@ tests
	];

(*!
	\function RunTestFiles
	
	\calltable
		RunTestFiles[files] '' run a list of .mt files, printing results using AntLog. Returns the total number of failures.
	
	\maintainer danielb
*)
Clear[RunTestFiles];
Options[RunTestFiles] =
{
	"PrintFiles" -> False,			(*< Print to indicate when running a file. *)
	"PrintSuccess" -> False,		(*< Print a line to indicate when a test was successful? *)
	"RerunFailures" -> False		(*< Only re-run the tests that failed last run. TODO *)
};
RunTestFiles[files_, OptionsPattern[]] :=
	Block[{testRes = {}, testFileRes, numFailures, numTests},
		
		numFailures = 0;
		numTests = 0;
		
		Function[{testFile},
			If [TrueQ[OptionValue["PrintFiles"]],
				Print["Running: " <> ToString[FileNameTake[testFile, -1]]];
			];
			testFileRes = RunTestFile[testFile];
			numFailures += Length[Select[testFileRes, (MUnit`FailureMode[#] =!= "Success") &]];
			numTests += Length[testFileRes];
			Function[{testResult},
				If [TrueQ[OptionValue["PrintSuccess"]] || MUnit`FailureMode[testResult] =!= "Success",
					Print[testResultToString[testResult]];
				]
			] /@ testFileRes;
			AppendTo[testRes, testFile -> testFileRes];
		] /@ files;
		
		If [numTests === 0,
			Print["No tests."];
			,
			If [numFailures === 0,
				Print["All ", numTests, " tests passed."];
			];
		];
		
		<|
			"NumFailures" -> numFailures,
			"NumTests" -> numTests
		|>
	];

(*!
	\function testResultToString
	
	\calltable
		testResultToString[testResult] '' converts a MUnit TestResult to a string.
	
	\maintainer danielb
*)
testResultToString[testResult_] :=
	Block[{str = "", out, actual, expected},
		
		out[line_] := (If [str =!= "", str = str <> "\n"]; str = str <> line); 
		
		If [MUnit`FailureMode[testResult] =!= "Success",
			out[TestID[testResult] <> ": FAILED"];
			out[Indent2[MUnit`TestInput[testResult], 1, 4]];
			
			(* For some reason, on cloud only, this returns the test wrapped in HoldForm
			   which has yet to be evaluated. Not sure why. To work around that,
			   I'll detect that case and force it to evaluate. *)
			actual = MUnit`ActualOutput[testResult];
			actual = Replace[actual, HoldForm[inner_] :> inner, {0}];
			
			expected = MUnit`ExpectedOutput[testResult];
			expected = Replace[expected, HoldForm[inner_] :> inner, {0}];
			
			out["	EXPECTED:"];
			out[Indent2[expected, 2, 4]];
			out["	ACTUAL:"];			
			out[Indent2[actual, 2, 4]];
			If [MUnit`ActualMessages[testResult] =!= MUnit`ExpectedMessages[testResult],
				If [MUnit`ExpectedMessages[testResult] =!= {},
					out["	EXPECTED MESSAGES:"];
					out[Indent2[MUnit`ExpectedMessages[testResult], 2, 4]];
					out["	ACTUAL MESSAGES:"];
					out[Indent2[MUnit`ActualMessages[testResult], 2, 4]];
					,
					out["	MESSAGES:"];
					out[Indent2[MUnit`ActualMessages[testResult], 2, 4]];
				]
			]
			,
			out[TestID[testResult] <> ": SUCCESS"];
		];
		
		str
	];

(*!
	\function UnitTestFilenames
	
	\calltable
		UnitTestFilenames[context] '' givne a context, returns the list of unit tests.
	
	Examples:
	
	UnitTestFilenames["WUtils`WUtils`"]
	
	\related 'UnitTestFilename
	
	\maintainer danielb
*)
Options[UnitTestFilenames] =
{
	"IncludeSlowTests" -> False		 (*< even run terribly slow "unit" tests? *)
};
UnitTestFilenames[contextOrDirectory_, opts:OptionsPattern[]] := UnitTestFilenamesImpl[contextOrDirectory, opts]

Options[UnitTestFilenamesImpl] = Options[UnitTestFilenames];
UnitTestFilenamesImpl[contextOrDirectory_, OptionsPattern[]] :=
	Module[{dir, files},
		
		(* But check if it is instead a directory. *)
		If [!StringFreeQ[contextOrDirectory, $PathnameSeparator] && FileExistsQ[contextOrDirectory] && DirectoryQ[contextOrDirectory],
			dir = contextOrDirectory;
			,
			dir = UnitTestDirectory[contextOrDirectory];
		];
		
		If [!StringQ[dir], Return[$Failed]];
		
		files = FileNames["*.mt", dir, Infinity];
		
		If [!TrueQ[OptionValue["IncludeSlowTests"]],
			TODO
		];
		
		files
	]

(*!
	\function RenameSymbolOrContext
	
	\calltable
		RenameSymbolOrContext[oldContext, oldSymbolName, newContext, newSymbolName] '' rename a symbol and/or its context. Updates the function notebook if it exists and the unit test file if it exists.
	
	\related 'MakeSymbolPrivate
	
	\maintainer danielb
*)
RenameSymbolOrContext[oldContext_String, oldSymbolName_String, newContext_String, newSymbolName_String] :=
	Block[{testFile, notebookFile, sourceFile, contents},
		
		With[{oldSymbol = ToExpression[oldContext <> oldSymbolName], newSymbol = ToExpression[newContext <> newSymbolName]},
			sourceFile = SymbolToFile[oldSymbol];
			If [!StringQ[sourceFile],
				sourceFile = SymbolToFile[newSymbol];
				If [StringQ[sourceFile],
					testFile = UnitTestFilename[newSymbol, "SourceFile" -> sourceFile];
				];
				,
				testFile = UnitTestFilename[oldSymbol, "SourceFile" -> sourceFile];
			];
			If [!StringQ[sourceFile],
				Print["RenameSymbolOrContext: Couldn't determine file that implements function."];
				Return[$Failed];
			];
		];
		
		If [StringQ[testFile] && FileExistsQ[testFile],
			RenameSymbolInTestFile[
				testFile,
				oldContext,
				oldSymbolName,
				newContext,
				newSymbolName,
				"SourceFile" -> sourceFile
			];
		];
		
		notebookFile = ResolveIssueNotebook[oldSymbolName];
		
		If [StringQ[notebookFile] && FileExistsQ[notebookFile],
			RenameSymbolInNotebook[
				notebookFile,
				oldContext,
				oldSymbolName,
				newContext,
				newSymbolName
			];
		];
		
	  	(* Replace any uses of the function in the current file. *)
	  	contents = Import[sourceFile, "Text"];
	  	contents =
			StringReplaceUsesOfSymbol[
				contents,
				oldContext,
				oldSymbolName,
				newContext,
				newSymbolName
			];
	  	
	  	If [StringQ[contents],
	  		Export[
	  			sourceFile,
	  			contents,
	  			"Text"
	  		];
	  		,
	  		Print["ERROR: RenameSymbolOrContext: Modified file contents aren't a String. Aborting."];
	  		Print["  File: ", sourceFile];
	  		Return[$Failed];
	  	];
	];

(*!
	\function RenameSymbolInTestFile
	
	\calltable
		RenameSymbolInTestFile[testFile, oldContext, oldSymbolName, newContext, newSymbolName] '' renames the a symbol and/or its context in a test file.
	
	\maintainer danielb
*)

Clear[RenameSymbolInTestFile];
Options[RenameSymbolInTestFile] =
{
	"SourceFile" -> Automatic			(*< The source file that implements the function. *)
};
RenameSymbolInTestFile[testFile_, oldContext_, oldSymbolName_, newContext_, newSymbolName_, OptionsPattern[]] :=
	Block[{testContents, res, newTestFile},
		
		testContents = Import[testFile, "Text"];
		
		If [testContents === $Failed || !StringQ[testContents],
			Print["RenameSymbolInTestFile: Couldn't read test file. Aborting."];
			Print["  Test file: ", testFile];
			Return[$Failed];
		];
		
	  	testContents =
			StringReplaceUsesOfSymbol[
				testContents,
				oldContext,
				oldSymbolName,
				newContext,
				newSymbolName
			];
		
		With[{newSymbol = ToExpression[newContext <> newSymbolName]},
			newTestFile = UnitTestFilename[newSymbol, "SourceFile" -> OptionValue["SourceFile"]];
		];
		
		If [!StringQ[newTestFile],
			Print["Couldn't determine test directory."];
			Return[$Failed];
		];
		  
		(* Rename the test file according to its new name, and write out the modified contents. *)
	
		If [ToLowerCase[newTestFile] === ToLowerCase[testFile],
			DeleteFile[testFile];
			With[{dir = DirectoryName[testFile]},
				If [FileNames["*", dir] === {},
					DeleteDirectory[dir];
				];
			];
			
			res = Export[newTestFile, testContents, "Text"];
			If [res === $Failed,
				Return[$Failed];
			];
			,
			res = Export[newTestFile, testContents, "Text"];
			If [res === $Failed,
				Return[$Failed];
			];
			
			If [newTestFile =!= testFile,
				DeleteFile[testFile];
				With[{dir = DirectoryName[testFile]},
					If [FileNames["*", dir] === {},
						DeleteDirectory[dir];
					];
				]
			];
		];
		
		res
	];

(*!
	\function StringReplaceUsesOfSymbol
	
	\calltable
		StringReplaceUsesOfSymbol[string, symbolContext, symbolName, newSymbolContext, newSymbolName] '' comment

	Examples:
	
	StringReplaceUsesOfSymbol[
		"MyContext`MySymbol = 1",
		"MyContext`",
		"MySymbol",
		"NewContext`",
		"NewSymbol"
	]

	===

	"NewContext`NewSymbol = 1"

	Unit tests:

	RunUnitTests[WUtils`WUtils`StringReplaceUsesOfSymbol]

	\maintainer danielb
*)
StringReplaceUsesOfSymbol[string_, symbolContext_, symbolName_, newSymbolContext_, newSymbolName_] :=
	Module[{},
  		StringReplace[
  			string,
  			{
  				(* Fully qualified uses of symbol (ie. with context) *)
  				WordBoundary ~~ (symbolContext <> symbolName | symbolContext <> "PackageScope`" <> symbolName) -> newSymbolContext <> newSymbolName,
  				WordBoundary ~~ symbolName <> "[" -> newSymbolName <> "[",
  				(* ex. Options[MyFunc] *)
  				"[" <> symbolName <> "]" -> "[" <> newSymbolName <> "]",
  				"\\function " <> symbolName -> "\\function " <> newSymbolName,
  				WordBoundary ~~ symbolName <> "::" -> newSymbolName <> "::",
  				(* Test IDs *)
  				"TestID" ~~ WhitespaceCharacter... ~~ "->" ~~ WhitespaceCharacter... ~~ "\"" ~~ symbolName ~~ "-" ->
  					"TestID -> \"" <> newSymbolName <> "-"
  			}
  		]
	];

(*!
	\function RenameSymbolInNotebook
	
	\calltable
		RenameSymbolInNotebook[notebookFile, oldContext, oldSymbolName, newContext, newSymbolName] '' renames a symbol and/or its context in the given notebook, and move's the notebook file if appropriate.
	
	\maintainer danielb
*)
RenameSymbolInNotebook[notebookFile_, oldContext_, oldSymbolName_, newContext_, newSymbolName_] :=
	Block[{notebookContents, res},
		
		notebookContents = Import[notebookFile, "Text"];
		
		If [notebookContents === $Failed || !StringQ[notebookContents],
			Print["MakeSymbolPrivate: Couldn't read notebook file. Aborting."];
			Print["  File: ", notebookContents];
			Return[$Failed];
		];
		
	  	notebookContents =
			StringReplaceUsesOfSymbol[
				notebookContents,
				oldContext,
				oldSymbolName,
				newContext,
				newSymbolName
			];
			
		(* We'll also be more aggressive and now do a full/simple replace. *)
		notebookContents =
			StringReplace[
				notebookContents,
	  			{
	  				(* Fully qualified uses of symbol (ie. with context) *)
	  				WordBoundary ~~ (oldContext <> oldSymbolName | oldContext <> "PackageScope`" <> oldSymbolName) -> newContext <> newSymbolName,
	  				(* Function calls should use the full context, since the symbol is now private. *)
	  				WordBoundary ~~ oldSymbolName <> "[" -> newContext <> newSymbolName <> "[",
	  				(* RowBox *)
	  				"\"" <> oldSymbolName <> "\", \"[" -> "\"" <> newContext <> newSymbolName <> "\", \"[",
	  				"\"" <> oldSymbolName <> "\", \"]" -> "\"" <> newContext <> newSymbolName <> "\", \"]", 
	  				(* For the notebook's title, etc. *)
					WordBoundary ~~ oldSymbolName -> newSymbolName
	  			}
				
			];
		  
		(* Rename the file according to its new name, and write out the modified contents. *)
		With[{newFile = FileNameJoin[{DirectoryName[notebookFile] <> newSymbolName <> ".nb"}]},
			If [newFile === notebookFile,
				res = Export[newFile, notebookContents, "Text"];
				,
				If [res = Export[newFile, notebookContents, "Text"] =!= $Failed,
					DeleteFile[notebookFile];
				];
				
				If [oldSymbolName =!= newSymbolName,
					With[{grandparentDirectory = ParentDirectory[DirectoryName[notebookFile]]},
						With[{newDir = FileNameJoin[{grandparentDirectory <> newSymbolName}]},
							RenameDirectory[
								DirectoryName[notebookFile],
								FileNameJoin[{grandparentDirectory, newDir}]
							]
						]
					];
				];
			];
		];
		
		res
	];

(*!
	\function ComposeEmail
	
	\calltable
		ComposeEmail[] '' compose an email.
	
	Example:
	
	ComposeEmail["To" -> "Jeremy"]
	
	\maintainer danielb
*)
Options[ComposeEmail] =
{
	"To" -> None,			(*< to who? *)
	"Invoke" -> Automatic,	(*< how should the compose be triggered? SystemOpen? Gmail? *)
	"Subject" -> Null		(*< the email subject. *)
};
ComposeEmail[opts:OptionsPattern[]] :=
	Module[{to = OptionValue["To"], tmp, invoke = OptionValue["Invoke"]},
		
		
		Which[
			StringQ[to] && StringFreeQ[to, "@"],
			(* Lookup the person's email address if known. *)
			tmp = GetPerson[to, "EmailAddress"];
			If [StringQ[tmp],
				to = tmp;
			];
			,
			MatchQ[to, _Person],
			to = Gett[to, "Email"];
		];
		
		If [invoke === Automatic,
			If [!StringFreeQ[to, "@gmail"],
				invoke = "Gmail";
			];
		];
		
		Switch[invoke,
			Automatic | SystemOpen | Missing[] | Null,
			SystemOpen[
				"mailto:" <> to <>
				If [OptionValue["Subject"] =!= Null,
					"?subject=" <> OptionValue["Subject"]
					,
					""
				]
			];
			,
			"Gmail",
			ComposeEmailViaGmail["To" -> to, "Subject" -> OptionValue["Subject"]];
			,
			_,
			Print["ComposeEmail: Unknown Invoke: ", OptionValue["Invoke"]];
		];
			
	];

(*!
	\function ComposeEmailViaGmail
	
	\calltable
		ComposeEmailViaGmail[] '' compose an email via the Gmail interface.
	
	Example:
	
	ComposeEmail["To" -> "address@domain.com"]
	
	\maintainer danielb
*)
Options[ComposeEmailViaGmail] =
{
	"To" -> None,			(*< to who? *)
	"Subject" -> Null,		(*< email subject *)
	"Body" -> Null			(*< email body *)
};
ComposeEmailViaGmail[opts:OptionsPattern[]] :=
	Module[{url},
		
		url = "https://mail.google.com/mail/?view=cm&fs=1";
		
		If [OptionValue["To"] =!= None,
			url = url <> "&to=" <> UrlEncode[OptionValue["To"]];
		];
		
		If [OptionValue["Subject"] =!= Null,
			url = url <> "&su=" <> UrlEncode[OptionValue["Subject"]];
		];
		
		If [OptionValue["Body"] =!= Null,
			url = url <> "&body=" <> UrlEncode[OptionValue["Body"]];
		];
		
		SystemOpen[url]
	];

(*!
	\function UrlEncode
	
	\calltable
		UrlEncode[str] '' url encode a string.
	
	Example:
	
	UrlEncode["just testing"] === "just+testing"
	
	\maintainer danielb
*)
UrlEncode[str_] :=
	(
	LoadJavaClass["java.net.URLEncoder"];
	URLEncoder`encode[str]
	)

(*!
	\function FileSearchUI
	
	\calltable
		FileSearchUI[searchStr] '' search for the given string and display buttons to jump into matching files. (can specify directory, etc.)
	
	\maintainer danielb
*)
Clear[FileSearchUI];
Options[FileSearchUI] =
{
	"Dir" -> Automatic,		 (*< the directories to search. *)
	"Files" -> Automatic,	   (*< the files to search. *)
	"FeelingLucky" -> False	 (*< open the first result? *)
};
FileSearchUI[searchStr_, OptionsPattern[]] :=
	Module[{files},
		
		files =
			FilesContaining[
				searchStr,
				"Dir" -> OptionValue["Dir"],
				"Files" ->
					If [OptionValue["Files"] =!= Automatic,
						Flatten[{OptionValue["Files"]}]
						,
						Automatic
					],
				"FilePattern" -> {"*.m", "*.mt", "*.nb", "*.java", "*.txt", "*.cpp", "*.h", "*.hpp"}
			];
		
		If [TrueQ[OptionValue["FeelingLucky"]],
			OpenFileInWorkbench[files[[1]], "Substring" -> searchStr]
			,
			displayFileList[files, "Substring" -> searchStr]
		]
	]

(* End of CodeToIndentedString functions *)

(*!
	\function FilesContaining
	
	\calltable
		FilesContaining[str] '' returns the list of files containing the given string.
		FilesContaining[strList] '' returns the list of files containing one of the given strings.
	
	Examples:
	
	FilesContaining["Indent2[", "FilePattern" -> "*.m", "Dir" -> "C:\\Temp"]
	
	\related 'FindList 'FileNames
	
	\maintainer danielb
*)
Clear[FilesContaining];
Options[FilesContaining] =
{
	"FilePattern" -> "*.*",					(*< the types of files to search. *)
	"Dir" -> $UserDocumentsDirectory,		(*< the directory/directories to search. Searches all subdirectories. Can be a string or a list of strings. If not specified, searches the user's documents directory. *)
	"Files" -> Automatic					(*< if specified, then "Dirs" is ignored and the explicit list of files is used. *)
};
FilesContaining[str_, OptionsPattern[]] :=
	Module[{dir = OptionValue["Dir"]},
		
		Select[
			If [OptionValue["Files"] === Automatic,
				FileNames[OptionValue["FilePattern"], dir, Infinity]
				,
				OptionValue["Files"]
			],
			FindList[#, str, 1] =!= {} &
		]
	]

(*!
	\function displayFileList
	
	\calltable
		displayFileList[files] '' displays a list of files as buttons. Clicking a button opens the file in Workbench.
	
	\maintainer danielb
*)
Clear[displayFileList];
Options[displayFileList] =
{
	"Substring" -> None	 (*< the substring to navigate to. *)
};
displayFileList[files_, OptionsPattern[]] :=
	Module[{extraEntry = Sequence @@ {}},
		
		(* Don't display more than 100 files. *)
		If [Length[files] > 100,
			files = files[[1;;100]];
			extraEntry = "...";
		];
		
		If [Length[files] > 0,
			SpacedRow[
				Join[
					Function[{file},
						SmartButton[
							FileNameTake[file, -1],
							If [FileExtension[file] === "nb",
								OpenNotebook[file]
								,
								OpenFileInWorkbench[file, "Substring" -> OptionValue["Substring"]]
							]
						]
					] /@ files
					,
					{extraEntry}
				]
			]
			,
			None
		]
	]

(*!
	\function SpacedRow
	
	\calltable
		SpacedRow[rows] '' like Row, but adds some space between the cells.
	
	Examples:
	
	SpacedRow[{"A", "B", "C"}]
	
	\related 'Row
	
	\maintainer danielb
*)
SpacedRow[rows_] := Row[Riffle[rows, " "]]

(*!
	\function WithTemporaryFiles
	
	\calltable
		WithTemporaryFiles[{x = x0, y = y0, ...}, expr] '' for each symbol assignment, writes the assignment value to a new temporary file and assigns the temporary file name to the given symbol, then acting like 'With', replaces any uses of the given symbol in the inner expression with the file path. Once the inner expression has finished evaluating, the temporary files are cleaned up. Useful when unit testing functions that act on files.

	Example:
	
	WithTemporaryFiles[
		{a = "1"},
		Wrapper[a]
	]
	
	===
	
	Wrapper["E:\\Users\\Daniel\\AppData\\Local\\Temp\\m-bcec1816-bd2b-4cbb-9dc6-99c1583b4587"]
	
	Example:
	
	WithTemporaryFiles[
		{a = "1"},
		Get[a]
	]
	
	===
	
	"1"

	Unit tests:

	RunUnitTests[WUtils`WUtils`WithTemporaryFiles]

	\maintainer danielb
*)
Clear[WithTemporaryFiles];
Attributes[WithTemporaryFiles] = {HoldAllComplete};
Clear[WithTemporaryFiles];
Options[WithTemporaryFiles] =
{
	"Directory" -> Automatic				(*< The directory in which to create the temporary files. If Automatic, then $TemporaryDirectory is used. *)
};
WithTemporaryFiles[assignmentsIn_List, expr_, OptionsPattern[]] :=
	Module[{assignments, heldExpr = HoldComplete[expr], temporaryHoldComplete,
			temporaryFiles = {}, symbolToTemporaryFileMapping, dir},
			
		dir = OptionValue["Directory"];
		If [dir === Automatic,
			dir = $TemporaryDirectory;
		];
		
		Attributes[temporaryHoldComplete] = {HoldAllComplete};
		
		If [!MatchQ[HoldComplete[assignmentsIn], HoldComplete[{Repeated[Set[_Symbol, _]]}]],
			Print["WithTemporaryFiles: Invalid assignments: ", Indent2[HoldComplete[assignmentsIn], "RemoveHold" -> True]];
			$Failed
			,
			assignments = HeldListToListOfHeld[HoldComplete[assignmentsIn]];

			(* Create a mapping from With symbols to temporary files, and write
			   the desired expression into the temporary files. *)
			symbolToTemporaryFileMapping =
				Function[{assignment},
					With[{newTemporaryFile = TemporaryFile["Directory" -> dir]},

						(* Create the list this way since Append/AppendTo are slow.
						   Not that it matters much for small lists like this. *)
						temporaryFiles = {temporaryFiles, newTemporaryFile};
					   
						assignment /. HoldComplete[Set[symbol_, value_]] :>
							(
							Export[newTemporaryFile, value, "Text"];
							HoldComplete[symbol] -> newTemporaryFile
							)
					]
			   ] /@ assignments;
			   
		   (* Replace instances of the 'With' symbols with their corresponding
			  temporary file. *)
		   heldExpr = ReplaceHeldExpressions[heldExpr, symbolToTemporaryFileMapping, _Symbol];
		   
		   (* Evaluate the expression, capture the result, cleanup the temporary
			  files, and then returned the evaluated expression. *)
		   With[{res = ReleaseHold[heldExpr]},
			   DeleteFile /@ Flatten[temporaryFiles];
			   res
		   ]
		]
	]

(*!
	\function TemporaryFile
	
	\calltable
		TemporaryFile[] '' returns the name of a new temporary file created in the $TemporaryDirectory.
	
	Examples:
	
	TemporaryFile[] === "E:\\Users\\Daniel\\AppData\\Local\\Temp\\m-1b608483-cdec-4a13-9fbd-0f07a03c856d"
	
	\related 'TemporaryDirectory
	
	\maintainer danielb
*)
Clear[TemporaryFile];
Options[TemporaryFile] =
{
	"Extension" -> None,					(*< The file extension to use. *)
	"Directory" -> Automatic				(*< The directory in which to create the temporary files. If Automatic, then $TemporaryDirectory is used. *)
};
TemporaryFile[OptionsPattern[]] :=
	Module[{
			tempFile,
			ext =
				If [OptionValue["Extension"] =!= None,
					If [!StringTake[OptionValue["Extension"], 1] === ".",
						".",
						""
					] <> OptionValue["Extension"]
					,
					""
				]
		   },
		
		tempFile = Close[OpenWrite[]];
		
		If [OptionValue["Directory"] =!= Automatic && OptionValue["Directory"] =!= $TemporaryDirectory,
			With[{path = FileNameJoin[{OptionValue["Directory"], FileNameTake[tempFile, -1] <> ext}]},
				RenameFile[
					tempFile,
					path
				];
				path
			]
			,
			If [ext =!= "",
			   With[{path = tempFile <> ext},
				   RenameFile[tempFile, path];
				   path
			   ]
			   ,
			   tempFile
			]
		]
	]

(*!
	\function ReplaceHeldExpressions
	
	\calltable
		ReplaceHeldExpressions[expr, replacementRules, pattern] '' like Replace, this function replaces occurrences in expr of the given replacement rules. However, the left-hand-sides of the replacement rules are to be wrapped in HoldComplete, so that they can be things that would otherwise evaluate. The sub-expressions to consider replacing are specified via a third argument, which specifies a pattern.

	Examples:
	
	ReplaceHeldExpressions[
		HoldComplete[1 + 1, 2 + 2, 3 + 3],
		{HoldComplete[1 + 1] -> "replaced"},
		_Plus
	]

	===

	HoldComplete[HoldComplete["replaced", 2 + 2, 3 + 3]]

	Unit tests:

	RunUnitTests[WUtils`WUtils`ReplaceHeldExpressions]

	\maintainer danielb
*) 
Clear[ReplaceHeldExpressions];
ReplaceHeldExpressions[expr_, replacementRules_List, pattern_] :=
	Module[{heldExpr = HoldComplete[expr], temporaryHoldComplete},
		
		Attributes[temporaryHoldComplete] = {HoldAllComplete};

		(* Process replacement rules one by one. *)
		Function[{replacementRule},
			   
				replacementRule /. (Rule | RuleDelayed)[HoldComplete[lhs_], rhs_] :>
					(
					(* Make the replacements for the current replacement rule. *)
					heldExpr =
						Replace[
							heldExpr,
							binding:pattern :>
								(
								With[{replaceRes =
										(
										If [HoldComplete[binding] === HoldComplete[lhs],
											rhs
											,
											(* Whoops, this didn't match the current replacement
											   rule. Put it back, wrapped in something that
											   will prevent it from evaluating, and we'll
											   remove the temporary wrapper below. *)
											temporaryHoldComplete[binding]
										]
										)
									 },
									 
									 replaceRes /; True
								]
								),
							Infinity
						];
					)
					
		   ] /@ replacementRules;
		   
		(* Temove any temporary hold wrappers that we added. *)
		ReleaseHold[
			Replace[heldExpr, temporaryHoldComplete[inner_] :> inner, Infinity]
		]
	]

(*!
	\function TemporaryFilesBlock
	
	\calltable
		TemporaryFilesBlock[dir, files, expr] '' given a directory and a list of rules with LHS specifying a file name and RHS specifying file contents, creates the given files, evaluates the specified expression, and then cleans up the temporary files.
		TemporaryFilesBlock[files, expr] '' given a list of rules with LHS specifying a file name and RHS specifying file contents, creates the given files in a new temporary directory, evaluates the specified expression, and then cleans up the temporary files and temporary directory. Replace any instances of "$TemporaryDirectory$" in the expression with the temporary directory's path.

	Examples:
	
	TemporaryFilesBlock[
		$TemporaryDirectory,
		{"MyFile1.m" -> "a", "MyFile2.m" -> "b"},
		{
			Get[FileNameJoin[{$TemporaryDirectory, "MyFile1.m"}]],
			Get[FileNameJoin[{$TemporaryDirectory, "MyFile2.m"}]]
		}
	]

	===

	{"a", "b"}

	Unit tests:

	RunUnitTests[WUtils`WUtils`TemporaryFilesBlock]

	\maintainer danielb
*)
Clear[TemporaryFilesBlock];
Attributes[TemporaryFilesBlock] = {HoldRest};
TemporaryFilesBlock[dir_String, files_List, expr_] :=
	Module[{fileName, fileContents, filePath, filePaths = {}},
		
		If [!FileExistsQ[dir],
			CreateDirectory[dir, CreateIntermediateDirectories -> True];
		];
		
		If [!DirectoryQ[dir],
			Print["TemporaryFilesBlock: The specified directory isn't a directory: ", InputForm[dir]];
			Return[$Failed];
		];
		
		Function[{file},
			
			{fileName, fileContents} = List @@ file;
			
			filePath = FileNameJoin[{dir, fileName}];
			
			filePaths = {filePaths, filePath};
			
			Export[filePath, fileContents, "Text"];
			
		] /@ files;
		
		With[{res = expr},
			DeleteFile /@ Flatten[filePaths];
			
			res
		]
	]

TemporaryFilesBlock[files_List, expr_, tempDirName_String:Null] :=
	Module[{expr2 = HoldComplete[expr]},
		With[{tempDir = TemporaryDirectory[tempDirName]},
			
			expr2 = Replace[expr2, "$TemporaryDirectory$" :> tempDir, Infinity];
		
			expr2 /. HoldComplete[inner_] :>
				With[{res = TemporaryFilesBlock[tempDir, files, inner]},
					DeleteDirectory[tempDir];
					res
				]
		]
	]

(*!
	\function TemporaryDirectory
	
	\calltable
		TemporaryDirectory[] '' returns the path of a new temporary directory created in $TemporaryDirectory.
		TemporaryDirectory[name] '' returns the path of a new temporary directory created in $TemporaryDirectory with directory name 'name'.
	
	\related 'TemporaryFile
	
	\maintainer danielb
*)
TemporaryDirectory[dirName_:Null] :=
	Module[{path = TemporaryFile[]},
		If [dirName === Null,
			DeleteFile[path];
			CreateDirectory[path];
			path
			,
			DeleteFile[path];
			path = FileNameJoin[{path, dirName}];
			CreateDirectory[path];
			path
		]
	]

(*!
	\function CapturePrint
	
	\calltable
		CapturePrint[e] '' captures and returns Print output when the given expression is evaluated. If no output is received, then Null is returned. The output is a list of items. Each item is itself a list, giving the individual args of the Print.
		
	This can be useful when writing unit tests for things that are expected to produce printed output which should be checked.

	Examples:
	
	CapturePrint[
		Print["a", "b", "c"];
		Print["d", "e", "f"];
	]
	
	===
	
	{{"a", "b", "c"}, {"d", "e", "f"}}

	Unit tests:

	RunUnitTests[WUtils`WUtils`CapturePrint]

	\maintainer danielb
*)
Attributes[CapturePrint] = {HoldAllComplete};
CapturePrint[e_] :=
	Module[{res, downValueSymbol},
		
		With[{res = res},
			downValueSymbol[args___] :=
				If [res === Null,
					res = {{args}};
					,
					AppendTo[res, {args}];
				];
		];
		
		res = Null;
		
		Block[{Print = downValueSymbol},
			e
		];
		
		res
	]

(*!
	\function StringToSymbol
	
	\calltable
		StringToSymbol[name] '' given a string, returns the corresponding symbol, or $Failed if no symbol of that name has been defined. Supports both exported and private symbols, so long as their package is on the $ContextPath.
	
	Examples:
	
	StringToSymbol["CreateIssueNotebook"] === WUtils`WUtils`CreateIssueNotebook
	
	StringToSymbol["toSingleLine"] === WUtils`WUtils``Private`toSingleLine

	Unit tests:

	RunUnitTests[StringToSymbol]

	\maintainer danielb
*)
StringToSymbol[name_] :=
	Module[{context},
		If [CouldBeWLSymbolQ[name],
			context = GetSymbolContext[name];
			If [!MatchQ[GetSymbolContext[name], None],
				ToExpression[context <> name]
				,
				$Failed
			]
			,
			$Failed
		]
	]

(* Marker to the test system that we just care about the head of the result. *)
TestHead[e_] := e

(*!
	\function EditFile
	
	\calltable
		EditFile[file] '' tries to find and then edit the given file.

	Examples:
	
	EditFile[file] === TODO
	
	\related '
	
	\maintainer danielb
*)
Clear[EditFile];
Options[EditFile] =
{
	"Substring" -> None			(*< The substring to go to within the file. *)
};
EditFile[file_, OptionsPattern[]] :=
	Block[{path},
		path = ResolveFile[file];
		If [!FailureQ[path],
			OpenFileInWorkbench[path, "Substring" -> OptionValue["Substring"]]
			,
			"Couldn't find file: " <> file
		]
	];

(*!
	\function ResolveFile
	
	\calltable
		ResolveFile[file] '' tries to find the given file. Consults $SearchDirectories.

	Examples:
	
	ResolveFile[file] === TODO
	
	\related '
	
	\maintainer danielb
*)
ResolveFile[file_] :=
	Block[{files, search},
		
		If [FileExistsQ[file], Return[file]];
		
		files = FileNames[file, Global`$SearchDirectories, Infinity];
		files = Select[files, !DirectoryQ[#] &];
		
		If [files === {} && StringFreeQ[file, "."],
			files = FileNames[file <> ".*", Global`$SearchDirectories, Infinity];
			files = Select[files, !DirectoryQ[#] &];
		];
		
		If [files === {},
			$Failed
			,
			First[files]
		]
	];

(*!
	\function SetEvaluationTarget
	
	\calltable
		SetEvaluationTarget[] '' sets the currently selected cell as the cell to be evaluated when the user presses the hotkey that has been designated to rerun the cell of interest.
	
	\related 'EvaluateEvaluationTarget
	
	\maintainer danielb
*)
If [!ValueQ[$evaluationTarget], $evaluationTarget = Null];
SetEvaluationTarget[] :=
	Block[{},
		With[{selectedCells = SelectedCells[SelectedNotebook[]]},
			If [MatchQ[selectedCells, {__CellObject}],
				$evaluationTarget = First[selectedCells];
				,
				Beep[]
			]
		]
	];

(*!
	\function EvaluateEvaluationTarget
	
	\calltable
		EvaluateEvaluationTarget[] '' re-evaluate the cell that has been designated as the evaluation target.
	
	\related 'SetEvaluationTarget
	
	\maintainer danielb
*)
EvaluateEvaluationTarget[] :=
	Block[{},
		If [MatchQ[$evaluationTarget, _CellObject],
			If [FailureQ[SelectionMove[$evaluationTarget, All, Cell]],
				Beep[]
				,
				SelectionEvaluate[SelectedNotebook[]];
			]
		]
	];

(*!
	\function DeleteCurrentNotebook
	
	\calltable
		DeleteCurrentNotebook[] '' makes a backup copy of the current notebook and then delete it.
	
	\related '
	
	\maintainer danielb
*)
DeleteCurrentNotebook[] :=
	Block[{nb, file},
		nb = SelectedNotebook[];
		If [!MatchQ[nb, _NotebookObject], Return[$Failed]];
		file = NotebookFileName[SelectedNotebook[]];
		(* Make a backup copy. *)
		CopyFile[
			file,
			FileNameJoin[
				{
					$TemporaryDirectory,
					FileNameTake[file, -1] <> ".bak"
				}
			]
		];
		NotebookClose[nb];
		DeleteFile[file];
		If [FileNameTake[file, {-2}] == FileBaseName[FileNameTake[file, -1]],
			(* Delete the directory as well. *)
			DeleteDirectory[FileNameDrop[file, -1]];
		];
	];

(*!
	\function CreateProject
	
	\calltable
		CreateProject[sourceCodeDirectoryInfo, name] '' creates a new directory under the base directory with the given name, and configures it as a package/project.

	Examples:
    
	CreateProject[
	    {
	        "Directory" -> HoldComplete[FileNameJoin[{$PersonalCvsDir, "Daniel"}]],
	        "ParentPackage" -> "Daniel`",
	        "Linguistic" -> "db" | "personal"
	    },
	    "Entities"
	]
	
	\related '
	
	\maintainer danielb
*)
CreateProject[sourceCodeDirectoryInfo_, name_] :=
	Block[{parentDir, dir, kernelDir, initFile, parentPackage, package, mainFile, testsDir, projectFile},
		
		parentDir = ReleaseHold[Lookup[sourceCodeDirectoryInfo, "Directory"]];
		If [!StringQ[parentDir] || !DirectoryQ[parentDir], Print["Invalid directory: ", parentDir]; Return[$Failed]];
		
		dir = FileNameJoin[{parentDir, name}];
		
		If [FileExistsQ[dir],
			Print["ERROR: CreateProject: Directory already exists. Aborting."];
			Return[$Failed];
		];
		
		If [!FileExistsQ[dir], CreateDirectory[dir]];
		If [!DirectoryQ[dir], Print["Couldn't create project directory: ", dir]; Return[$Failed]];
		
		kernelDir = FileNameJoin[{dir, "kernel"}];
		If [!FileExistsQ[kernelDir], CreateDirectory[kernelDir]];
		
		parentPackage = Lookup[sourceCodeDirectoryInfo, "ParentPackage"];
		package = parentPackage <> name <> "`";
		
		initFile = FileNameJoin[{kernelDir, "init.m"}];
		Export[initFile, "Get[" <> ToString[package <> name <> "`", InputForm] <> "];\n\n", "Text"];
		
		testsDir = FileNameJoin[{dir, "Tests"}];
		If [!FileExistsQ[testsDir], CreateDirectory[testsDir]];
		
		mainFile = FileNameJoin[{dir, name <> ".m"}];
		Export[
			mainFile,
			"BeginPackage[" <> ToString[package <> name <> "`", InputForm] <> "]

Needs[\"WUtils`WUtils`\"]; (* CreateReloadFunctionForDirectory, etc. *)

Reload" <> name <> "::usage = \"Reload" <> name <> "  \"; 

Begin[\"`Private`\"]

With[{package = " <> ToString[package, InputForm] <> "},
With[{dir = DirectoryName[DirectoryName[FindFile[package]]]},
	" <> package <> "Private`$ReloadFunction = Reload" <> name <> ";
	WUtils`WUtils`TabsOrSpaces[package] = \"Tabs\";
	If [!ValueQ[$reload" <> name <> "],
		$reload" <> name <> " =
			CreateReloadFunctionForDirectory[
				DirectoryName[DirectoryName[FindFile[package]]]
			];
	];
	" <> package <> "$UnitTestDir = FileNameJoin[{DirectoryName[DirectoryName[FindFile[package]]], \"Tests\"}];
	WUtils`WUtils`NotebookTypeToDirectory[package] = FileNameJoin[{dir, \"Notebooks\"}];
	If [!MemberQ[Global`$WorkbenchProjects, dir],
		AppendTo[Global`$WorkbenchProjects, dir];
	];
];
];

(* Reloads .m files in this directory if they've changed. *)
Reload" <> name <> "[] := $reload" <> name <> "[]
If [ListQ[Global`$ReloadFunctions],
	Global`$ReloadFunctions =
		DeleteDuplicates[
			Append[Global`$ReloadFunctions, Reload" <> name <> "]
		]
	];

End[]

EndPackage[]
",
			"Text"
		];
		
		Get[initFile];
		
		projectFile = FileNameJoin[{dir, ".project"}];
		Export[
			projectFile,
			"<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<projectDescription>
	<name>" <> name <> "</name>
	<comment></comment>
	<projects>
	</projects>
	<buildSpec>
	</buildSpec>
	<natures>
	</natures>
</projectDescription>",
			"Text"
		];
		
		(* Tries to open the file in Workbench, but fails because the project doesn't exist *)
		(*OpenFileInWorkbench[mainFile, "Substring" -> "\nEnd[]"];*)
		
		Lui`Actions`DefineLinguistic[
			"Symbol" -> "sourceCodeDirectory",
			"Linguistic" -> ToLowerCase[DeCamelCase[name]],
			"Expression" ->
				<|
					"Directory" -> dir,
					"Package" -> package,
					"Linguistic" -> "TODO"
				|>
		]
	];

(*!
	\function CreateSourceFile
	
	\calltable
		CreateSourceFile[name, parentInfo] '' creates a new .m file.

	Examples:
	
	CreateSourceFile[name, dir] === TODO
	
	\related '
	
	\maintainer danielb
*)
CreateSourceFile::fae = "File already exists: `1`"
CreateSourceFile::dde = "Directory doesn't exist: `1`"
CreateSourceFile[name_, parentInfo_] :=
	Block[{file, package},
		If [!FileExistsQ[parentInfo["Directory"]],
			MessageFail[CreateSourceFile::dde, parentInfo["Directory"]];
		];
		file = FileNameJoin[{parentInfo["Directory"], name <> ".m"}];
		If [FileExistsQ[file],
			MessageFail[CreateSourceFile::fae, file];
		];
		
		package = parentInfo["Package"] <> name <> "`";
		
		Export[
			file,
			"BeginPackage[\"" <> package <> "\"]

Needs[\"WUtils`WUtils`\"];

Begin[\"`Private`\"]

End[]

EndPackage[]",
			"Text"
		];
		
		OpenFileInWorkbench[file, "Substring" -> "\nEnd[]"];
		
		Lui`Actions`DefineLinguistic[
			"Symbol" -> "sourceFile",
			"Linguistic" -> ToLowerCase[DeCamelCase[name]],
			"Expression" -> package
		]
	];

(*!
	\function MessageFail
	
	\calltable
		MessageFail[args] '' like the Message function but also does Return[$Failed, Block].

	Examples:
	
	Block[{}, Blah::abc = "Just testing: `1`"; MessageFail[Blah::abc, "123"]] === $Failed

	Unit tests:

	RunUnitTests[WUtils`WUtils`MessageFail]

	\maintainer danielb
*)
Clear[MessageFail];
Attributes[MessageFail] = {HoldFirst};
MessageFail[name_, args___] :=
	(
		Message[name, args];
		Return[$Failed, Block];
	)

(*!
	\function CreateDirectoryIfDoesntExist
	
	\calltable
		CreateDirectoryIfDoesntExist[dir] '' creates the directory if it doesn't already exist.
	
	Returns True if the directory was created, Null if it already exists, $Failed upon failure.

	Examples:
	
	Block[
		{res, dir},
		(
			dir = TemporaryDirectory[];
			res = {CreateDirectoryIfDoesntExist[dir], FileExistsQ[dir]};
			DeleteDirectory[dir];
			res
		)
	]

	===

	{Null, True}

	Unit tests:

	RunUnitTests[WUtils`WUtils`CreateDirectoryIfDoesntExist]

	\maintainer danielb
*)
CreateDirectoryIfDoesntExist::exnd = "The file `1` already exists and is not a directory.";
CreateDirectoryIfDoesntExist[dir_] :=
	Block[{},
		If [!FileExistsQ[dir],
			With[{res = CreateDirectory[dir, CreateIntermediateDirectories -> True]},
				If[!StringQ[res] || !FileExistsQ[res],
					$Failed
					,
					True
				]
			]
			,
			If [!DirectoryQ[dir],
				MessageFail[CreateDirectoryIfDoesntExist::exnd, dir]
			]
		]
	];

(*!
	\function WriteFileIndented
	
	\calltable
		WriteFileIndented[expr, file] '' write the given expression to the given file, indenting the expression.

	Examples:
	
	WithTemporaryFiles[
		{file = ""},
		(
			WriteFileIndented[Table[i, {i, 1, 80}], file];
			Import[file, "Text"]
		)
	]

	===

	{
		1,
		2,
		...
	}

	Unit tests:

	RunUnitTests[WUtils`WUtils`WriteFileIndented]

	\maintainer danielb
*)
WriteFileIndented[expr_, file_] :=
	Block[{},
		Export[
			file,
			Indent2[expr],
			"Text"
		]
	];

(*!
	\function EnsureFileLoaded
	
	\calltable
		EnsureFileLoaded[fileAssoc] '' given an association that provides a file's path, a held variable to store its contents when loaded, etc, ensures the file has been loaded.

	Examples:
	
	WithTemporaryFiles[
		{file = "{1, 2, 3}"},
		(
			EnsureFileLoaded[
				Association["Path" -> file, "Variable" -> HoldComplete[var]]
			];
			var
		)
	]

	===

	{1, 2, 3}

	Unit tests:

	RunUnitTests[WUtils`WUtils`EnsureFileLoaded]
	
	\related 'LoadFile

	\maintainer danielb
*)
EnsureFileLoaded[fileAssoc_] :=
	Block[{res},
		fileAssoc["Variable"] /. HoldComplete[var_] :>
			If [!ValueQ[var] || FailureQ[var],
				res = var = Get[fileAssoc["Path"]]
				,
				res = var
			];
			
		res
	];

(*!
	\function LoadFile
	
	\calltable
		LoadFile[fileAssoc] '' given an association that provides a file's path, a held variable to store its contents when loaded, etc, load the file.

	Examples:
	
	WithTemporaryFiles[
		{file = "{1, 2, 3}"},
		(
			LoadFile[Association["Path" -> file, "Variable" -> HoldComplete[var]]];
			var
		)
	]

	===

	{1, 2, 3}

	Unit tests:

	RunUnitTests[WUtils`WUtils`LoadFile]

	\related 'EnsureFileLoaded
	
	\maintainer danielb
*)
LoadFile[fileAssoc_] :=
	Block[{res},
		fileAssoc["Variable"] /. HoldComplete[var_] :>
			(
			res = var = Get[fileAssoc["Path"]];
			);
			
		res
	];

(*!
	\function Second
	
	\calltable
		Second[list] '' returns the second item in a list.

	Examples:
	
	Second[{1, 2, 3}] === 2

	Unit tests:

	RunUnitTests[WUtils`WUtils`Second]

	\maintainer danielb
*)
Second[list_] := list[[2]]

(*!
	\function WriteFile
	
	\calltable
		WriteFile[fileInfo] '' given an association that specifies the path of the file, the variable holding its contents, and the function to use when writing the file, write the data to disk.

	The "Writer" key/value is optional. If not specified, Put is used.

	Examples:
	
	WithTemporaryFiles[
		{file = ""},
		(
			var = Table[i, {i, 1, 60}];
			WriteFile[
				<|
					"Path" -> file,
					"Variable" -> HoldComplete[var],
					"Writer" -> WriteFileIndented
				|>
			];
			Import[file, "Text"]
		)
	]

	===

	{
		1,
		2,
		...
	}

	Unit tests:

	RunUnitTests[WUtils`WUtils`WriteFile]

	\maintainer danielb
*)
WriteFile[fileInfo_] :=
	Block[{writer},
		writer = fileInfo["Writer"];
		If [MissingQ[writer], writer = Put];
		writer[
			ReleaseHold[fileInfo["Variable"]],
			fileInfo["Path"]
		]
	];

(*!
	\function FocusInputFieldDelayed
	
	\calltable
		FocusInputFieldDelayed[boxId] '' set the focus to the InputField with the given BoxID, but wait a moment before doing so. (ie. the box has yet to actually be created)

	Examples:
	
	With[{boxId = ToString[Unique["MyInputBox"]]},
		FocusInputFieldDelayed[boxId];
		InputField[Dynamic[input], String, BoxID -> boxId]
	]
	
	\maintainer danielb
*)
FocusInputFieldDelayed[boxId_] :=
	StartScheduledTask[
		CreateScheduledTask[
			FrontEnd`MoveCursorToInputField[
				InputNotebook[],
				boxId
			],
			{0.3}
		]
	];

(*!
	\function NotEmpty
	
	\calltable
		NotEmpty[list] '' returns True if the given list isn't empty.

	Examples:
	
	NotEmpty[{}] === False

	Unit tests:

	RunUnitTests[WUtils`WUtils`NotEmpty]

	\maintainer danielb
*)
NotEmpty[list_List] := list =!= {}

(*!
	\function EmptyQ
	
	\calltable
		EmptyQ[list] '' returns True if the given list is empty.

	Examples:
	
	EmptyQ[{}] === True

	Unit tests:

	RunUnitTests[WUtils`WUtils`EmptyQ]

	\maintainer danielb
*)
EmptyQ[list_List] := list === {}

(*!
	\function FirstIndex
	
	\calltable
		FirstIndex[list, func] '' returns the index of the first item in the list for which the function returns True. Otherwise, returns None.

	Examples:
	
	FirstIndex[{1, 2, 3}, EvenQ] === 2

	Unit tests:

	RunUnitTests[WUtils`WUtils`FirstIndex]

	\maintainer danielb
*)
FirstIndex[list_, func_] :=
	Block[{index = 0},
		Function[{item},
			++index;
			If [TrueQ[func[item]],
				Return[index, Block];
			];
		] /@ list;
		None
	];

(*!
	\function ToCamelCase
	
	\calltable
		ToCamelCase[str] '' converts a string to camel case.

	Examples:
	
	ToCamelCase["just testing"] === "Just Testing"

	Unit tests:

	RunUnitTests[WUtils`WUtils`ToCamelCase]

	\maintainer danielb
*)
ToCamelCase[str_] :=
	Block[{},
		StringReplace[
			StringReplace[
				str,
				word:RegularExpression["(?<![A-Za-z])[a-z][a-z]+"] :>
					ToUpperCase[StringTake[word, 1]] <> StringDrop[word, 1]
			],
			WhitespaceCharacter.. :> ""
		]
	];

(*!
	\function CreateInputField
	
	\calltable
		CreateInputField[] '' creates an InputField.
	
	\related '
	
	\maintainer danielb
*)
Clear[CreateInputField];
Options[CreateInputField] =
{
	"EnterFunction" -> None,	(*< The function to call with the input when ENTER is pressed. *)
	"InputReturn" -> None,		(*< A held variable can be passed in as a return for the input var. *)
	"BoxIDReturn" -> None		(*< A held variable can be passed in as a return for the Box ID var. *)
};
CreateInputField[OptionsPattern[]] :=
	DynamicModule[{input},
		
		With[{heldInput = OptionValue["InputReturn"]},
			With[{input = input},
				If [heldInput =!= None, SetHeldVar[heldInput, HoldComplete[input]]];
			];
		];
		
		input = Null;
		
		With[{boxId = ToString[Unique["BoxId"]]},
			With[{heldBoxId = OptionValue["BoxIDReturn"]},
				If [heldBoxId =!= None, SetHeldVar[heldBoxId, boxId]];
			];
			FocusInputFieldDelayed[boxId];
			EventHandler[
				#,
				{
					"ReturnKeyDown" :>
					(
						OptionValue["EnterFunction"][input]
					)
				}
			] & @
			InputField[
				Dynamic[input],
				String,
				ImageSize -> {500, Automatic},
				FrameMargins -> Medium,
				BoxID -> boxId
			]
		]
	]

(*!
	\function SimpleReap
	
	\calltable
		SimpleReap[tag, expr] '' evaluates the given expression, reaping the given tag and returning the matching items.

	Examples:
	
	SimpleReap["Tag", Sow[1, "Tag"]; Sow[2, "Tag"]] === {1, 2}

	Unit tests:

	RunUnitTests[WUtils`WUtils`SimpleReap]

	\maintainer danielb
*)
Clear[SimpleReap];
Attributes[SimpleReap] = {HoldRest};
SimpleReap[tag_, expr_] :=
	Block[{},
		Flatten[
			Reap[
				expr,
				tag
			][[2]],
			1
		]
	];

(*!
	\function ReplaceRange
	
	\calltable
		ReplaceRange[list, start, end, replacementItems] '' replace the given range within the list with the new item(s).
		ReplaceRange[list, start ;; end, replacementItems] '' ...

	Examples:
	
	ReplaceRange[{1, 2, 3, 4, 5, 6}, 2, 3, {888, 999}] === {1, 888, 999, 4, 5, 6}

	Unit tests:

	RunUnitTests[WUtils`WUtils`ReplaceRange]

	\maintainer danielb
*)
ReplaceRange[list_, start_, end_, replacementItems_] :=
	Join[
		list[[1;;start-1]],
		If [ListQ[replacementItems],
			replacementItems
			,
			{replacementItems}
		],
		list[[end+1;;-1]]
	];

ReplaceRange[list_, start_ ;; end_, replacementItems_] :=
	ReplaceRange[list, start, end, replacementItems]

(*!
	\function AddOptions
	
	\calltable
		AddOptions[str] '' given the line of code declaring a function, return the lines of code to replace it with that support options being passed to the function.
	
	\maintainer danielb
*)
AddOptions[strIn_] :=
	Block[{str},
		str = AddOptionsHelper[strIn];
		CopyToClipboard[str]
	];

(*!
	\function AddOptionsHelper
	
	\calltable
		AddOptionsHelper[str] '' helper for AddOptions.

	Example:

	AddOptionsHelper["AddOptions[str_] :="]

	===

	"Options[AddOptions]\n{\n\t\"TODO\" -> TODO\t\t(*< TODO *)\n};\nAddOptions[str_, OptionsPattern[]] :="

	Unit tests:

	RunUnitTests[WUtils`WUtils`AddOptionsHelper]

	\maintainer danielb
*)
AddOptionsHelper[strIn_] :=
	Block[{str = strIn, closingBracketPos, funcName = Null},
		closingBracketPos = LastStringPosition[str, "]"];
		If [closingBracketPos === None, Return[$Failed]];
		
		StringCases[
			str,
			name:(LetterCharacter ~~ (LetterCharacter | DigitCharacter)...) ~~ "[" :>
				(funcName = name)
		];
		If [funcName === Null, Return[$Failed]];
		
		"Options[" <> funcName <> "] =
{
	TODO
};
" <>
		InsertString[str, closingBracketPos, ", OptionsPattern[]"]
	];

(*!
	\function LastStringPosition
	
	\calltable
		LastStringPosition[str, substr] '' returns the last string position matching the given substring.

	Examples:
	
	LastStringPosition["Just testing", "t"] === 9

	Unit tests:

	RunUnitTests[WUtils`WUtils`LastStringPosition]

	\maintainer danielb
*)
LastStringPosition[str_, substr_] :=
	With[{positions = StringPosition[str, substr]},
		If [positions === {},
			None
			,
			Last[positions][[1]]
		]
	]

(*!
	\function InsertString
	
	\calltable
		InsertString[str, pos, substr] '' inserts the given substring at the given position.

	Examples:
	
	InsertString["just testing", 6, "X "] === "just X testing"

	Unit tests:

	RunUnitTests[WUtils`WUtils`InsertString]

	\maintainer danielb
*)
InsertString[str_, pos_, substr_] :=
	Block[{},
		StringTake[str, pos - 1] <> substr <> StringTake[str, {pos, -1}]
	];


(*!
	\function Gettt
	
	\calltable
		Gettt[items, key] '' returns the given key for every tiem in the list.

	Examples:
	
	Gettt[{Association["A" -> 1, "B" -> 2], Association["A" -> 3]}, "A"] === {1, 3}

	Unit tests:

	RunUnitTests[WUtils`WUtils`Gettt]

	\maintainer danielb
*)
Clear[Gettt];
Gettt[items_List, key_String, defaultValue_:Missing[]] :=
	Gett[#, key, defaultValue] & /@ items

End[]

EndPackage[]