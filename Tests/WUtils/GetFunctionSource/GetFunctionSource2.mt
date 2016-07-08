(* Tests for: WUtils`WUtils`GetFunctionSource

   Author: danielb
*)

Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`GetFunctionSource,
        "File" -> FindFile["WUtils`WUtils`"]
    ],
    "(*!\n\t\\function GetFunctionSource\n\t\n\t\\calltable\n\t\tGetFunctionSource[func] '' given a function, returns the source code.\n\t\tGetFunctionSource[func, source] '' given a function and the source code file's contents, returns the source code of the function.\n\t\n\tNOTE: This doesn't currently grab the line that exports the symbol if it's exported.\n\t\n\tKnown issues:\n\t\n\t- This function doesn't work if a down value's value has a top-level\n\t  infix operator. ex. MyFunc[] := \"(\" | \")\"\n\t- That said, it would be easy to fix the case where the infix operator\n\t  occurs on the same line as ':='. (TODO)\n\t- If you have something like Func[1][[1]], then the [[1]] is ignored because\n\t  this function is happy to have found Func[1].\n\t\n\tExamples:\n\t\n\tGetFunctionSource[MyFunc, \"MyFunc[] := Module[{}, 1]\"] === \"MyFunc[] := Module[{}, 1]\"\n\n\tUnit tests: GetFunctionSource.mt\n\n\t\\maintainer danielb\n*)\nClear[GetFunctionSource];\nOptions[GetFunctionSource] =\n{\n\t\"File\" -> Automatic\t\t\t(*< the source code file. *)\n};\nGetFunctionSource[func_Symbol, OptionsPattern[]] :=\n\tBlock[{file = OptionValue[\"File\"], src, name},\n\t\tIf [file === Automatic,\n\t\t\tPrint[\"TODO: File -> Automatic\"];\n\t\t\tReturn[$Failed];\n\t\t];\n\t\t\n\t\tsrc = Import[file, \"Text\"];\n\t\t\n\t\tGetFunctionSource[func, src]\n\t];\n\nGetFunctionSource::cff = \"Couldn't find function `1`\";\nGetFunctionSource[func_Symbol, src_String] :=\n\tBlock[{name, spans, firstBracketCharPos, nextNewlineCharPos, remainder, endPos, cases, bracketType,\n\t\t   closingBracketPos, remainderOfLine, nextNonWhitespaceCharPos, funcName},\n\t\t\n\t\tname = SymbolName[func];\n\t\t\n\t\tfuncName = SymbolName[func];\n\t\t\n\t\tspans = GetFunctionCodeSections[src, funcName];\n\t\t\n\t\tIf [spans === {} || spans === $Failed,\n\t\t\tMessage[GetFunctionSource::cff, func];\n\t\t\tReturn[$Failed];\n\t\t];\n\t\t\n\t\tspans = Join[spans, FindMessageDefinitions[src, funcName]];\n\t\tspans = Join[spans, FindOptionsCodeSections[src, funcName]];\n\t\tspans = Join[spans, FindAttributesCodeSections[src, funcName]];\n\t\tspans = Join[spans, FindMathdocComments[src, funcName]];\n\t\t\n\t\tspans =\n\t\t\tJoin[\n\t\t\t\tspans,\n\t\t\t\tFlatten[DeleteCases[(FindCommentBeforeSpan[src, #1] & ) /@ spans, {}], 1]\n\t\t\t];\n\t\t\n\t\t(* We should keep trailing semi-colons if they're present. *)\n\t\tspans = ExpandSpansToIncludeTrailingSemiColons[src, spans];\n\t\t\n\t\t(* We should delete spans that are nested. If a larger span\n\t\t   contains a smaller span, then get rid of the smaller span.\n\t\t   One case where this is important is if a comment such as a\n\t\t   top-of-function Mathdoc comment contains something that\n\t\t   looks like a down value. *)\n\t\tspans = DeleteNesetedSpans[spans];\n\t\t\n\t\tspans = Sort[spans];\n\t\t\n\t\t(* For now I'm going to create a single span that goes from\n\t\t   the start of the first span to the end of the last span\n\t\t   so that, for example, we don't miss definitions of\n\t\t   global variables that might happen after the top-of-function\n\t\t   Mathdoc comment but before the down value definition.\n\t\t   This is dangerous though because:\n\t\t   1) There might be things in between our spans that we don't\n\t\t\t  want.\n\t\t   2) If a function doesn't have a Mathdoc comment, or if it\n\t\t\t  has necessary globals defined prior to that comment,\n\t\t\t  or below the last down value definition, then those\n\t\t\t  code sections will be missed. (but creating a single span\n\t\t\t  doesn't cause that -- it just doesn't do anything to\n\t\t\t  protect us against that) *)\n\t\tIf [Length[spans] > 0,\n\t\t\tspans = {{spans[[1, 1]], spans[[-1, 2]]}};\n\t\t];\n\t\t\n\t\tStringJoin[Riffle[StringTake[src, #] & /@ spans, \"\\n\"]]\n\t];",
    TestID -> "GetFunctionSource2-20160707-RLRU7T"
]
Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`Private`bracketPattern,
        "File" -> FindFile["WUtils`WUtils`"]
    ]
    ,
    "bracketPattern[\"[\" | \"]\"] := \"[\" | \"]\";\nbracketPattern[\"(\" | \")\"] := \"(\" | \")\";\nbracketPattern[\"{\" | \"}\"] := \"{\" | \"}\";\nbracketPattern[\"(*\" | \"*)\"] := \"(*\" | \"*)\";\nbracketPattern[_] := $Failed;"
    ,
    TestID -> "GetFunctionSource2-20160707-I9UP00"
]

Test[
	WUtils`WUtils`GetFunctionSource[
		WUtils`WUtils`Private`bracketValue,
		"File" -> FindFile["WUtils`WUtils`"]
	]
	,
	"bracketValue[\"[\"] := 1;\nbracketValue[\"]\"] := -1;\nbracketValue[\"(\"] := 1;\nbracketValue[\")\"] := -1;\nbracketValue[\"{\"] := 1;\nbracketValue[\"}\"] := -1;\nbracketValue[\"(*\"] := 1;\nbracketValue[\"*)\"] := -1;\nbracketValue[_] := 0;"
	,
	TestID -> "GetFunctionSource2-20160707-4LRYDE"
]

Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`FindMatchingBracket,
        "File" -> FindFile["WUtils`WUtils`"]
    ],
    "(*!\n\t\\function FindMatchingBracket\n\t\n\t\\calltable\n\t\tFindMatchingBracket[str, firstBracketPos] '' find the matching closing bracket in the string.\n\t\tFindMatchingBracket[str, bracket, startPos, initialCount] '' find the matching closing bracket in the string, starting from the given 'startPos', and starting with the initial count provided.\n\t\n\tNOTE: If the 'bracket' is a double quote, then\n\t\t  FindMatchingBracket[str, bracket, startPos, initialCount] makes the assumption that\n\t\t  the character previous to 'startPos' is the opening double quote.\n\t\n\tExamples:\n\t\n\tFindMatchingBracket[\"Func[1, 2, 3]\", 5] === 13\n\n\tUnit tests: FindMatchingBracket.mt\n\n\t\\maintainer danielb\n*)\nFindMatchingBracket::invb = \"Invalid bracket character: `1`\";\nFindMatchingBracket[str_String, bracket_String, startPos_Integer, initialCount_Integer] :=\n\tBlock[{nextTokenPos, remainder, pattern, counter, eatenPos, stringMatchPos},\n\t\t\n\t\tremainder = StringTake[str, {startPos, -1}];\n\t\t\n\t\tIf [bracket === \"\\\"\",\n\t\t\tstringMatchPos = StringPosition[StringTake[str, {startPos - 1, -1}], doubleQuotedStringPattern[], 1];\n\t\t\tIf [stringMatchPos =!= {} && stringMatchPos[[1, 1]] === 1,\n\t\t\t\tReturn[startPos + stringMatchPos[[1, 2]] - 2];\n\t\t\t\t,\n\t\t\t\tReturn[$Failed];\n\t\t\t];\n\t\t];\n\t\t\n\t\tpattern = bracketPattern[bracket];\n\t\tIf [pattern === $Failed,\n\t\t\tMessage[FindMatchingBracket::invb, bracket];\n\t\t\tReturn[$Failed];\n\t\t];\n\t\tpattern = pattern | doubleQuotedStringPattern[] | commentPattern[];\n\t\teatenPos = startPos - 1;\n\t\tcounter = initialCount;\n\t\tWhile[True,\n\t\t\tnextTokenPos = StringPosition[remainder, pattern, 1];\n\t\t\tIf [nextTokenPos === {},\n\t\t\t\tReturn[$Failed, Block];\n\t\t\t];\n\t\t\tXPrint[nextTokenPos, \": \", StringTake[remainder, nextTokenPos][[1]]];\n\t\t\tcounter += bracketValue[StringTake[remainder, nextTokenPos[[1]]]];\n\t\t\tIf [counter === 0,\n\t\t\t\t(* Found matching closing bracket. *)\n\t\t\t\tReturn[eatenPos + nextTokenPos[[1, 2]], Block];\n\t\t\t];\n\t\t\teatenPos += nextTokenPos[[1, 2]];\n\t\t\tremainder = StringTake[remainder, {nextTokenPos[[1, 2]] + 1, -1}];\n\t\t];\n\t]\n\t\nFindMatchingBracket[str_String, firstBracketPos_Integer] :=\n\tBlock[{bracketChar},\n\t\t\n\t\tbracketChar = StringTake[str, {firstBracketPos}];\n\t\t\n\t\tFindMatchingBracket[str, bracketChar, firstBracketPos + 1, 1]\n\t];",
    TestID -> "GetFunctionSource2-20160707-IAEY5C"
]
Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`Private`doubleQuotedStringPattern,
        "File" -> FindFile["WUtils`WUtils`"]
    ],
    "(*!\n\t\\function doubleQuotedStringPattern\n\t\n\t\\calltable\n\t\tdoubleQuotedStringPattern[] '' a string pattern that can be used to find instances of double quoted strings.\n\n\tExample:\n\n\tStringCases[\"Just \\\"testing\\\"\", doubleQuotedStringPattern[]] === {\"\\\"testing\\\"\"}\n\n\tUnit tests: doubleQuotedStringPattern.mt\n\n\t\\maintainer danielb\n*)\ndoubleQuotedStringPattern[] := (\"\\\"\" ~~ RepeatedNull[\"\\\\\\\\\" | \"\\\\\\\"\" | Except[\"\\\"\"]] ~~ \"\\\"\");",
    TestID -> "GetFunctionSource2-20160707-VIKURN"
]
Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`Private`commentPattern,
        "File" -> FindFile["WUtils`WUtils`"]
    ],
    "(*!\n\t\\function commentPattern\n\t\n\t\\calltable\n\t\tcommentPattern[] '' a string pattern that can be used to find instances of comments.\n\n\tExample:\n\n\tStringCases[\n\t\t\"(* just testing *)\",\n\t\tWUtils`WUtils`Private`commentPattern[]\n\t]\n\n\t===\n\n\t{\"(* just testing *)\"}\n\n\tUnit tests: commentPattern.mt\n\n\t\\maintainer danielb\n*)\ncommentPattern[] := RegularExpression[\"\\\\(\\\\*\\\\s*(.*?)\\\\s*\\\\*\\\\)\"];",
    TestID -> "GetFunctionSource2-20160707-19D0X7"
]
Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`FindFirstBracketNotInComment,
        "File" -> FindFile["WUtils`WUtils`"]
    ]
    ,
    "(*!\n\t\\function FindFirstBracketNotInComment\n\t\n\t\\calltable\n\t\tFindFirstBracketNotInComment[str, startPos] '' find the first instance of a 'bracket', but ensure that it isn't inside of a comment.\n\n\tExamples:\n\t\n\tFindFirstBracketNotInComment[\"Func[]\", 1] === 5\n\n\tUnit tests: FindFirstBracketNotInComment.mt\n\n\t\\maintainer danielb\n*)\nFindFirstBracketNotInComment[str_, startPos_] :=\n\tBlock[{pattern, remainder, eatenPos, counter, nextTokenPos},\n\t\tpattern = commentPattern[] | \"(\" | \"[\" | doubleQuotedStringPattern[];\n\t\tremainder = str;\n\t\teatenPos = 0;\n\t\tcounter = 1;\n\t\tWhile[True,\n\t\t\tnextTokenPos = StringPosition[remainder, pattern, 1];\n\t\t\tIf [nextTokenPos === {},\n\t\t\t\tReturn[None, Block];\n\t\t\t];\n\t\t\tIf [!StringMatchQ[StringTake[remainder, {nextTokenPos[[1, 1]], -1}], StartOfString ~~ (commentPattern[] ~~ ___)],\n\t\t\t\t(* Not a comment. This is what we want. *)\n\t\t\t\tReturn[eatenPos + nextTokenPos[[1, 1]], Block];\n\t\t\t];\n\t\t\teatenPos += nextTokenPos[[1, 2]];\n\t\t\tremainder = StringTake[remainder, {nextTokenPos[[1, 2]] + 1, -1}];\n\t\t];\n\t];"
    ,
    TestID -> "GetFunctionSource2-20160707-OE26JT"
]

Test[
    WUtils`WUtils`GetFunctionSource[
        WUtils`WUtils`FindCodeSections,
        "File" -> FindFile["WUtils`WUtils`"]
    ]
    ,
    "(*!\n\t\\function FindCodeSections\n\t\n\t\\calltable\n\t\tFindCodeSections[str, pattern, endType, initialBracketCount] '' find code sections within a string 'str'. The 'patterns' identify the beginning of a code section while the 'endType' specifies how to look for the end of the code section.\n\n\tExamples:\n\t\n\tFindCodeSections[\"Options[Func] = { ... }\", \"Options[\", \"}\", 0] === {{1, 23}}\n\n\tUnit tests: FindCodeSections.mt\n\n\t\\maintainer danielb\n*)\nFindCodeSections::cfcb = \"Couldn't find closing bracket\";\nFindCodeSections[str_, pattern_, endType_, initialBracketCount_] :=\n\tBlock[{positions, closingBracketPos},\n\t\t\n\t\tpositions = StringPosition[str, pattern];\n\t\t\n\t\tFunction[{position},\n\t\t\tclosingBracketPos = FindMatchingBracket[str, endType, position[[2]] + 1, initialBracketCount];\n\t\t\tIf [closingBracketPos === $Failed,\n\t\t\t\tMessage[FindCodeSections::cfcb];\n\t\t\t\tReturn[$Failed, Block];\n\t\t\t];\n\t\t\t{position[[1]], closingBracketPos}\n\t\t] /@ positions\n\t];"
    ,
    TestID -> "GetFunctionSource2-20160707-DJ4NXK"
]