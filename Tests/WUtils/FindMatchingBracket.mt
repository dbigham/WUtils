(* Tests for: WUtils`WUtils`FindMatchingBracket

   Author: danielb
*)

Test[
	WUtils`WUtils`FindMatchingBracket["Func[1, 2, 3]", 5]
	,
	13
	,
	TestID -> "FindMatchingBracket-20160706-2U8ZPG"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[Func[1], 2]", 5]
	,
	16
	,
	TestID -> "FindMatchingBracket-20160706-95ENTD"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[Func[Func[1]], 2]", 5]
	,
	22
	,
	TestID -> "FindMatchingBracket-20160706-MBCJTH"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[Func[1], Func[2], 3]", 5]
	,
	25
	,
	TestID -> "FindMatchingBracket-20160706-K0SJ77"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"test\", 1]", 5]
	,
	15
	,
	TestID -> "FindMatchingBracket-20160706-LBKBIQ"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"test[]\", 1]", 5]
	,
	17
	,
	TestID -> "FindMatchingBracket-20160706-4M5COV"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"test]\", 1]", 5]
	,
	16
	,
	TestID -> "FindMatchingBracket-20160706-F57F0R"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"test\\\"[]\\\"\", 1]", 5]
	,
	21
	,
	TestID -> "FindMatchingBracket-20160706-SJEXOT"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"test\\\"]\\\"\", 1]", 5]
	,
	20
	,
	TestID -> "FindMatchingBracket-20160706-8U3076"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[1", 5]
	,
	$Failed
	,
	TestID -> "FindMatchingBracket-20160706-VSFMQY"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[1]; Func[2]", 5]
	,
	7
	,
	TestID -> "FindMatchingBracket-20160706-MAVNN1"
]

Test[
	WUtils`WUtils`FindMatchingBracket["\"abc\"", 1]
	,
	5
	,
	TestID -> "FindMatchingBracket-20160706-G1XB5M"
]

Test[
	WUtils`WUtils`FindMatchingBracket["\"abc\" <> \"def\"", 1]
	,
	5
	,
	TestID -> "FindMatchingBracket-20160706-X25UML"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"def\"]", 6]
	,
	10
	,
	TestID -> "FindMatchingBracket-20160706-4BNKSN"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"def\"]", "\"", 7, 1]
	,
	10
	,
	TestID -> "FindMatchingBracket-20160706-EKRCNL"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Func[\"def\"]", "[", 6, 1]
	,
	11
	,
	TestID -> "FindMatchingBracket-20160706-FBWHMR"
]

Test[
	WUtils`WUtils`FindMatchingBracket["Options[Func] = { ... }", "}", 9, 0]
	,
	23
	,
	TestID -> "FindMatchingBracket-20160706-O63GQE"
]

Test[
	WUtils`WUtils`FindMatchingBracket["(* test *)", "*)", 1, 0]
	,
	10
	,
	TestID -> "FindMatchingBracket-20160706-O63GQ1"
]
