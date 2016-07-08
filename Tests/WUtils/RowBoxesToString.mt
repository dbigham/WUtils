(* Tests for: CalculateParse`Prototype`VirtualAssistant`Utility`RowBoxesToString

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["CalculateParse/Prototype/VirtualAssistant/Tests/UnitTests/Utility/RowBoxesToString.mt"]
   ]
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
    CalculateParse`Prototype`VirtualAssistant`Utility`RowBoxesToString[
        RowBox[{"(*"," ",RowBox[{"Test"," ",RowBox[{"comment","."}]}]," ","*)"}]
    ]
    ,
    "(* Test comment. *)"
    ,
    TestID -> "RowBoxesToString-20150220-LRWZW7"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RowBoxesToString[
        RowBox[{RowBox[{"ReloadVirtualAssistantFiles","[","]"}],";"}]
    ]
    ,
    "ReloadVirtualAssistantFiles[];"
    ,
    TestID -> "RowBoxesToString-20150220-N1ID6V"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RowBoxesToString[
        \(myFunc[\(1,\ 2,\ 3\)]\)
    ]
    ,
    "myFunc[1, 2, 3]"
    ,
    TestID -> "RowBoxesToString-20150220-E7YDCM"
]

Test[
    CalculateParse`Prototype`VirtualAssistant`Utility`RowBoxesToString[
	    RowBox[
	        {
	            RowBox[{"(*"," ",RowBox[{"Test"," ",RowBox[{"comment","."}]}]," ","*)"}],
	            "\n",
	            RowBox[
	                {
	                RowBox[{RowBox[{"ReloadVirtualAssistantFiles","[","]"}],";"}],
	                "\n",
	                RowBox[
	                    {
	                    "myFunc",
	                    "[",
	                    RowBox[{"1",","," ","2",","," ","3"}],
	                    "]"
	                    }
	                ]
	                }
	            ]
	        }
	    ]
    ]
    ,
    "(* Test comment. *)\nReloadVirtualAssistantFiles[];\nmyFunc[1, 2, 3]"
    ,
    TestID -> "RowBoxesToString-20150220-FWFDGA"
]