(* Tests for: WUtils`WUtils`RowBoxesToString

   Author: danielb
*)

Test[
    WUtils`WUtils`RowBoxesToString[
        RowBox[{"(*"," ",RowBox[{"Test"," ",RowBox[{"comment","."}]}]," ","*)"}]
    ]
    ,
    "(* Test comment. *)"
    ,
    TestID -> "RowBoxesToString-20150220-LRWZW7"
]

Test[
    WUtils`WUtils`RowBoxesToString[
        RowBox[{RowBox[{"ReloadVirtualAssistantFiles","[","]"}],";"}]
    ]
    ,
    "ReloadVirtualAssistantFiles[];"
    ,
    TestID -> "RowBoxesToString-20150220-N1ID6V"
]

Test[
    WUtils`WUtils`RowBoxesToString[
        \(myFunc[\(1,\ 2,\ 3\)]\)
    ]
    ,
    "myFunc[1, 2, 3]"
    ,
    TestID -> "RowBoxesToString-20150220-E7YDCM"
]

Test[
    WUtils`WUtils`RowBoxesToString[
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