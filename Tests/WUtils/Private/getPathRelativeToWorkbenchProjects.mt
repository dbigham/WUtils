(* Tests for: CalculateParse`GeneralLibrary`Private`CalculateParse`GeneralLibrary`Private`getPathRelativeToWorkbenchProjects

   Author: danielb

   Usage:
   
   << Tests`Utilities`ParserTestingTools`
   ParserTestReport[
       FindFile["Tests/UnitTests/CalculateParse/GeneralLibrary/Private/getPathRelativeToWorkbenchProjects.mt"]
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
    Block[
        {
            $WorkbenchProjects = {"Users\\Daniel\\WolframWorkspaces\\Base2\\Alpha"},
            CalculateScan`CommonSymbols`$AlphaRootDirectory = "E:\\Temp"
        },
        CalculateParse`GeneralLibrary`Private`getPathRelativeToWorkbenchProjects[
            "Users\\Daniel\\WolframWorkspaces\\Base2\\Alpha\\Source\\CalculateParse\\GeneralLibrary.m"
        ]
    ]
    ,
    "/Alpha/Source/CalculateParse/GeneralLibrary.m"
    ,
    TestID -> "getPathRelativeToWorkbenchProjects-20160331-GT0JN6"
]

Test[
    Block[
        {
            $WorkbenchProjects = {"Users\\Daniel\\git\\NGParser"},
            CalculateScan`CommonSymbols`$AlphaRootDirectory = "E:\\Temp"
        },
        CalculateParse`GeneralLibrary`Private`getPathRelativeToWorkbenchProjects[
            "Users\\Daniel\\git\\NGParser\\NGParser\\Utility.m"
        ]
    ]
    ,
    "/NGParser/NGParser/Utility.m"
    ,
    TestID -> "getPathRelativeToWorkbenchProjects-20160331-OLNWPZ"
]

Test[
    Block[
        {$WorkbenchProjects = {"E:\\Users\\Daniel\\git\\generalutilities"}},
        CalculateParse`GeneralLibrary`Private`getPathRelativeToWorkbenchProjects[
            "E:\\Users\\Daniel\\git\\generalutilities\\GeneralUtilities"
        ]
    ]
    ,
    "/generalutilities/GeneralUtilities"
    ,
    TestID -> "getPathRelativeToWorkbenchProjects-20160331-YUA3DN"
]

Test[
    Block[
        {$WorkbenchProjects = {"E:\\Users\\Daniel\\git\\generalutilities"}},
        CalculateParse`GeneralLibrary`Private`getPathRelativeToWorkbenchProjects[
            "E:\\Users\\Daniel\\git\\generalutilities"
        ]
    ]
    ,
    "/generalutilities"
    ,
    TestID -> "getPathRelativeToWorkbenchProjects-20160331-7CYSKS"
]