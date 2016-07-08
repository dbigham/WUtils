(* Tests for: WUtils`WUtils`FunctionWithNoArgs

   Author: danielb
*)

Test[
    WUtils`WUtils`FunctionWithNoArgs["myFunc[]"]
    ,
    True
    ,
    TestID -> "FunctionWithNoArgs-20150225-JXQTIM"
]

Test[
    WUtils`WUtils`FunctionWithNoArgs["myFunc[arg]"]
    ,
    False
    ,
    TestID -> "FunctionWithNoArgs-20150225-OMDUNB"
]