module Eval

open AST
open System

let evalFunction (f: Function) = 0

let eval (prog: Prog) =
    if prog.ContainsKey("main")
    then evalFunction prog["main"]
    else Exception "Failed to find function 'main'" |> raise
