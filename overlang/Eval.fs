module Eval
open System
open AST
open EvalTypes
open EvalSym

let rec evalElem (ctx: Context) (e: Element) =
    match e with
    | EConst (value, _) -> value |> ctx.vs.Add
    | EId (comment, name, position) ->
        if ctx.prog.ContainsKey name
        then evalFunction ctx ctx.prog[name]
        else printError $"Unknown identifier: '{name}'" position
             0.0 |> ctx.vs.Add
    | ESym (name, position) -> evalSymbol ctx name position

and evalFunction (ctx: Context) (f: Function) =
    if countFuncs ctx f.name = f.limit
    then // call next function
        nextByOrder ctx.prog f.name |> evalFunction ctx
    else
        ctx.fs.Add f
        
        for e in f.code do
            evalElem ctx e

        ctx.fs.RemoveAt(ctx.fs.Count - 1)

let eval (ctx: Context) (prog: Prog) = 
    if prog.ContainsKey("() main")
    then evalFunction ctx prog["() main"]
    else Exception "Failed to find function '() main'" |> raise
