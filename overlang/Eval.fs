module Eval
open System
open AST
open EvalTypes
open EvalSym

let rec evalElem (ctx: Context) (e: Element) =
    (
        match e with
        | EConst (value, _) -> value
        | EId (name, position) ->
            if ctx.prog.ContainsKey name
            then evalFunction ctx ctx.prog[name]
            else 0.0
        | ESym _ -> evalSymbol ctx e
    ) |> ctx.vs.Add
    ctx.vs[ctx.vs.Count - 1]

and evalFunction (ctx: Context) (f: Function) =
    if countFuncs ctx f.name = f.limit
    then // call next function
        nextByOrder ctx.prog f.name |> evalFunction ctx
    else
        for e in f.code do
            evalElem ctx e |> ctx.vs.Add

        ctx.vs[ctx.vs.Count - 1]

let eval (ctx: Context) (prog: Prog) = 
    if prog.ContainsKey("() main")
    then evalFunction ctx prog["() main"]
    else Exception "Failed to find function '() main'" |> raise
