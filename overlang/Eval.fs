module Eval

open AST
open System
open System.Linq.Expressions
type SysList<'T> = Collections.Generic.List<'T>

type VStack = SysList<float>
type FStack = SysList<Function>
type Context = { vs: VStack; fs: FStack; prog: Prog }

let countFuncs (ctx: Context) (name: string) =
    ctx.fs |> Seq.fold
        (fun x (y: Function) ->
            x + (if y.name = name then 1 else 0))
        0

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

and evalSymbol (ctx: Context) (e: Element) =
    0.0

and evalFunction (ctx: Context) (f: Function) =
    if countFuncs ctx f.name = f.limit
    then 0.0
    else
        for e in f.code do
            evalElem ctx e |> ctx.vs.Add

        ctx.vs[ctx.vs.Count - 1]

let eval (ctx: Context) (prog: Prog) = 

    if prog.ContainsKey("() main")
    then evalFunction ctx prog["() main"]
    else Exception "Failed to find function '() main'" |> raise
