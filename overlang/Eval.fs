module Eval
open System
open AST
open EvalTypes
open EvalSym
open Parser

let rec evalElem (ctx: Context) (e: Element) =
    match e with
    | EConst (value, _) -> value |> ctx.Last.vals.Add
    | EId (comment, name, position) ->
        if ctx.prog.ContainsKey name
        then
            let indices =
                match FParsec.CharParsers.run parseCommentIndices comment with
                | FParsec.CharParsers.Success(result, _, _)   -> result
                | FParsec.CharParsers.Failure(errorMsg, _, _) ->
                    printfn "%s" errorMsg
                    []

            let prevStack = ctx.stack[ctx.stack.Count - 2]

            { func = ctx.prog[name]; vals = VStack() } |> ctx.stack.Add
            for idx in indices do ctx.Last.vals.Add prevStack.vals[prevStack.vals.Count - idx]
            
            evalFunction ctx ctx.prog[name]
        else
            printError $"Unknown identifier: '{name}'" position
            0.0 |> ctx.Last.vals.Add
    | ESym (name, position) -> evalSymbol ctx name position

and evalFunction (ctx: Context) (f: Function) =
    if countFuncs ctx f.name = f.limit
    then // call next function
        nextByOrder ctx.prog f.name |> evalFunction ctx
    else // evaluate this call
        for e in f.code do
            evalElem ctx e

let eval (ctx: Context) (prog: Prog) = 
    if prog.ContainsKey("() main")
    then
        { func = prog["() main"]; vals = VStack() } |> ctx.stack.Add
        evalFunction ctx prog["() main"]
        ctx.stack.RemoveAt(ctx.stack.Count - 1)
    else Exception "Failed to find function '() main'" |> raise
