module EvalTypes
open System
open AST

let printError (msg: string) (where: FParsec.Position) =
    FParsec.Error.ParserError(where, null, FParsec.Error.messageError msg).WriteTo(stderr)

type VStack = Collections.Generic.List<float>
type FStack = Collections.Generic.List<Function>
type Context =
    { vs: VStack; fs: FStack; prog: Prog }
    member this.Pop =
        let v = this.vs[this.vs.Count - 1]
        this.vs.RemoveAt(this.vs.Count - 1)
        v
    member this.AtLeast (n: int) (where: FParsec.Position) =
        if this.vs.Count < n
        then printError $"At least {n} values on the stack are required!" where

let countFuncs (ctx: Context) (name: string) =
    ctx.fs |> Seq.fold
        (fun x (y: Function) ->
            x + (if y.name = name then 1 else 0))
        0

