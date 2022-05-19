module EvalTypes
open System
open AST

let printError (msg: string) (where: FParsec.Position) =
    FParsec.Error.ParserError(where, null, FParsec.Error.messageError msg).WriteTo(stderr)

type VStack = Collections.Generic.List<float>
type FStackElement = { func: Function; vals: VStack }
type FStack = Collections.Generic.List<FStackElement>
type Context =
    { stack: FStack; prog: Prog }

    member this.Last = this.stack[this.stack.Count - 1]
    member this.LastLast = this.Last.vals[this.Last.vals.Count - 1]

    member this.Pop =
        let v = this.LastLast
        this.stack.RemoveAt(this.Last.vals.Count - 1)
        v

    member this.AtLeast (n: int) (where: FParsec.Position) =
        if this.Last.vals.Count < n
        then printError $"At least {n} values on the stack are required!" where

let countFuncs (ctx: Context) (name: string) =
    ctx.stack |> Seq.fold
        (fun x (y: FStackElement) ->
            x + (if y.func.name = name then 1 else 0))
        0
