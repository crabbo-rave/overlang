module EvalSym
open EvalTypes
open AST
open System

(*
¬         ∧   ∨   ⊻     ⊼    ⊽
NOT/NEG AND  OR  XOR  NAND  NOR

so current list of operators:
∑ ∏ ¬ ∧ ∨ ⊻ ⊼ ⊽ ∅ ∐ ℤ ⌊⌋ ⌈⌉ ⪌
maybe ∅ - sort of like null
∐ - division
ℤ - convert to integer, truncate
⌊⌋ - floor
⌈⌉ - ceil
⪌ - comparison (1 if >, 0 if =, -1 if <) 

*)

let private toBoolean (f: float) =
    if f < 0 then false else true

let private fromBoolean (f: bool) =
    if f then 1 else -1

let private xor (a: bool) (b: bool) =
    (a && (not b)) || (b && (not a))

let private nand (a: bool) (b: bool) =
    not (a && b)

let private nor (a: bool) (b: bool) =
    not (a || b)

let evalSymbol (ctx: Context) (e: string) (p: FParsec.Position) =
    match e with
    | "∑" ->
        ctx.AtLeast 2 p
        ctx.Pop + ctx.Pop |> ctx.vs.Add
    | "∏" ->
        ctx.AtLeast 2 p
        ctx.Pop * ctx.Pop |> ctx.vs.Add
    | "∐" ->
        ctx.AtLeast 2 p
        ctx.Pop / ctx.Pop |> ctx.vs.Add
    | "ℤ" ->
        ctx.AtLeast 1 p    
        int ctx.Pop |> ctx.vs.Add
    | "⌊⌋" ->
        ctx.AtLeast 1 p
        System.Math.Floor ctx.Pop |> ctx.vs.Add
    | "⌈⌉" ->
        ctx.AtLeast 1 p
        System.Math.Ceiling ctx.Pop |> ctx.vs.Add
    | "⪌" ->
        ctx.AtLeast 2 p
        let a = ctx.Pop
        let b = ctx.Pop
        if a > b then ctx.vs.Add 1
        if a = b then ctx.vs.Add 0
        if a < b then ctx.vs.Add -1
    | "¬" ->
        ctx.AtLeast 1 p
        -ctx.Pop |> ctx.vs.Add
    | "∧" ->
        ctx.AtLeast 2 p
        fromBoolean (toBoolean ctx.Pop && toBoolean ctx.Pop) |> ctx.vs.Add
    | "∨" ->
        ctx.AtLeast 2 p
        fromBoolean (toBoolean ctx.Pop || toBoolean ctx.Pop) |> ctx.vs.Add
    | "⊻" ->
        ctx.AtLeast 2 p
        fromBoolean (xor (toBoolean ctx.Pop) (toBoolean ctx.Pop)) |> ctx.vs.Add
    | "⊼" ->
        ctx.AtLeast 2 p
        fromBoolean (nand (toBoolean ctx.Pop) (toBoolean ctx.Pop)) |> ctx.vs.Add
    | "⊽" ->
        ctx.AtLeast 2 p
        fromBoolean (nor (toBoolean ctx.Pop) (toBoolean ctx.Pop)) |> ctx.vs.Add
    | _ ->
        printError $"Unkown symbol: '{e}'" p
        exit 1

