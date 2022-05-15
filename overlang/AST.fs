module AST

open System.Collections.Generic
open FParsec;

type Element = 
    | EConst of value: float * position: Position
    | ESym of symbol: string * position: Position
    | EId of name: string * position: Position

type Function = { comment: string;
                  code: Element[];
                  position: Position }

type Prog = Dictionary<string, Function>
