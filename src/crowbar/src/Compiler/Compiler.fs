
/// <summary>Crowbar compiler, processing source file, generate abstract syntax tree.</summary>
namespace Crowbar.Compiler

/// <summary>Tokenizer, processing source file, generate token stream..</summary>
module Tokenizer =
    open System
    open System.IO

    /// <summary>token position in source file</summary>
    type Position = {file: string; line: int; column: int}

    /// <summary>token's kind</summary>
    type TokenKind =
        | Add           // operator: +
        | Subtract      // operator: -
        | Multiply      // operator: *
        | Divide        // operator: /
        | Mod           // operator: %
        | Greater       // operator: >
        | GreaterEqual  // operator: >=
        | Less          // operator: <
        | LessEqual     // operator: <=
        | Equal         // operator: ==
        | NotEqual      // operator: !=
        | And           // operator: &&
        | Or            // operator: ||
        | Assign        // operator: =
        | Comma         // ,
        | Point         // .
        | Semicolon     // ;
        | LeftParenthesis   // (
        | RightParenthesis  // )
        | LeftCurlyBrace    // {
        | RightCurlyBrace   // }
        | If            // keyword: if
        | Else          // keyword: else
        | ElIf          // keyword: elif
        | Function      // keyword: function
        | Return        // keyword: return
        | Global        // keyword: global
        | For           // keyword: for
        | While         // keyword: while
        | Continue      // keyword: continue
        | Break         // keyword: break
        | True          // bool literal: true
        | False         // bool literal: false
        | Int           // int value
        | String        // string value
        | Double        // double value
        | Identifer     // identifer with name
        | EOF           // end of file
        | Error         // for error token 

    /// <summary>token instance</summary>
    type Token = {kind: TokenKind; value: string option; position: Position}

    // internal CharReader
    type private CharReader(path: string) =
        do if isNull path then nullArg "the reader argument cannot be null"
        let reader = new StreamReader(path)
        let mutable line = 1
        let mutable column = 1
        member _.IsEnd = reader.EndOfStream
        member _.NotEnd = reader.EndOfStream |> not
        member _.FileName = Path.GetFileName path
        member _.FilePath = path
        member _.Line = line
        member _.Column = column
        member _.Peek() = reader.Peek() |> char
        member _.Read() =
            match reader.Read() |> char with
            | '\n' -> line <- line + 1; column <- 1; '\n'
            | c -> column <- column + 1; c
        member _.Close() = reader.Close()
        interface IDisposable with
            member _.Dispose() = reader.Dispose()

    // skip white spaces and emit a token
    let private parseOneToken(reader: CharReader) =
        let isUnusedChar c = c = ' ' || c = '\n' || c = '\r' || c = '\t'
        let rec skip (r: CharReader) =
            match r.Peek() with
            | '#' -> while r.NotEnd && r.Peek() <> '\n' do r.Read() |> ignore done; skip r
            | c when isUnusedChar c -> r.Read() |> ignore; skip r
            | _ -> ()
        let parse (r: CharReader) = 
            let pos = {file = r.FileName; line = r.Line; column = r.Column}
            match r.Read() with
            | '+' -> {kind = Add; value = None; position = pos}
            | '-' -> {kind = Subtract; value = None; position = pos}
            | '*' -> {kind = Multiply; value = None; position = pos}
            | '/' -> {kind = Divide; value = None; position = pos}
            | '%' -> {kind = Mod; value = None; position = pos}
            | '>' -> if r.Peek() <> '=' then {kind = Greater; value = None; position = pos}
                     else r.Read() |> ignore; {kind = GreaterEqual; value = None; position = pos}
            | '<' -> if r.Peek() <> '=' then {kind = Less; value = None; position = pos}
                     else r.Read() |> ignore; {kind = LessEqual; value = None; position = pos}
            | '=' -> if r.Peek() <> '=' then {kind = Assign; value = None; position = pos}
                     else r.Read() |> ignore; {kind = Equal; value = None; position = pos}
            | '!' -> if r.Peek() <> '=' then {kind = Error; value = Some "!"; position = pos}
                     else r.Read() |> ignore; {kind = NotEqual; value = None; position = pos}
            | '&' -> if r.Peek() <> '&' then {kind = Error; value = Some "&"; position = pos}
                     else r.Read() |> ignore; {kind = And; value = None; position = pos}
            | '|' -> if r.Peek() <> '|' then {kind = Error; value = Some "|"; position = pos}
                     else r.Read() |> ignore; {kind = Or; value = None; position = pos}
            | ',' -> {kind = Comma; value = None; position = pos}
            | '.' -> {kind = Point; value = None; position = pos}
            | ';' -> {kind = Semicolon; value = None; position = pos}
            | '(' -> {kind = LeftParenthesis; value = None; position = pos}
            | ')' -> {kind = RightParenthesis; value = None; position = pos}
            | '{' -> {kind = LeftCurlyBrace; value = None; position = pos}
            | '}' -> {kind = RightCurlyBrace; value = None; position = pos}
            | '"' ->
                let list = ResizeArray<char>()
                while r.NotEnd && r.Peek() <> '"' do list.Add(r.Read())
                r.Read() |> ignore // throw another '"'
                let str = list |> String.Concat
                if r.IsEnd then {kind = Error; value = Some str; position = pos}
                else {kind = String; value = Some str; position = pos}
            | d when Char.IsDigit d ->
                let list = ResizeArray<char>()
                list.Add(d)
                while r.NotEnd && (Char.IsDigit(r.Peek()) || r.Peek() = '.') do list.Add(r.Read())
                let count = list |> Seq.filter (fun c -> c = '.') |> Seq.length
                let str = list |> String.Concat
                if count = 0 then {kind = Int; value = Some str; position = pos}
                elif count = 1 then {kind = Double; value = Some str; position = pos}
                else {kind = Error; value = Some str; position = pos}
            | c ->
                let list = ResizeArray<char>()
                list.Add(c)
                while r.NotEnd && (Char.IsLetterOrDigit(r.Peek()) || r.Peek() = '_') do list.Add(r.Read())
                let str = list |> String.Concat
                match str with
                | "if" -> {kind = If; value = None; position = pos}
                | "else" -> {kind = Else; value = None; position = pos}
                | "elif" -> {kind = ElIf; value = None; position = pos}
                | "function" -> {kind = Function; value = None; position = pos}
                | "return" -> {kind = Return; value = None; position = pos}
                | "global" -> {kind = Global; value = None; position = pos}
                | "for" -> {kind = For; value = None; position = pos}
                | "while" -> {kind = While; value = None; position = pos}
                | "continue" -> {kind = Continue; value = None; position = pos}
                | "break" -> {kind = Break; value = None; position = pos}
                | "true" -> {kind = True; value = None; position = pos}
                | "false" -> {kind = False; value = None; position = pos}
                | _ -> {kind = Identifer; value = Some str; position = pos}
        let parseEOF (r: CharReader) = 
            let pos = {file = r.FileName; line = r.Line; column = r.Column}
            {kind = EOF; value = None; position = pos}
        // do real parse, skip white spaces and parse a token
        skip reader
        if reader.NotEnd then parse reader else parseEOF reader

    /// <summary>tokenizer, parse source file to tokens</summary>
    /// <param name="path">the path to source file.</param>
    /// <returns>token sequence.</returns>
    [<CompiledName("Tokenize")>]
    let tokenize (path: string) = 
        seq {
            use reader = new CharReader(path)
            while reader.NotEnd do
                yield parseOneToken reader
            yield parseOneToken reader // emit an EOF token here
        }

    // just for inner test
    let private innerTest () = 
        let pretty t =
            let {kind = k; value = v; position = {line = l; column = c; file = _}} = t
            match v with
            | Some s -> printfn "%2d:%2d -> [%A] \"%s\"" l c k s
            | None -> printfn "%2d:%2d -> [%A]" l c k
        let path = "resource/tokenizer-test.crb"
        tokenize path |> Seq.iter pretty

module AbstractSyntaxTree =

    type BiExpKind = 
        | Add           // operator: +
        | Subtract      // operator: -
        | Multiply      // operator: *
        | Divide        // operator: /
        | Mod           // operator: %
        | Greater       // operator: >
        | GreaterEqual  // operator: >=
        | Less          // operator: <
        | LessEqual     // operator: <=
        | Equal         // operator: ==
        | NotEqual      // operator: !=
        | And           // operator: &&
        | Or            // operator: ||

    type Expression =
        | IntLiteral of int
        | DoubleLiteral of double
        | BoolLiteral of bool
        | StringLiteral of string
        | VarRefExp of VarRefExp
        | FuncInvokeExp of FuncInvokeExp
        | BinaryExp of BinaryExp
    and VarRefExp = {name: string}
    and FuncInvokeExp = {name: string; args: Expression list}
    and BinaryExp = {kind: BiExpKind; left: Expression; right: Expression}

    type Statement =
        | GlobalRefStmt of GlobalRefStmt
        | AssignStmt of AssignStmt
        | ReturnStmt of ReturnStmt
        | IfElseStmt of IfElseStmt
        | WhileStmt of WhileStmt
        | ForStmt of ForStmt
        | BreakStmt
        | ContinueStmt
    and GlobalRefStmt = {names: string list}
    and AssignStmt = {name: string; value: Expression}
    and ReturnStmt = {exp: Expression option}
    and IfElseStmt = {condition: Expression; trueBody: Statement list; falseBody: Statement list}
    and WhileStmt = {condition: Expression; body: Statement list}
    and ForStmt = {init: Statement; inc: Statement; condition: Expression; body: Statement list}

module Parser =
    open Tokenizer
    open AbstractSyntaxTree

    // todo: fix here ...
    [<AbstractClass>]
    type private TokenReader =
        abstract member Mark: unit -> unit      // mark
        abstract member UnMark: unit -> unit    // stop mark
        abstract member Reset: unit -> unit     // reset mark state
        abstract member Peek: unit -> Token
        abstract member Read: unit -> Token
        member this.EatToken (kind: TokenKind) =
            match this.Read() with
            | {kind = k} when kind = k -> ()
            | t -> sprintf "need a '%A' token here but got a %A" kind t |> failwith

    // type private MarkableTokenReader(tokens: Token seq) =
    //     let mutable marking = false
    //     let enumrator = tokens.GetEnumerator()
    //     member _.Mark() = marking <- true
    //     member _.UnMark() = marking <- false
    //     member _.Peek() = enumrator.

    module private rec ExpParser = 

        let parseIntLiteral (reader: TokenReader) =
            match reader.Read() with
            | {kind = Int; value = Some str} -> str |> int |> IntLiteral
            | t -> t |> sprintf "need an int value here but got a %A " |> failwith

        let parseDoubleLiteral (reader: TokenReader) =
            match reader.Read() with
            | {kind = Double; value = Some str} -> str |> double |> DoubleLiteral
            | t -> t |> sprintf "need a double value here but got a %A " |> failwith

        let parseBoolLiteral (reader: TokenReader) =
            match reader.Read() with
            | {kind = True} -> BoolLiteral true
            | {kind = False} -> BoolLiteral false
            | t -> t |> sprintf "need true/false here but got a %A " |> failwith

        let parseStringLiteral (reader: TokenReader) = 
            match reader.Read() with 
            | {kind = String; value = Some str} -> StringLiteral str
            | t -> t |> sprintf "need a string value here but got a %A " |> failwith

        let parseVarRefExp (reader: TokenReader) =
            match reader.Read() with 
            | {kind = Identifer; value = Some str} -> VarRefExp {name = str}
            | t -> t |> sprintf "need a identifer here but got a %A " |> failwith

        let parseFuncInvokeExp (reader: TokenReader) =
            let rec parseArgList (r: TokenReader) (list: Expression list) =
                match r.Peek() with
                | {kind = RightParenthesis} -> list
                | _ -> parseExpression r |> List.singleton |> (@) list |> parseArgList r
            match reader.Read() with
            | {kind = Identifer; value = Some str} ->
                reader.EatToken LeftParenthesis
                let args = parseArgList reader []
                reader.EatToken RightParenthesis
                FuncInvokeExp {name = str; args = args}
            | t -> t |> sprintf "need a identifer here but got a %A " |> failwith

        let parseBinaryExp (reader: TokenReader) = 
            let left = parseExpression reader
            let op =
                match reader.Read() with
                | {kind = TokenKind.Add} -> Add
                | _ -> failwith ""
            let right = parseExpression reader
            BinaryExp {kind = op; left = left; right = right}

        let parseExpression (reader: TokenReader) =
            parseFuncInvokeExp reader


    // let parse 

// http://tomasp.net/blog/csharp-fsharp-async-intro.aspx/
// https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/
// module Parser =
//     // parser combinator 
//     // https://fsharpforfunandprofit.com/series/understanding-parser-combinators.html
//     type Parser<'T> = Parser of (string -> Result<'T, string>)
