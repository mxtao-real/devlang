
/// <summary>Crowbar Compiler, generate abstract syntax tree.</summary>

module Crowbar.Compiler

/// <summary></summary>
/// <param name=""></param>
/// <returns></returns>

module Tokenizer =
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
        | Greater       // >
        | GreaterEqual  // >=
        | Less      // <
        | LessEqual // <=
        | Equal     // ==
        | NotEqual  // !=
        | And   // &&
        | Or    // ||
        | Assign        // operator: =
        | Comma // ,
        | Point // .
        | Semicoln  // ;
        | LeftParenthesis   // (
        | RightParen // )
        | LeftCurlyBrace // {
        | Right // }
        | True          // bool literal: true
        | False         // bool literal: false
        | Int           // int value
        | String        // string value
        | Double        // double value
        | Indentifer    // identifer with name
        | Error         // for error token 

    /// <summary>token</summary>
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
        interface System.IDisposable with
            member _.Dispose() = reader.Dispose()

    let private singeCharToken (reader: CharReader) =
        let pos = {file = reader.FileName; line = reader.Line; column = reader.Column}
        if reader.IsEnd then {kind = Val; value = None; position = pos}
        else
            match reader.Read() with
            | '+' -> {kind = Add; value = None; position = pos}
            | c when Char.IsDigit c -> 

    let private multiCharToken (reader: CharReader) = 
        let pos = {file = reader.FileName; line = reader.Line; column = reader.Column}
        match reader.Read() with
        | i when Char.IsDigit i -> 
            let co = new ResizeArray<char>()
            co.Add(' ')

    let tokenize (path: string) = 
        let genToken (reader: CharReader) =
            let pos = {file = reader.FileName; line = reader.Line; column = reader.Column}
            if reader.IsEnd then {kind = Val; value = None; position = pos}
            else
                match reader.Read() with
                | '+' -> {kind = Add; value = None; position = pos}
                | c when Char.IsDigit c -> 


        let sr = new StreamReader(path)
        sr

    // let reader = new StreamReader("")

    // char reader needn't marking future
    // type CharReader(reader: TextReader)  = 
    //     let mutable marking = false
    //     let mutable chars = List.empty
    //     member _.Mark() = marking <- true
    //     member _.UnMark() = marking <- false; chars <- List.empty
    //     member _.Read() = 
    //         match (marking, chars) with
    //         | (false, []) -> reader.Read() |> char
    //         | (false, head::tail) -> chars <- tail; head
    //         | (true, _) -> chars <- chars @ [reader.Read() |> char]; List.last chars
    //     member _.Peek() = reader.Peek() |> char
    //     member _.Close() = if isNull reader then reader.Close()
    //     interface IDisposable with
    //         member _.Dispose() =
    //             if isNull reader then reader.Dispose()


// http://tomasp.net/blog/csharp-fsharp-async-intro.aspx/
// https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/

module Parser =
    // parser combinator 
    // https://fsharpforfunandprofit.com/series/understanding-parser-combinators.html
    type Parser<'T> = Parser of (string -> Result<'T, string>)
