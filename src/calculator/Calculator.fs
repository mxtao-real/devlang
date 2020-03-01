namespace Calculator

open System

module Tokenizer = 
    type TokenKind = 
        | BadToken
        | NumberToken
        | AddOperatorToken
        | SubOperatorToken
        | MulOperatorToken
        | DivOperatorToken
        | LeftPartToken
        | RightPartToken
        | EndOfLineToken
    type Token = {kind: TokenKind; value: double option; str: string option}
    let private parseTokens (str: string) =
        let innerToken = {kind = BadToken; value = None; str = None}
        let charSeq = str.ToCharArray() |> Seq.ofArray
        let matchSingleChar (c: char) = 
            match c with
            | '+' -> {innerToken with kind = AddOperatorToken}
            | '-' -> {innerToken with kind = SubOperatorToken}
            | '*' -> {innerToken with kind = MulOperatorToken}
            | '/' -> {innerToken with kind = DivOperatorToken}
            | '(' -> {innerToken with kind = LeftPartToken}
            | ')' -> {innerToken with kind = RightPartToken}
            | ' ' | '\n' -> {innerToken with kind = EndOfLineToken}
            | unknown -> {kind = BadToken; value = None; str = Some (string unknown)}
        seq {
            use enumerator = charSeq.GetEnumerator()
            while enumerator.MoveNext() do 
                match enumerator.Current with 
                | c when Char.IsDigit(c) -> 
                    let mutable dotted = false
                    let mutable s = seq {c}
                    while (enumerator.MoveNext() && (Char.IsDigit(enumerator.Current) || (not dotted && enumerator.Current = '.'))) do
                        s <- Seq.append s [enumerator.Current]
                        if enumerator.Current = '.' then dotted <- true
                    match Seq.last s with
                    | sl when Char.IsDigit(sl) ->
                        yield {kind = NumberToken; value = Some (s |> String.Concat |> double); str = Some (s |> String.Concat)}
                        yield matchSingleChar (try enumerator.Current with _ -> ' ')
                    | _ -> yield {kind = BadToken; value = None; str = Some (s |> String.Concat)}
                | _ -> yield matchSingleChar enumerator.Current
        }
    type Tokenizer(line) = 
        let tokenList = parseTokens line |> ResizeArray
        let mutable index = 0   // current token index
        member val Tokens = tokenList
        member this.Item with get(index) = this.Tokens.[index]
        member this.GetToken() = index <- index + 1; this.[index - 1]
        member _.UnGetToken() = index <- index - 1
        member _.Reset() = index <- 0

module Parser = 
    open Tokenizer
    type Parser(tokenizer: Tokenizer) = 
        new(line) =  Parser(Tokenizer line)
        member this.Parse() = this.ParseExpression()
        member private this.ParsePrimaryExpression() =
            match tokenizer.GetToken() with
            | {kind = NumberToken; value = Some number} -> number
            | {kind = SubOperatorToken} -> - this.ParsePrimaryExpression()
            | {kind = LeftPartToken} -> 
                let value = this.ParseExpression()
                match tokenizer.GetToken() with
                | {kind = RightPartToken} -> value
                | token -> failwithf "syntax error! at %A" token
            | token -> failwithf "syntax error! at %A" token
        member private this.ParseTerm() =
            let rec innerParseTerm (value: double) (tokenizer: Tokenizer) = 
                match tokenizer.GetToken() with
                | {kind = MulOperatorToken} -> 
                    let value2 = this.ParsePrimaryExpression()
                    innerParseTerm (value * value2) tokenizer
                | {kind = DivOperatorToken} ->
                    let value2 = this.ParsePrimaryExpression()
                    innerParseTerm (value / value2) tokenizer
                | _ -> tokenizer.UnGetToken(); value
            let value = this.ParsePrimaryExpression()
            innerParseTerm value tokenizer
        member private this.ParseExpression() = 
            let rec innerParseExpression (value: double) (tokenizer: Tokenizer) =
                match tokenizer.GetToken() with
                | {kind = AddOperatorToken} ->
                    let value2 = this.ParseTerm()
                    innerParseExpression (value + value2) tokenizer
                | {kind = SubOperatorToken} ->
                    let value2 = this.ParseTerm()
                    innerParseExpression (value - value2) tokenizer
                | _ -> tokenizer.UnGetToken(); value
            let value = this.ParseTerm()
            innerParseExpression value tokenizer