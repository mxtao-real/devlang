
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
                | "elif" | "elsif" -> {kind = ElIf; value = None; position = pos}
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

module Parser =
    open Tokenizer
    open Crowbar.Lang.AbstractSyntaxTree

    // peek one and two
    type private TokenReader(tokens: Token seq) =
        let enumerator = tokens.GetEnumerator()
        do if enumerator.MoveNext() |> not then failwith "cannot get a token"
        let mutable window = [enumerator.Current]
        do if enumerator.MoveNext() then window <- window @ [enumerator.Current]
        // peek current
        member _.Peek() =
            match window with
            | [] -> failwith "nothing to peek ..."
            | t :: _ -> t
        // peek next
        member _.PeekNext() = 
            match window with
            | _ :: t :: _ -> t
            | _ -> failwith "nothing to peek ..."
        // read token
        member _.Read() =
            match window with
            | [] -> failwith "nothing to read..."
            | head :: tail ->
                if enumerator.MoveNext() then
                    window <- tail @ [enumerator.Current]
                else window <- tail
                head
        // eat a token
        member this.EatToken(kind: TokenKind) =
            match this.Read() with
            | {kind = k} when kind = k -> ()
            | t -> failwithf "need a '%A' token here but got a %A" kind t

    module private rec ExpParser = 
        // ------------------ atom expressions ------------------
        let parseIntLiteral (reader: TokenReader) =
            match reader.Read() with
            | {kind = Int; value = Some str} -> str |> int |> IntLiteral
            | t -> failwithf "need an int value here but got a %A" t

        let parseDoubleLiteral (reader: TokenReader) =
            match reader.Read() with
            | {kind = Double; value = Some str} -> str |> double |> DoubleLiteral
            | t -> failwithf "need a double value here but got a %A" t

        let parseBoolLiteral (reader: TokenReader) =
            match reader.Read() with
            | {kind = True} -> BoolLiteral true
            | {kind = False} -> BoolLiteral false
            | t -> failwithf "need true/false here but got a %A" t

        let parseStringLiteral (reader: TokenReader) = 
            match reader.Read() with 
            | {kind = String; value = Some str} -> StringLiteral str
            | t -> failwithf "need a string value here but got a %A" t

        let parseVarRefExp (reader: TokenReader) =
            match reader.Read() with 
            | {kind = Identifer; value = Some str} -> VarRefExp {name = str}
            | t -> failwithf "need a identifer here but got a %A" t

        let parseFuncInvokeExp (reader: TokenReader) =
            let rec parseArgList (r: TokenReader) (list: Expression list) =
                match r.Peek() with
                | {kind = RightParenthesis} -> list
                // | {kind = Comma} -> r.EatToken Comma; parseArgList r list
                | {kind = Comma} -> r.EatToken Comma; list @ [parseExpression r] |> parseArgList r // make sure an expression following ','
                | _ -> list @ [parseExpression r] |> parseArgList r
            match reader.Read() with
            | {kind = Identifer; value = Some str} ->
                reader.EatToken LeftParenthesis
                let args = parseArgList reader []
                reader.EatToken RightParenthesis
                FuncInvokeExp {name = str; args = args}
            | t -> failwithf "need a identifer here but got a %A" t

        let parseSubExp (reader: TokenReader) =
            reader.EatToken LeftParenthesis
            let exp = parseExpression reader
            reader.EatToken RightParenthesis
            exp

        let parseAtomExpression (reader: TokenReader) =
            match reader.Peek() with
            | {kind = LeftParenthesis} -> parseSubExp reader
            | {kind = Int} -> parseIntLiteral reader
            | {kind = Double} -> parseDoubleLiteral reader
            | {kind = True} | {kind = False} -> parseBoolLiteral reader
            | {kind = String} -> parseStringLiteral reader
            | {kind = Identifer} ->
                match reader.PeekNext() with
                | {kind = LeftParenthesis} -> parseFuncInvokeExp reader
                | _ -> parseVarRefExp reader
            | t -> failwithf "cannot parse expression start with %A" t

        // ------------------ binary expressions ------------------
        let ``parse * / %`` (reader: TokenReader) =
            let exp = parseAtomExpression reader
            let o = match reader.Peek() with
                    | {kind = TokenKind.Multiply} -> ValueSome Multiply
                    | {kind = TokenKind.Divide} -> ValueSome Divide
                    | {kind = TokenKind.Mod} -> ValueSome Mod
                    | _ -> ValueNone
            match o with
            | ValueNone -> exp
            | ValueSome op -> reader.Read() |> ignore; BinaryExp {kind = op; left = exp; right = ``parse * / %`` reader}

        let ``parse + -`` (reader: TokenReader) = 
            let exp = ``parse * / %`` reader
            let o = match reader.Peek() with
                    | {kind = TokenKind.Add} -> ValueSome Add
                    | {kind = TokenKind.Subtract} -> ValueSome Subtract
                    | _ -> ValueNone
            match o with 
            | ValueNone -> exp
            | ValueSome op -> reader.Read() |> ignore; BinaryExp {kind = op; left = exp; right = ``parse + -`` reader}

        let ``parse > >= < <= == != `` (reader: TokenReader) =
            let exp = ``parse + -`` reader
            let o = match reader.Peek() with
                    | {kind = TokenKind.Greater} -> ValueSome Greater
                    | {kind = TokenKind.GreaterEqual} -> ValueSome GreaterEqual
                    | {kind = TokenKind.Less} -> ValueSome Less
                    | {kind = TokenKind.LessEqual} -> ValueSome LessEqual
                    | {kind = TokenKind.Equal} -> ValueSome Equal
                    | {kind = TokenKind.NotEqual} -> ValueSome NotEqual
                    | _ -> ValueNone
            match o with
            | ValueNone -> exp
            | ValueSome op -> reader.Read() |> ignore; BinaryExp {kind = op; left = exp; right = ``parse + -`` reader}

        let ``parse && ||`` (reader: TokenReader) =
            let exp = ``parse > >= < <= == != `` reader
            let o = match reader.Peek() with
                    | {kind = TokenKind.And} -> ValueSome And
                    | {kind = TokenKind.Or} -> ValueSome Or
                    | _ -> ValueNone
            match o with
            | ValueNone -> exp
            | ValueSome op -> reader.Read() |> ignore; BinaryExp {kind = op; left = exp; right = ``parse > >= < <= == != `` reader}

        // to outside
        let parseExpression = ``parse && ||``

    // just use expression parser here ...
    let private parseExp = ExpParser.parseExpression

    module private rec StmtParser = 

        // target: "a, b, c, d"
        let rec private parseNameList (reader: TokenReader) (list: string list) =
            match reader.Peek() with
            | {kind = Identifer; value = Some str} -> reader.EatToken Identifer; list @ [str] |> parseNameList reader
            | {kind = Comma} -> 
                // make sure a name following ','
                reader.EatToken Comma
                match reader.Read() with
                | {kind = Identifer; value = Some str} -> list @ [str] |> parseNameList reader
                | t -> failwithf "need a identifer here but got a %A" t
            | _ -> list

        let parseRawExpStmt (reader: TokenReader) =
            let exp = parseExp reader
            reader.EatToken Semicolon
            RawExpStmt {expression = exp}

        let parseGlobalRefStmt (reader: TokenReader) =
            reader.EatToken Global
            let list = parseNameList reader []
            reader.EatToken Semicolon
            GlobalRefStmt {names = list}

        let parseAssignStmt (reader: TokenReader) =
            let n = match reader.Read() with
                    | {kind = Identifer; value = Some str} -> str
                    | t -> failwithf "need a identifer here but got a %A" t
            reader.EatToken Assign
            let exp = parseExp reader
            reader.EatToken Semicolon
            AssignStmt {name = n; value = exp}

        let parseReturnStmt (reader: TokenReader) =
            reader.EatToken Return
            let e = match reader.Peek() with
                    | {kind = Semicolon} -> None
                    | _ -> parseExp reader |> Some
            reader.EatToken Semicolon
            ReturnStmt {exp = e}

        let parseIfElseStmt (reader: TokenReader) = 
            match reader.Read() with
            | {kind = If} | {kind = ElIf} -> ()
            | t -> failwithf "need a if/elif here but got a %A" t
            reader.EatToken LeftParenthesis
            let cond = parseExp reader
            reader.EatToken RightParenthesis
            reader.EatToken LeftCurlyBrace
            let t = parseStmtList reader
            reader.EatToken RightCurlyBrace
            match reader.Peek() with
            | {kind = Else} ->
                reader.EatToken Else
                reader.EatToken LeftCurlyBrace
                let f = parseStmtList reader
                reader.EatToken RightCurlyBrace
                IfElseStmt {condition = cond; trueBody = t; falseBody = f}
            | {kind = ElIf} ->
                let f = parseIfElseStmt reader
                IfElseStmt {condition = cond; trueBody = t; falseBody = [f]}
            | t -> failwithf "need a elif/else here but got a %A" t

        let parseWhileStmt (reader: TokenReader) = 
            reader.EatToken While
            reader.EatToken LeftParenthesis
            let c = parseExp reader
            reader.EatToken RightParenthesis
            reader.EatToken LeftCurlyBrace
            let sl = parseStmtList reader
            reader.EatToken RightCurlyBrace
            WhileStmt {condition = c; body = sl}

        let parseForStmt (reader: TokenReader) = 
            let initp (r: TokenReader) = parseStmt r
            let condp (r: TokenReader) = 
                let e = parseExp r
                r.EatToken Semicolon
                e
            let incp (r: TokenReader) =  // an assign statement without semicolon
                let n = match r.Read() with
                        | {kind = Identifer; value = Some str} -> str
                        | t -> failwithf "need a identifer here but got a %A" t
                r.EatToken Assign
                let e = parseExp r
                AssignStmt {name = n; value = e}
            reader.EatToken For
            reader.EatToken LeftParenthesis
            let init = initp reader
            let cond = condp reader
            let inc = incp reader
            reader.EatToken RightParenthesis
            reader.EatToken LeftCurlyBrace
            let sl = parseStmtList reader
            reader.EatToken RightCurlyBrace
            ForStmt {init = init; condition = cond; inc = inc; body = sl}

        let parseBreakStmt (reader: TokenReader) =
            reader.EatToken Break
            reader.EatToken Semicolon
            BreakStmt

        let parseContinueStmt (reader: TokenReader) =
            reader.EatToken Continue
            reader.EatToken Semicolon
            ContinueStmt

        let parseFunDefStmt (reader: TokenReader) = 
            reader.EatToken Function
            let n = match reader.Read() with
                    | {kind = Identifer; value = Some str} -> str
                    | t -> failwithf "need a identifer here but got a %A" t
            reader.EatToken LeftParenthesis
            let args = parseNameList reader []
            reader.EatToken RightParenthesis
            reader.EatToken LeftCurlyBrace
            let body = parseStmtList reader
            reader.EatToken RightCurlyBrace
            FuncDefStmt {name = n; args = args; body = body}

        let parseStmt (reader: TokenReader): Statement =
            let {kind = k; value = _; position = _} = reader.Peek()
            let f = match k with
                    | Global -> parseGlobalRefStmt
                    | Return -> parseReturnStmt
                    | If -> parseIfElseStmt
                    | While -> parseWhileStmt
                    | For -> parseForStmt
                    | Break -> parseForStmt
                    | Continue -> parseContinueStmt
                    | Function -> parseFunDefStmt
                    | Identifer ->
                        match reader.PeekNext() with
                        | {kind = Assign} -> parseAssignStmt
                        | _ -> parseRawExpStmt
                    | _ -> parseRawExpStmt
                    // | t -> failwithf "cannot parse a statement starts with %A" t
            f reader

        let parseStmtList (reader: TokenReader) = 
            let rec inner (r: TokenReader) (l: Statement list) =
                match r.Peek() with
                | {kind = RightCurlyBrace} | {kind = EOF} -> l
                | _ -> l @ [parseStmt reader] |> inner r
            inner reader []

    // just use statement list parser here 
    let private parseStatement = StmtParser.parseStmt
    let private parseStatementList = StmtParser.parseStmtList

    [<CompiledName("Parse")>]
    let parse (path: string) =
        let reader = tokenize path |> TokenReader
        parseStatementList reader
