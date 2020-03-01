
open Calculator

[<EntryPoint>]
let main argv =

    let tokenizer = Tokenizer.Tokenizer "1+2*3/4-5*6+7\n"
    printfn "%A" tokenizer.Tokens
    let parser = Parser.Parser "1+2*3/4-5*-(6+7) "
    parser.Parse() |> printfn "%f"

    0
