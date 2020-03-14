
open System

/// <summary>entry point</summary>
/// <param name="argv">arguments</param>
/// <returns>exit code</returns>
[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    // let pretty t =
    //     let {kind = k; value = v; position = {line = l; column = c; file = _}} = t
    //     match v with
    //     | Some s -> printfn "%2d:%2d -> [%A] \"%s\"" l c k s
    //     | None -> printfn "%2d:%2d -> [%A]" l c k
    // let path = "resource/tokenizer-test.crb"
    // tokenize path |> Seq.iter pretty

    let path = "resource/parser-test.crb"
    let parse = Crowbar.Compiler.Parser.parse
    parse path |> List.iter (fun s -> printfn "%A" s)

    0 // return an integer exit code
