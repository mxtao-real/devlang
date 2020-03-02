
open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

// https://docs.microsoft.com/en-us/dotnet/fsharp/whats-new/fsharp-45

#nowarn "9"

let opSpan (span: Span<byte>) = 
    let s = Span<byte>()
    span.CopyTo(s)
    s

let hglobal = Marshal.AllocHGlobal(1024)

let np0 = hglobal.ToPointer() |> NativePtr.ofVoidPtr<byte>
let np = hglobal |> NativePtr.ofNativeInt<byte>
let arr = Array.zeroCreate<int>(10)
let si = NativePtr.stackalloc<int> 1
let sb = si |> NativePtr.toVoidPtr |> NativePtr.ofVoidPtr<byte>

// must free memory here!
Marshal.FreeHGlobal(hglobal)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
