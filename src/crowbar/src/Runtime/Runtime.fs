// this module for runtime impl

namespace Crowbar.Runtime

// open System
// open System.Runtime.InteropServices
// open Microsoft.FSharp.NativeInterop

// #nowarn "9" // disable warnings for using NativePtr module

// // https://docs.microsoft.com/en-us/dotnet/fsharp/whats-new/fsharp-45

// let operatingSpan (span: Span<byte>) = 
//     let s = Span<byte>()
//     span.CopyTo(s)
//     // ....
//     ()

// let ``some operatings`` () = 
//     let hglobal = Marshal.AllocHGlobal(1024)
//     let np0 = hglobal.ToPointer() |> NativePtr.ofVoidPtr<byte>
//     let np = hglobal |> NativePtr.ofNativeInt<byte>
//     let arr = Array.zeroCreate<int>(10)
//     let si = NativePtr.stackalloc<int> 1
//     let sb = si |> NativePtr.toVoidPtr |> NativePtr.ofVoidPtr<byte>
//     // must free memory here!
//     Marshal.FreeHGlobal(hglobal)
//     ()
