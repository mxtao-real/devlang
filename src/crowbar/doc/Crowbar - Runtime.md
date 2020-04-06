
# Crowbar Runtime

```fsharp
    type RtType = 
        | Unit = 0
        | Int = 1
        | Double = 2
        | String = 3
        | Pointer = 4

    // |    8 byte    |  4 byte  |      |   4 byte   |   8 byte  |       |
    // | used mem len | name len | name | value type | value len | value |
    type ReadOnlyMemoryEntry(ptr: voidptr) =
        let len = NativePtr.ofVoidPtr<uint64> ptr |> NativePtr.read |> int
        let span = new ReadOnlySpan<byte>(ptr, len)
        let nameLen =  BitConverter.ToUInt32(span.Slice(8, 4)) |> int
        let name = System.Text.Encoding.ASCII.GetString (span.Slice(12, nameLen))
        let ``type`` = BitConverter.ToInt32(span.Slice(8+4+nameLen, 4))
        let valueLen = BitConverter.ToUInt64(span.Slice(8+4+nameLen+4, 8)) |> int
        let valueSpan = span.Slice(8+4+nameLen+4+8, valueLen)
        member _.Ptr = ptr
        member _.Name = name
        // todo: FIX HERE !!!
        member _.Value: obj = 
            match ``type`` with
            | 1 -> BitConverter.ToInt32(valueSpan) |> box
            | 2 -> BitConverter.ToDouble(valueSpan) |> box 
            | _ -> null

```

以上运行时对象设计存在问题，这是个无类型语言，需要将类型信息处理掉。