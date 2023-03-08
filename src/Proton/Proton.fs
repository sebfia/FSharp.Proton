module Proton
    open System
    open System.IO
    open System.Reflection
    open ProtoBuf
    open FSharp.Reflection
    open System.ComponentModel

    [<AttributeUsage(AttributeTargets.Class)>]
    type TypeMapAttribute(identifier: int) =
        inherit Attribute()
        member val Identifier = identifier
    type Proto = 
        | Reader of ProtoReader
        | Writer of ProtoWriter
    type Proto<'a> = Proto -> ProtoResult<'a>*Proto
    and
        ProtoResult<'a> = Result<'a, string>

    [<AutoOpen>]
    module Functional =
        let inline init (a: 'a) : Proto<'a> =
            fun proto ->
                Ok a, proto
        let inline bind (l: Proto<'a>) (f: 'a -> Proto<'b>) =
            fun proto ->
                match l proto with
                | Ok a, proto -> (f a) proto
                | Error e, proto -> Error e, proto

        let inline testb (f: Proto< 'a -> 'b>) (m: Proto<'a>) : Proto<'b> =
            bind f (fun x ->
                bind m (fun t ->
                    init(x t)))
        let inline apply (f: Proto<'a -> 'b>) (m: Proto<'a>) : Proto<'b> =
            bind f (fun f' ->
                bind m (fun m' ->
                    init (f' m')))
        let inline map (f: 'a -> 'b) (m: Proto<'a>) : Proto<'b> =
            bind m (fun m' ->
                init (f m'))
        let inline map2 (f: 'a -> 'b -> 'c) (m1: Proto<'a>) (m2: Proto<'b>) : Proto<'c> =
            let n1 = init f
            let n2 = apply n1 m1
            let n3 = apply n2 m2
            apply (apply (init f) m1) m2

    [<AutoOpen>]
    module Operators =
        let inline ( *>) m1 m2 = map2 (fun _ x -> x) m1 m2
    [<AutoOpen>]
    module Builder =
        type ProtoBuilder() = 
            member __.Bind (m1, m2) : Proto<_> =
                bind m1 m2

            member __.Combine (m1, m2) : Proto<_> =
                bind m1 (fun () -> m2)

            member __.Delay (f) : Proto<_> =
                bind (init ()) f

            member __.Return (x) : Proto<_> =
                init x

            member __.ReturnFrom (f) : Proto<_> = f

            member __.Zero () : Proto<_> =
                init ()
    let proto =
        ProtoBuilder ()

    module Proto = 
        let inline private writeSignedVarIntHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.SignedVarint, writer)
        let inline private writeVarIntHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.Varint, writer)
        let inline private writeFixed32Header field writer = ProtoWriter.WriteFieldHeader(field, WireType.Fixed32, writer)
        let inline private writeFixed64Header field writer = ProtoWriter.WriteFieldHeader(field, WireType.Fixed64, writer)
        let inline private writeStringHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.String, writer)
        let inline private writeInt16 i w = ProtoWriter.WriteInt16(i,w)
        let inline private writeInt32 i w = ProtoWriter.WriteInt32(i,w)
        let inline private writeInt64 i w = ProtoWriter.WriteInt64(i,w)
        let inline private writeDouble i w = ProtoWriter.WriteDouble(i,w)
        let inline private writeSingle i w = ProtoWriter.WriteSingle(i,w)
        let inline private writeBool i w = ProtoWriter.WriteBoolean(i,w)
        let inline private writeString i w = ProtoWriter.WriteString(i,w)
        let inline private writeDecimal i (w: ProtoWriter) = BclHelpers.WriteDecimal(i,w)
        let inline private writeDateTime i (w: ProtoWriter) = BclHelpers.WriteDateTimeWithKind(i,w)
        let inline private writeTimeSpan i (w: ProtoWriter) = BclHelpers.WriteTimeSpan(i,w)
        let inline private writeGuid i (w: ProtoWriter) = BclHelpers.WriteGuid(i,w)
        let inline private writeByte i w = ProtoWriter.WriteByte(i,w)
        let inline private writeBytes i w = ProtoWriter.WriteBytes(i,w);
        let inline private withReader<'T> i (f: ProtoReader -> 'T) =
            fun proto ->
                match proto with
                | Writer _ -> (Error "Can not read from a writer!"),proto
                | Reader r ->
                    match r.TryReadFieldHeader(i) with
                    | false -> Error("This field header does not exist!"),proto
                    | true -> try (f r |> Ok),proto with ex -> (Error $"An exeption was thrown while trying to decode a value.{Environment.NewLine}{ex}"),proto
        let inline readInt16 i : Proto<int16> = withReader i (fun r -> r.ReadInt16())
        let inline readInt i : Proto<int> = withReader i (fun r -> r.ReadInt32())
        let inline readInt64 i : Proto<int64> = withReader i (fun r -> r.ReadInt64())
        let inline readString i : Proto<string> = withReader i (fun r -> r.ReadString())
        let inline readFloat i : Proto<float> = withReader i (fun r -> r.ReadDouble())
        let inline readSingle i : Proto<float32> = withReader i (fun r -> r.ReadSingle())
        let inline readBool i : Proto<bool> = withReader i (fun r -> r.ReadBoolean())
        let inline readDecimal i : Proto<decimal> = withReader i (fun r -> BclHelpers.ReadDecimal(r))
        let inline readDateTime i : Proto<DateTime> = withReader i (fun r -> BclHelpers.ReadDateTime(r))
        let inline readTimeSpan i : Proto<TimeSpan> = withReader i (fun r -> BclHelpers.ReadTimeSpan(r))
        let inline readGuid i : Proto<Guid> = withReader i (fun r -> BclHelpers.ReadGuid(r))
        let inline readByte i : Proto<byte> = withReader i (fun r -> r.ReadByte())
        let inline readBytes i : Proto<byte array> = withReader i (fun r -> ProtoReader.AppendBytes([||], r))

        let inline error<'a> s : Proto<'a> = 
            fun (proto: Proto) ->
                (Error s),proto

        let bytesToHex bytes = 
            bytes 
            |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
            |> String.concat String.Empty

        let inline serializeInFrame<'T> (f:'T -> Proto -> ProtoResult<unit>*Proto) a =
            let ms = new MemoryStream()
            let w = ProtoWriter.Create(ms,null,null)
            f a (Writer w) |> ignore
            w.Close()
            let result = ms.ToArray()
            w :> IDisposable |> fun d -> d.Dispose()
            ms.Dispose()
            result
        let inline serialize a =
            let mutable map = Map.empty<string,MethodInfo>
            let getM()  =
                let getTypeForReflection (t: Type) =
                    if FSharpType.IsUnion t then 
                        match t.DeclaringType.DeclaringType with
                        | null -> t
                        | _ -> t.DeclaringType
                    else t
                let t = a.GetType()
                match Map.tryFind t.FullName map with
                | None ->
                    let m = getTypeForReflection (t) |> fun rt -> rt.GetTypeInfo() |> fun ti -> ti.GetMethod("Encoder", Reflection.BindingFlags.Public|||Reflection.BindingFlags.Static)
                    map <- Map.add t.FullName m map
                    m
                | Some m -> m
            let m = getM()
            let f = m.Invoke(null, [|a |> box|]):?> Proto<unit>
            let ms = new IO.MemoryStream()
            let writer = ProtoWriter.Create(ms,null,null)
            let proto = Writer writer
            match f proto with
            | Error e, _ -> 
                writer :> IDisposable |> fun d -> d.Dispose()
                ms.Dispose()
                failwith e
            | _ ->
                writer.Close()
                let buf = ms.ToArray()
                writer :> IDisposable |> fun d -> d.Dispose()
                ms.Dispose()
                buf
        let inline deserializeInFrame<'T> (f: Proto -> ProtoResult<'T>*Proto) (buffer: byte array) : ProtoResult<'T> =
            use ms = new MemoryStream(buffer)
            use r = ProtoReader.Create(ms, null, null)
            let proto = Reader r
            match f proto with
            | (Ok t),_ -> Ok t
            | (Error e),_ -> Error e

        // get buffer for resp. field
        // initialize new proto reader with buffer
        // read header and integer at field 1 (varint) -> length of array
        // zero-create an array with length
        // from field 2 until field 1 + length do
            // advance reader by reading header (string)
            // read bytes at field
            // deserialze bytes in frame to object using the decoder that was passed to mother method as argument
            // append deserialized object to array at position field - 2
        // return the array 
        // NOTE: This could be improved by using stack memory instead of streams and byte arrays
        let inline deserializeArray<'T> (f: Proto -> ProtoResult<'T>*Proto) (buffer: byte array) : ProtoResult<'T array> =
            
            use ms = new MemoryStream(buffer)
            use r = ProtoReader.Create(ms, null, null)

            if r.TryReadFieldHeader(1) |> not then Error "No field header found for array length!" else
            let length = r.ReadInt32()
            let result = Array.zeroCreate<'T> length

            for i = 2 to (length + 1) do
                if r.TryReadFieldHeader(i) then
                    let buf = ProtoReader.AppendBytes(Array.empty, r)
                    match deserializeInFrame f buf with
                    | Ok o -> result[i - 2] <- o
                    | Error e -> failwith $"Unable to read object from array buffer at position {i - 2} Reson: {e}" // there is room for improvement here
            Ok result

        let inline serializeArray<'T> (f: 'T -> Proto<unit>) (values: 'T array) : ProtoResult<byte array> =

            use ms = new MemoryStream()
            use w = ProtoWriter.Create(ms, null, null)
        
            ProtoWriter.WriteFieldHeader(1, WireType.Varint, w)
            ProtoWriter.WriteInt32(values.Length, w)

            for i = 0 to (values.Length - 1) do
                let buf = serializeInFrame f values[i]
                ProtoWriter.WriteFieldHeader((i + 2), WireType.String, w)
                ProtoWriter.WriteBytes(buf, w)
            
            w.Close()

            Ok (ms.ToArray())

        let inline serializeOption<'T> (f: 'T -> Proto<unit>) value : ProtoResult<byte array> =

            use ms = new MemoryStream()
            use w = ProtoWriter.Create(ms, null, null)
        
            ProtoWriter.WriteFieldHeader(1, WireType.Varint, w)
            match value with
            | None -> ProtoWriter.WriteByte(0uy, w)
            | Some x ->
                ProtoWriter.WriteByte(1uy, w)
                let buf = serializeInFrame f x
                ProtoWriter.WriteFieldHeader(2, WireType.String, w)
                ProtoWriter.WriteBytes(buf, w)
            
            w.Close()

            Ok (ms.ToArray())

        let inline deserializeOption<'T> (f: Proto -> ProtoResult<'T>*Proto) (buffer: byte array) : ProtoResult<'T option> =
            
            use ms = new MemoryStream(buffer)
            use r = ProtoReader.Create(ms, null, null)

            if r.TryReadFieldHeader(1) |> not then Error "No field header option discriminator!" else
            
            match r.ReadByte() with
            | 0uy -> None |> Ok
            | 1uy ->  
                if r.TryReadFieldHeader(2) |> not then Error "No field header for Some option found!" else
                let buf = ProtoReader.AppendBytes(Array.empty, r)
                match deserializeInFrame f buf with
                | Ok x -> Some x |> Ok
                | Error e -> Error e
            | _ -> Error "Invalid data for Option type!"

        [<RequireQualifiedAccess>]
        type Decode =
            static member inline int16 i = readInt16 i
            static member inline int32 i = readInt i
            static member inline int64 i = readInt64 i
            static member inline float i = readFloat i
            static member inline float32 i = readSingle i
            static member inline boolean i = readBool i
            static member inline byte i = readByte i
            static member inline bytes i = readBytes i
            static member inline string i = readString i
            static member inline decimal i = readDecimal i
            static member inline guid (i: int) = withReader i (fun r ->
                // let state = ProtoReader.State.Create(new MemoryStream(), Meta.RuntimeTypeModel.Default)
                ProtoReader.AppendBytes([||], r) |> Guid
            )
            static member inline dateTime i = readDateTime i
            static member inline timeSpan i = readTimeSpan i
            static member inline dateTimeOffset (i: int) : Proto<DateTimeOffset> = (fun p ->
                match p with
                | Writer _ -> failwith "Can not use a writer for reading!"
                | Reader r -> 
                    match r.TryReadFieldHeader(i) with
                    | true ->
                        let bytes = ProtoReader.AppendBytes(Array.empty, r)
                        let frameResult = 
                            deserializeInFrame (proto {
                                let! (date: DateTime) = Decode.dateTime 1
                                let! offset = Decode.timeSpan 2
                                return DateTimeOffset(date, offset)
                            }) bytes
                        frameResult,p
                    | _ -> failwith "Unable to read field header for DateTimeOffset!"
            )

            static member inline array<'a> decoder i : Proto<'a array> = (fun p ->
                match p with
                | Writer _ -> failwith "Can not use a writer for reading!"
                | Reader r -> 
                    match r.TryReadFieldHeader(i) with
                    | true ->
                        let bytes = ProtoReader.AppendBytes(Array.empty, r)
                        (deserializeArray decoder bytes),p
                    | _ -> (Error "Unable to read field header for array"),p
            )
            static member inline option<'a> decoder i : Proto<'a option> = (fun p ->
                match p with
                | Writer _ -> failwith "Can not use a writer for reading!"
                | Reader r -> 
                    match r.TryReadFieldHeader(i) with
                    | true ->
                        let bytes = ProtoReader.AppendBytes(Array.empty, r)
                        (deserializeOption decoder bytes),p
                    | _ -> (Error "Unable to read field header for Option."),p
            )
            static member inline object<'a> decoder i : Proto<'a> = (fun p ->
                match p with
                | Writer _ -> failwith "Can not use a writer for reading!"
                | Reader r -> 
                    match r.TryReadFieldHeader(i) with
                    | true ->
                        let bytes = ProtoReader.AppendBytes(Array.empty, r)
                        (deserializeInFrame decoder bytes),p
                    | _ -> (Error "Unable to read field header for object"),p
            )
            static member inline fromBytes decoder (bytes: byte array) = 
                use ms = new MemoryStream(bytes)
                use r = ProtoReader.Create(ms, null, null)
                let proto = Reader r
                match decoder proto with
                | (Ok t),_ -> t
                | (Error e),_ -> failwith e
        
        [<RequireQualifiedAccess>]
        type Encode =
            [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
            static member inline private encodeWithWriter f : Proto<unit> = (fun p ->
                match p with
                | Reader _ -> failwith "Can not write to a reader!"
                | Writer w -> match f w with | Ok () -> (Ok ()),p | Error (e: string) -> (Error e),p)
            [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
            static member inline private safeCage f =
                try
                    f()
                    Ok ()
                with ex -> Error $"An exeption was thrown while trying to encode a value.{Environment.NewLine}{ex}"
            static member inline int16 i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeVarIntHeader i w
                    writeInt16 value w
                )
            )
            static member inline int32 i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeVarIntHeader i w
                    writeInt32 value w
                )
            )
            static member inline int64 i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeVarIntHeader i w
                    writeInt64 value w
                )
            )
            static member inline float i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeFixed64Header i w
                    writeDouble value w
                )
            )
            static member inline float32 i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeFixed32Header i w
                    writeSingle value w
                )
            )
            static member inline boolean i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ -> 
                    writeVarIntHeader i w
                    writeBool value w
                )
            )
            static member inline byte i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ -> 
                    writeVarIntHeader i w
                    writeByte value w
                )
            )
            static member inline bytes i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeStringHeader i w
                    writeBytes value w
                )
            )
            static member inline string i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeStringHeader i w
                    writeString value w
                )
            )
            static member inline decimal i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeStringHeader i w
                    writeDecimal value w
                )
            )
            static member inline dateTime i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeStringHeader i w
                    writeDateTime value w
                )
            )
            static member inline timeSpan i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    writeStringHeader i w
                    writeTimeSpan value w
                )
            )
            static member inline guid i (value: Guid) : Proto<unit> = value.ToByteArray() |> Encode.bytes i
            static member inline dateTimeOffset i value : Proto<unit> = Encode.encodeWithWriter(fun w ->
                Encode.safeCage(fun _ ->
                    let bytes = serializeInFrame (fun (d: DateTimeOffset) -> Encode.dateTime 1 d.DateTime *> Encode.timeSpan 2 d.Offset) value
                    writeStringHeader i w
                    writeBytes bytes w
                )
            )
            static member inline array<'a> encoder i (values: 'a array) : Proto<unit> = Encode.encodeWithWriter(fun w ->
                match serializeArray encoder values with
                | Ok buffer ->
                    writeStringHeader i w
                    writeBytes buffer w
                    Ok ()
                | Error e -> Error e
            )
            static member inline option<'a> encoder i (value: 'a option) : Proto<unit> = Encode.encodeWithWriter(fun w ->
                match serializeOption encoder value with
                | Ok buffer ->
                    writeStringHeader i w
                    writeBytes buffer w
                    Ok ()
                | Error e -> Error e
            )
            static member inline object<'a> encoder i (value: 'a) : Proto<unit> = Encode.encodeWithWriter(fun w ->
                try
                    let buffer = serializeInFrame encoder value
                    writeStringHeader i w
                    writeBytes buffer w
                    Ok()
                with
                | ex -> Error $"Error trying to encode object of type {typeof<'a>}.{Environment.NewLine}{ex}"
            )

            static member inline toBytes (f) a =
                use ms = new MemoryStream()
                use w = ProtoWriter.Create(ms,null,null)
                f a (Writer w) |> ignore
                w.Close()
                let result = ms.ToArray()
                result

        
        let inline write<'T> i (value: 'T) : Proto<unit> =
            fun proto ->
                match proto with
                | Reader _ -> (Error "Can not write to a reader, dumbass!"),proto
                | Writer w ->
                    match box value with
                    | :? int16 as x ->
                        writeVarIntHeader i w
                        writeInt16 x w
                        (Ok ()),proto
                    | :? int as x ->
                        writeVarIntHeader i w
                        writeInt32 x w
                        (Ok ()),proto
                    | :? int64 as x ->
                        writeVarIntHeader i w
                        writeInt64 x w
                        (Ok ()),proto
                    | :? float as x ->
                        writeFixed64Header i w
                        writeDouble x w
                        (Ok ()),proto
                    | :? float32 as x ->
                        writeFixed32Header i w
                        writeSingle x w
                        (Ok ()),proto
                    | :? string as x ->
                        writeStringHeader i w
                        writeString x w
                        (Ok ()),proto
                    | :? bool as x ->
                        writeVarIntHeader i w
                        writeBool x w
                        (Ok ()),proto
                    | :? decimal as x ->
                        writeStringHeader i w
                        writeDecimal x w
                        (Ok ()),proto
                    | :? DateTime as x ->
                        writeStringHeader i w
                        writeDateTime x w
                        (Ok ()),proto
                    | :? TimeSpan as x ->
                        writeStringHeader i w
                        writeTimeSpan x w
                        (Ok ()),proto
                    | :? Guid as x ->
                        writeStringHeader i w
                        writeGuid x w
                        (Ok ()),proto
                    | :? byte as x ->
                        writeVarIntHeader i w
                        writeByte x w
                        (Ok ()),proto
                    | :? (byte array) as x ->
                        writeStringHeader i w
                        writeBytes x w
                        (Ok ()),proto
                    | x -> 
                        try
                            writeStringHeader i w
                            serialize value |> fun buf -> writeBytes buf w
                            (Ok()),proto
                        with ex -> (Error (sprintf "No writer defined for type %s!" (x.GetType().FullName)) ),proto
        
        
        let inline deserialize<'a> (buffer: byte array) =
            let mutable map = Map.empty<string,obj>
            let getF() =
                let t = typeof<'a>
                match Map.tryFind t.FullName map with
                | None ->
                    let m = t.GetTypeInfo() |> fun ti -> ti.GetMethod("Decoder", Reflection.BindingFlags.Public|||Reflection.BindingFlags.Static)
                    let o = m.Invoke(null, [|null|])
                    map <- Map.add t.FullName o map
                    o :?> Proto<'a>
                | Some o ->
                    o :?> Proto<'a>
            let f = getF()
            use ms = new IO.MemoryStream(buffer)
            use reader = ProtoReader.Create(ms, null, null)
            let proto = Reader reader
            match f proto with
            | Error e, _ -> failwith e
            | Ok a,_ -> a

        let inline private wrap<'u,'a> =
                let [|fSucc;fErr|] = FSharpType.GetUnionCases typeof<ProtoResult<'a>> |> Array.map FSharpValue.PreComputeUnionConstructor
                function
                | Ok (x: 'u) -> fSucc [|x |> box|] :?> ProtoResult<'a>
                | Error e -> fErr [|e |> box|] :?> ProtoResult<'a>
        let inline read<'T> i : Proto<'T> =
            let int16Name = typeof<int16>.FullName
            let int32Name = typeof<int>.FullName
            let int64Name = typeof<int64>.FullName
            let floatName = typeof<float>.FullName
            let float32Name = typeof<float32>.FullName
            let stringName = typeof<string>.FullName
            let boolName = typeof<bool>.FullName
            let decimalName = typeof<decimal>.FullName
            let dateTimeName = typeof<DateTime>.FullName
            let timeSpanName = typeof<TimeSpan>.FullName
            let guidName = typeof<Guid>.FullName
            let byteName = typeof<byte>.FullName
            let bytesName = typeof<byte array>.FullName
            fun proto ->
                match typeof<'T>.FullName with
                | x when x=int16Name -> readInt16 i proto |> fun (r,_) -> (r |> wrap<int16,'T>),proto
                | x when x=int32Name -> readInt i proto |> fun (r,_) -> (r |> wrap<int,'T>),proto
                | x when x=int64Name -> readInt64 i proto |> fun (r,_) -> (r |> wrap<int64,'T>),proto
                | x when x=floatName -> readFloat i proto |> fun (r,_) -> (r |> wrap<float,'T>),proto
                | x when x=float32Name -> readSingle i proto |> fun (r,_) -> (r |> wrap<float32,'T>),proto
                | x when x=stringName -> readString i proto |> fun (r,_) -> (r |> wrap<string,'T>),proto
                | x when x=boolName -> readBool i proto |> fun (r,_) -> (r |> wrap<bool,'T>),proto
                | x when x=decimalName -> readDecimal i proto |> fun (r,_) -> (r |> wrap<decimal,'T>),proto
                | x when x=dateTimeName -> readDateTime i proto |> fun (r,_) -> (r |> wrap<DateTime,'T>),proto
                | x when x=timeSpanName -> readTimeSpan i proto |> fun (r,_) -> (r |> wrap<TimeSpan,'T>),proto
                | x when x=guidName -> readGuid i proto |> fun (r,_) -> (r |> wrap<Guid,'T>),proto
                | x when x=byteName -> readByte i proto |> fun (r,_) -> (r |> wrap<byte,'T>),proto
                | x when x=bytesName -> readBytes i proto |> fun (r,_) -> (r |> wrap<byte array,'T>),proto
                | _ ->
                    try
                        match readBytes i proto with
                        | (Ok buf),_ -> 
                            let result = deserialize<'T> buf
                            (Ok result),proto
                        | (Error e),_ -> (Error e),proto
                    with _ -> (Error "No reader found for this type"),proto
        

        let inline readOption<'T> i : Proto<Option<'T>> =
            let [|fNone;fSome|] = FSharpType.GetUnionCases typeof<Option<'T>> |> Array.map FSharpValue.PreComputeUnionConstructor
            fun proto ->
                match readBytes i proto with
                | (Ok buf),_ ->
                    let ms = new MemoryStream(buf)
                    let r = ProtoReader.Create(ms, null, null)
                    let p = Reader r
                    match readBool 1 p with
                    | (Ok b),_ ->
                        match b with
                        | false -> fNone [||] :?> Option<'T> |> Ok |> fun v -> v,proto
                        | true ->
                            match readBytes 2 p with
                            | (Ok sub),_ ->
                                let opt = deserializeInFrame (read<'T> 1) sub |> fun t -> fSome [|t |> box|] :?> Option<'T>
                                (Ok opt),proto
                            | (Error e),_ -> (Error e),proto
                    | (Error e),_ -> (Error e),proto
                | (Error e),_ -> (Error e),proto
    [<RequireQualifiedAccess>]
    module Message =
        open ProtoBuf.Meta
        type private ITypeMap =
            abstract member AddNew: string -> int -> unit
            abstract member TryGet: int -> string option
        let inline private createTypeModel () =
            let typeModel = RuntimeTypeModel.Create("model")
            typeModel.CompileInPlace()
            typeModel
        let private serializeToMessage<'T> =
            let mutable typeMap = Map.empty<string,int>
            let tryGetTypeId (t: Type) =
                match t.GetTypeInfo() |> fun ti -> ti.GetCustomAttribute(typeof<TypeMapAttribute>) with
                | null -> None
                | (att: Attribute) -> att :?> TypeMapAttribute |> fun a ->  Some a.Identifier
            fun (a: 'T) ->
                let t = a.GetType()
                let tn = t |> fun t -> t.FullName
                match Map.tryFind tn typeMap with
                | Some i ->
                    (i,a |> Proto.serialize) |> Some
                | _ ->
                    match tryGetTypeId t with
                    | None -> None
                    | Some i ->
                        (i,a |> Proto.serialize) |> Some
        let private serializeMessage =
            let serializer = createTypeModel()
            fun (stream: Stream) (i,buf: byte array) ->
                try
                    serializer.SerializeWithLengthPrefix(stream, buf, typeof<byte array>, PrefixStyle.Base128, i)
                with
                | ex -> 
                    printfn "%A" ex
                    raise ex
        let private appendMessage (stream: Stream) blob =
            if not stream.CanSeek then
                failwith "Stream can not seek and therefore will never find its own end!"
            else
                stream.Seek(0L, SeekOrigin.End) |> ignore
                serializeMessage stream blob
        let getAll<'T> =
            let mutable typeMap = Map.empty<string,int>
            let serializer = createTypeModel ()
            let tryGetTypeId (t: Type) =
                match t.GetTypeInfo() |> fun ti -> ti.GetCustomAttribute(typeof<TypeMapAttribute>) with
                | null -> None
                | (att: Attribute) -> att :?> TypeMapAttribute |> fun a ->  Some a.Identifier
            fun (stream: Stream) ->
                let tn = typeof<'T>.FullName 
                match Map.tryFind tn typeMap with
                | Some i ->
                    serializer.DeserializeItems<byte array>(stream, PrefixStyle.Base128, i)
                    |> Seq.map Proto.deserialize<'T>
                    |> Seq.cache
                | _ -> 
                    match tryGetTypeId typeof<'T> with
                    | None -> failwith "Unable to deserialize this type of data"
                    | Some i -> 
                        serializer.DeserializeItems<byte array>(stream, PrefixStyle.Base128, i)
                        |> Seq.map Proto.deserialize<'T>
                        |> Seq.cache
        let tryGetSingle<'T> (buffer: byte array) =
            using (new MemoryStream(buffer)) (fun ms -> 
                ms.Seek(0L, SeekOrigin.Begin) |> ignore
                getAll<'T> ms |> Seq.tryHead
            )
        let append stream data =
            match data |> serializeToMessage with
            | None -> failwith "Unable to serialize data to message."
            | Some b -> b |> appendMessage stream
        let tryWriteSingle data =
            match data |> serializeToMessage with
            | None -> None
            | Some b ->
                using (new MemoryStream()) (fun ms -> 
                    b |> appendMessage ms
                    ms.ToArray() |> Some
                )
        let appendMultiple<'T> (stream: Stream) (data: 'T seq) =
            let msgs = data |> Seq.choose serializeToMessage |> Seq.cache
            match (msgs |> Seq.length) = (data |> Seq.length) with
            | false -> failwith "Unable to serialize all data to message."
            | _ ->
                let ms = new MemoryStream()
                msgs |> Seq.iter (appendMessage ms)
                ms.Seek(0L, SeekOrigin.Begin) |> ignore
                if not stream.CanSeek then
                    failwith "Stream can not seek and therefore will never find its own end!"
                else
                    stream.Seek(0L, SeekOrigin.End) |> ignore
                    ms.CopyTo(stream)
                    ms.Flush()
                    ms.Dispose()