module Proton
    open System
    open System.IO
    open System.Reflection
    open ProtoBuf
    open FSharp.Reflection

    [<AttributeUsage(AttributeTargets.Class)>]
    type TypeMapAttribute(identifier: int) =
        inherit Attribute()
        member val Identifier = identifier
    type Proto = 
        | Reader of ProtoReader
        | Writer of ProtoWriter
    type Proto<'a> = Proto -> ProtoResult<'a>*Proto
    and
        ProtoResult<'a> =
            | Value of 'a
            | Error of string
    [<AutoOpen>]
    module Functional =
        let inline init (a: 'a) : Proto<'a> =
            fun proto ->
                Value a, proto
        let inline bind (l: Proto<'a>) (f: 'a -> Proto<'b>) =
            fun proto ->
                match l proto with
                | Value a, proto -> (f a) proto
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
        let inline private writeVariantHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.Variant, writer)
        let inline private writeFixed32Header field writer = ProtoWriter.WriteFieldHeader(field, WireType.Fixed32, writer)
        let inline private writeFixed64Header field writer = ProtoWriter.WriteFieldHeader(field, WireType.Fixed64, writer)
        let inline private writeStringHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.String, writer)
        let inline private writeStartGroupHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.StartGroup, writer)
        let inline private writeEndGroupHeader field writer = ProtoWriter.WriteFieldHeader(field, WireType.EndGroup, writer)
        let inline private writeInt16 i w = ProtoWriter.WriteInt16(i,w)
        let inline private writeInt32 i w = ProtoWriter.WriteInt32(i,w)
        let inline private writeInt64 i w = ProtoWriter.WriteInt64(i,w)
        let inline private writeDouble i w = ProtoWriter.WriteDouble(i,w)
        let inline private writeSingle i w = ProtoWriter.WriteSingle(i,w)
        let inline private writeBool i w = ProtoWriter.WriteBoolean(i,w)
        let inline private writeString i w = ProtoWriter.WriteString(i,w)
        let inline private writeDecimal i w = BclHelpers.WriteDecimal(i,w)
        let inline private writeDateTime i w = BclHelpers.WriteDateTimeWithKind(i,w)
        let inline private writeTimeSpan i w = BclHelpers.WriteTimeSpan(i,w)
        let inline private writeGuid i w = BclHelpers.WriteGuid(i,w)
        let inline private writeByte i w = ProtoWriter.WriteByte(i,w)
        let inline private writeBytes i w = ProtoWriter.WriteBytes(i,w);
        let inline private writeOptionalInt16 i w = BclHelpers.WriteNetObject
        let inline private withReader<'T> i (f: ProtoReader -> 'T) =
            fun proto ->
                match proto with
                | Writer _ -> (Error "Can not read from a writer, dumbass!"),proto
                | Reader r ->
                    match r.TryReadFieldHeader(i) with
                    | false -> Error("This field header does not exist, fucking idiot!"),proto
                    | true -> (f r |> Value),proto
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
            |> String.concat System.String.Empty

        let inline serializeInFrame<'T> (f:'T -> Proto -> ProtoResult<unit>*Proto) a =
            let ms = new MemoryStream()
            let w = new ProtoWriter(ms,null,null)
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
                    let m = getTypeForReflection (t) |> fun rt -> rt.GetTypeInfo() |> fun ti -> ti.GetMethod("ToProto", Reflection.BindingFlags.Public|||Reflection.BindingFlags.Static)
                    map <- Map.add t.FullName m map
                    m
                | Some m -> m
            let m = getM()
            let f = m.Invoke(null, [|a |> box|]):?> Proto<unit>
            let ms = new IO.MemoryStream()
            let writer = new ProtoWriter(ms,null,null)
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
        let inline write<'T> i (value: 'T) : Proto<unit> =
            fun proto ->
                match proto with
                | Reader _ -> (Error "Can not write to a reader, dumbass!"),proto
                | Writer w ->
                    match box value with
                    | :? int16 as x ->
                        writeVariantHeader i w
                        writeInt16 x w
                        (Value ()),proto
                    | :? int as x ->
                        writeVariantHeader i w
                        writeInt32 x w
                        (Value ()),proto
                    | :? int64 as x ->
                        writeVariantHeader i w
                        writeInt64 x w
                        (Value ()),proto
                    | :? float as x ->
                        writeFixed64Header i w
                        writeDouble x w
                        (Value ()),proto
                    | :? float32 as x ->
                        writeFixed32Header i w
                        writeSingle x w
                        (Value ()),proto
                    | :? string as x ->
                        writeStringHeader i w
                        writeString x w
                        (Value ()),proto
                    | :? bool as x ->
                        writeVariantHeader i w
                        writeBool x w
                        (Value ()),proto
                    | :? decimal as x ->
                        writeStringHeader i w
                        writeDecimal x w
                        (Value ()),proto
                    | :? DateTime as x ->
                        writeStringHeader i w
                        writeDateTime x w
                        (Value ()),proto
                    | :? TimeSpan as x ->
                        writeStringHeader i w
                        writeTimeSpan x w
                        (Value ()),proto
                    | :? Guid as x ->
                        writeStringHeader i w
                        writeGuid x w
                        (Value ()),proto
                    | :? byte as x ->
                        writeVariantHeader i w
                        writeByte x w
                        (Value ()),proto
                    | :? (byte array) as x ->
                        writeStringHeader i w
                        writeBytes x w
                        (Value ()),proto
                    | x -> 
                        try
                            writeStringHeader i w
                            serialize value |> fun buf -> writeBytes buf w
                            (Value()),proto
                        with ex -> (Error (sprintf "No writer defined for type %s!" (x.GetType().FullName)) ),proto
        let inline writeOption<'T> i (value: Option<'T>) : Proto<unit> =
            fun proto ->
                match proto with
                | Reader _ -> (Error "Can not write to a reader, dumbass!"),proto
                | Writer topWriter ->
                    writeStringHeader i topWriter
                    let ms = new MemoryStream()
                    let w = new ProtoWriter(ms, null, null)
                    match value with
                    | None -> 
                        writeVariantHeader 1 w
                        writeBool false w
                    | Some a ->
                        writeVariantHeader 1 w
                        writeBool true w
                        writeStringHeader 2 w
                        serializeInFrame (write 1) a |> fun buf -> writeBytes buf w
                    w.Close()
                    writeBytes (ms.ToArray()) topWriter
                    w :> IDisposable |> fun d -> d.Dispose()
                    ms.Dispose()
                    (Value ()),proto
        
        let inline deserialize<'a> (buffer: byte array) =
            let mutable map = Map.empty<string,obj>
            let getF() =
                let t = typeof<'a>
                match Map.tryFind t.FullName map with
                | None ->
                    let m = t.GetTypeInfo() |> fun ti -> ti.GetMethod("FromProto", Reflection.BindingFlags.Public|||Reflection.BindingFlags.Static)
                    let o = m.Invoke(null, [|null|])
                    map <- Map.add t.FullName o map
                    o :?> Proto<'a>
                | Some o ->
                    o :?> Proto<'a>
            let f = getF()
            let ms = new IO.MemoryStream(buffer)
            let reader = new ProtoReader(ms, null, null)
            let proto = Reader reader
            match f proto with
            | Error e, _ -> failwith e
            | Value a,_ ->
                reader.Dispose()
                ms.Dispose()
                a
        let inline private wrap<'u,'a> =
                let [|fSucc;fErr|] = FSharpType.GetUnionCases typeof<ProtoResult<'a>> |> Array.map FSharpValue.PreComputeUnionConstructor
                function
                | Value (x: 'u) -> fSucc [|x |> box|] :?> ProtoResult<'a>
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
                        | (Value buf),_ -> 
                            let result = deserialize<'T> buf
                            (Value result),proto
                        | (Error e),_ -> (Error e),proto
                    with _ -> (Error "No reader found for this type"),proto
        let inline deserializeInFrame<'T> (f: Proto -> ProtoResult<'T>*Proto) (buffer: byte array) : 'T =
            let ms = new MemoryStream(buffer)
            let r = new ProtoReader(ms, null, null)
            let proto = Reader r
            match f proto with
            | (Value t),_ -> 
                r.Dispose()
                ms.Dispose()
                t
            | (Error e),_ -> 
                r.Dispose()
                ms.Dispose()
                failwith e

        let inline readOption<'T> i : Proto<Option<'T>> =
            let [|fNone;fSome|] = FSharpType.GetUnionCases typeof<Option<'T>> |> Array.map FSharpValue.PreComputeUnionConstructor
            fun proto ->
                match readBytes i proto with
                | (Value buf),_ ->
                    let ms = new MemoryStream(buf)
                    let r = new ProtoReader(ms, null, null)
                    let p = Reader r
                    match readBool 1 p with
                    | (Value b),_ ->
                        match b with
                        | false -> fNone [||] :?> Option<'T> |> Value |> fun v -> v,proto
                        | true ->
                            match readBytes 2 p with
                            | (Value sub),_ ->
                                let opt = deserializeInFrame (read<'T> 1) sub |> fun t -> fSome [|t |> box|] :?> Option<'T>
                                (Value opt),proto
                            | (Error e),_ -> (Error e),proto
                    | (Error e),_ -> (Error e),proto
                | (Error e),_ -> (Error e),proto
    [<RequireQualifiedAccess>]
    module Message =
        open ProtoBuf.Meta
        let inline private createTypeModel () =
            let typeModel = TypeModel.Create()
            typeModel.CompileInPlace()
            typeModel
        let private serializeToMessage =
            let mutable typeMap = Map.empty<string,int>
            let tryGetTypeId (t: Type) =
                match t.GetTypeInfo() |> fun ti -> ti.GetCustomAttribute(typeof<TypeMapAttribute>) with
                | null -> None
                | (att: Attribute) -> att :?> TypeMapAttribute |> fun a ->  Some a.Identifier
            fun a ->
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
        let tryGetSingle<'T> =
            let mutable typeMap = Map.empty<string,int>
            let serializer = createTypeModel ()
            let tryGetTypeId (t: Type) =
                match t.GetTypeInfo() |> fun ti -> ti.GetCustomAttribute(typeof<TypeMapAttribute>) with
                | null -> None
                | (att: Attribute) -> att :?> TypeMapAttribute |> fun a ->  Some a.Identifier
            fun (buffer: byte array) ->
                using (new MemoryStream(buffer)) (fun ms ->
                    let tn = typeof<'T>.FullName 
                    match Map.tryFind tn typeMap with
                    | Some i ->
                        serializer.DeserializeItems<byte array>(ms, PrefixStyle.Base128, i)
                        |> Seq.map Proto.deserialize<'T>
                        |> Seq.tryHead
                    | _ -> 
                        match tryGetTypeId typeof<'T> with
                        | None -> failwith "Unable to deserialize this type of data"
                        | Some i -> 
                            serializer.DeserializeItems<byte array>(ms, PrefixStyle.Base128, i)
                            |> Seq.map Proto.deserialize<'T>
                            |> Seq.tryHead
                )
        let append<'T> stream (data: 'T) =
            match data |> serializeToMessage with
            | None -> failwith "Unable to serialize data to message."
            | Some b -> b |> appendMessage stream
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