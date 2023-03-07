#r "nuget: protobuf-net"
#load "Proton.fs"

open System
open System.IO
open ProtoBuf

let ms = new MemoryStream()
let writer = ProtoWriter.Create(ms, null, null)
(writer :> IDisposable).Dispose()
ms.Dispose()
let reader = new ProtoReader(ms, null, null)
reader.TryReadFieldHeader(1)
reader.ReadFieldHeader()
reader.ReadString()

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



ProtoWriter.WriteFieldHeader(1, WireType.Variant, writer)
ProtoWriter.WriteByte(3uy, writer)
let optBuf = [|8uy; 1uy; 18uy; 7uy; 10uy; 5uy; 65uy; 114uy; 115uy; 99uy; 104uy|]
let buffer = [|10uy; 5uy; 65uy; 114uy; 115uy; 99uy; 104uy|]

ms.Write(buffer, 0, buffer.Length)
ms.Position
ms.Seek(0L, SeekOrigin.Begin)
reader.Dispose()
ms.Dispose()

open Microsoft.FSharp.Reflection


let opt = Some 3
let n = None
let t = opt.GetType()
FSharpType.IsUnion t
FSharpType.GetUnionCases t
(typeof<Microsoft.FSharp.Core.Option<_>>) |> fun t -> t.GetGenericTypeDefinition() = t.GetGenericTypeDefinition()
FSharpValue.GetUnionFields (opt,t)
let uci,arr = FSharpValue.GetUnionFields (n,t)
uci.DeclaringType.GetGenericArguments() |> Array.mapi (fun i x -> printfn "%d %A" i x)
let [|fNone;fSome;|] = FSharpType.GetUnionCases t |> Array.map FSharpValue.PreComputeUnionConstructor
fSome [|4|] :?> Option<int>

open System
open System.IO
open Proton
open Proton.Operators

type Discriminator =
    | Dumb
    | Idiot
    | Normal of float
    with
        static member Encoder (x: Discriminator) =
            match x with
            | Dumb -> Proto.write 1 0uy
            | Idiot -> Proto.write 1 1uy
            | Normal f -> Proto.write 1 2uy *> Proto.write 2 f
        static member Decoder =
            proto {
                let! i = Proto.read 1
                match i with
                | 0uy -> return Dumb
                | 1uy -> return Idiot
                | 2uy -> 
                    let! f = Proto.read 2
                    return Normal f
                | x -> return! Proto.error (sprintf "Expected Discriminator but got %A" x)
            }
module Encoders =
    let discriminator x = 
        match x with
        | Dumb -> Proto.Encode.byte 1 0uy
        | Idiot -> Proto.Encode.byte 1 1uy
        | Normal f -> Proto.Encode.byte 1 2uy *> Proto.Encode.float 2 f

let buff = Proto.Encode.toBytes Encoders.discriminator (Normal 2.)
buff.Length

let buff = Proto.serialize (Normal 2.)
buff.Length

let buff = "{ 'Tag' = 'Normal', 'Value' = 2 }" |> Text.Encoding.UTF8.GetBytes
buff.Length
type Decode = 
    static member object<'a> (proto: Proto<'a>) = 
        (fun (p: Proto) -> 
            match proto p with
            | Ok r, _ -> r
            | Error e, _ -> failwith $"Could not decode bytes to {(typeof<'a>).Name}"
        )

// type Encode =
//     static member object a : Proto<unit> = 
//         fun (p: Proto) ->
//             p  


let decodeDiscriminator = Decode.object (
    proto {
        let! i = Proto.read 1
        match i with
        | 0uy -> return Dumb
        | 1uy -> return Idiot
        | 2uy ->
            let! f = Proto.read 2
            return Normal f
        | _ -> return! Proto.error "Invalid data for type Discriminator!"
    }
)
    

[<TypeMap(12)>]
type Something =
    {
        Some: int
        Thing: string option
        Discriminator: Discriminator
    }
    with
        static member ToProto (x: Something) =
            Proto.write 1 x.Some
            *> Proto.writeOption 2 x.Thing
            *> Proto.write 3 x.Discriminator
        static member FromProto (_: Something) =
            proto {
                let! s = Proto.read 1
                let! t = Proto.readOption 2
                let! d = Proto.read 3
                return { Some = s; Thing = t; Discriminator = d}
            }

let decodeSomething = Decode.object(
    proto {
        let! s = Proto.read 1
        let! t = Proto.readOption 2
        let! d = Proto.read 3
        return { Some = s; Thing = t; Discriminator = d}
    }
)

let buf = Proto.serialize { Some = 3; Thing = Some "Arsch"; Discriminator = Idiot }
printfn "%A" buf
let orig = Proto.deserialize<Something> buf

type FileAction =
    | Reading
    | Writing

let openFile action path =
    let fileMode =
        function
        | Reading -> FileMode.Open
        | Writing ->
            if File.Exists path then FileMode.Open else FileMode.Create
    let fileAccess = function | Reading -> FileAccess.Read | Writing -> FileAccess.ReadWrite
    let fileShare = function | Reading -> FileShare.ReadWrite | Writing -> FileShare.Read
    new FileStream(path, (fileMode action), (fileAccess action), (fileShare action))

let fp = "Test.db"
let fs = fp |> openFile Writing
let fs = new MemoryStream()
[{ Some = 36; Thing = Some "Irgendwas"; Discriminator = Normal 2.2 }; {Some = 26; Thing = Some "Depp"; Discriminator = Dumb}; { Some = 80; Thing = None; Discriminator = Idiot}] |> Message.appendMultiple fs
fs.Seek(0L, SeekOrigin.Begin)
fs.Length
fs.Close()
fs.Dispose()

let x = Message.getAll<Something> fs |> Seq.toArray

open System

let safeCage f =
    try
        f()
        Ok ()
    with ex -> Error $"An exeption was thrown while trying to encode a value.{f}{Environment.NewLine}{ex}"

let encodeme() = failwith "Thats expected too!"

safeCage(encodeme)

sizeof<decimal> * 8