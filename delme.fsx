#r "packages/protobuf-net/lib/net45/protobuf-net.dll"
#I "deploy"
#r "deploy/Proton.dll"

open System
open System.IO
open ProtoBuf

let ms = new MemoryStream()
let writer = new ProtoWriter(ms, null, null)
let reader = new ProtoReader(ms, null, null)
reader.TryReadFieldHeader(1)
reader.ReadFieldHeader()
reader.ReadString()

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
open System.Reflection

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
        static member ToProto (x: Discriminator) =
            match x with
            | Dumb -> Proto.write 1 0uy
            | Idiot -> Proto.write 1 1uy
            | Normal f -> Proto.write 1 2uy *> Proto.write 2 f
        static member FromProto (_: Discriminator) =
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

let fp = "/Users/sebastian/Trading/Test.db"
let fs = fp |> openFile Reading
[{ Some = 36; Thing = Some "Irgendwas"; Discriminator = Normal 2.2 }; {Some = 26; Thing = Some "Depp"; Discriminator = Dumb}; { Some = 80; Thing = None; Discriminator = Idiot}] |> Message.appendMultiple fs
fs.Seek(0L, SeekOrigin.Begin)
fs.Length
fs.Close()
fs.Dispose()

let x = Message.getAll<Something> fs |> Seq.toArray
