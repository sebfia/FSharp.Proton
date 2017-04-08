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
let optBuf = [|8uy; 1uy; 18uy; 7uy; 10uy; 5uy; 65uy; 114uy; 115uy; 99uy; 104uy|]
let buffer = [|10uy; 5uy; 65uy; 114uy; 115uy; 99uy; 104uy|]

ms.Write(buffer, 0, buffer.Length)
ms.Position
ms.Seek(0L, SeekOrigin.Begin)
reader.Dispose()
ms.Dispose()

[<AttributeUsage(AttributeTargets.Class)>]
type TypeMapAttribute(identifier: int) =
    inherit Attribute()
    member val Identifier = identifier

[<TypeMap(1)>]
type Action =
    | Release
    | Hold
    | Cancel of string

let att = Attribute.GetCustomAttribute(typeof<Action>, typeof<TypeMapAttribute>)

att :?> TypeMapAttribute |> fun a -> a.Identifier |> int16

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

open Proton
open Proton.Operators

type Something =
    {
        Some: int
        Thing: string option
    }
    with
        static member ToProto (x: Something) =
            Proto.write 1 x.Some
            *> Proto.writeOption 2 x.Thing
        static member FromProto (_: Something) =
            proto {
                let! s = Proto.read 1
                let! t = Proto.readOption 2
                return { Some = s; Thing = t}
            }

let buf = Proto.serialize { Some = 3; Thing = Some "Arsch" }
printfn "%A" buf
let orig = Proto.deserialize<Something> buf

type Damnit = 
    {
        Ass : int
        Hole : string
    }
    with 
        static member ToProto (x: Damnit) =
            Proto.write 1 x.Ass
            *> Proto.write 2 x.Hole
        static member FromProto (_: Damnit) =
            proto {
                let! a = Proto.read 1
                let! h = Proto.read 2
                return { Ass = a; Hole = h }
            }

let dbuf = Proto.serialize { Ass = 3; Hole = "Mongo" }
let dOrig = Proto.deserialize<Damnit> dbuf
let ms = System.IO.MemoryStream()
let w = ProtoBuf.ProtoWriter(ms, null, null)
ProtoBuf.ProtoWriter.WriteFieldHeader(1,ProtoBuf.WireType.String,w)
ProtoBuf.ProtoWriter.WriteType(t,w)
ms.Length
ms.Dispose()
w.Close()