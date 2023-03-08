namespace Tests

open FsUnit

type InitMsgUtils() =
    inherit FSharpCustomMessageFormatter()

[<AutoOpen>]
module Types =
    
    open System
    
    type Shape =
    | Circle of radius: int
    | Rectangle of width: int * height: int

    type Book = {
        Id: Guid
        Title: string
        Author: string
    }

    type Bookshelf =
        | Empty
        | Filled of favorite:Book option*books: Book array

    type Bookstore = {
        Shape: Shape
        Bookshelf: Bookshelf
        FeaturedBook: Book
    }


module Encoder =
    open Proton.Proto
    open Proton.Operators
    

    let ofShape x =
        match x with
        | Circle r -> 
            Encode.byte 1 0uy 
            *> Encode.int32 2 r
        | Rectangle (w,h) -> 
            Encode.byte 1 1uy 
            *> Encode.int32 2 w 
            *> Encode.int32 3 h

    let ofBook (x: Book) =
        Encode.guid 1 x.Id
        *> Encode.string 2 x.Title
        *> Encode.string 3 x.Author

    let ofBookShelf (x: Bookshelf) =
        match x with
        | Empty -> Encode.byte 1 0uy
        | Filled (fave,books) -> 
            Encode.byte 1 1uy
            *> Encode.option ofBook 2 fave
            *> Encode.array ofBook 3 books

    let ofBookstore (x: Bookstore) =
        Encode.object ofShape 1 x.Shape
        *> Encode.object ofBookShelf 2 x.Bookshelf
        *> Encode.object ofBook 3 x.FeaturedBook

module Decoder =
    open Proton
    open Proton.Proto

    let ofShape = proto {
        match! Decode.byte 1 with
        | 0uy -> 
            let! r = Decode.int32 2
            return Circle r
        | 1uy ->
            let! w = Decode.int32 2
            let! h = Decode.int32 3
            return Rectangle(w,h)
        | _ -> return! Proto.error "Invalid data for type Shape!"
    }

    let ofBook = proto {
        let! id = Decode.guid 1
        let! title = Decode.string 2
        let! author = Decode.string 3
        return { Id = id; Title = title; Author = author }
    }

    let ofBookShelf = proto {
        match! Decode.byte 1 with
        | 0uy -> return Empty
        | 1uy -> 
            let! fave = Decode.option ofBook 2
            let! books = Decode.array ofBook 3
            return Filled (fave,books)
        | _ -> return! Proto.error "Invalid data for Bookshelf!"
    }

    let ofBookstore = proto {
        let! shape = Decode.object ofShape 1
        let! shelf = Decode.object ofBookShelf 2
        let! book = Decode.object ofBook 3
        return { Shape = shape; Bookshelf = shelf; FeaturedBook = book }
    }

module ``Simple Encoders and Decoders`` =
    open System
    open NUnit.Framework
    open Proton.Proto

    [<Test>]
    let ``Encoding and Decoding Boolean`` () =
        let buffer = (Encode.boolean 1 |> Encode.toBytes) false
        (Decode.boolean 1 |> Decode.fromBytes) buffer |> should equal false

    [<Test>]
    let ``Encoding and Decoding Int16`` () =
        let buffer = (Encode.int16 1 |> Encode.toBytes) 42s
        (Decode.int16 1 |> Decode.fromBytes) buffer |> should equal 42s

    [<Test>]
    let ``Encoding and Decoding Int32`` () =
        let buffer = (Encode.int32 1 |> Encode.toBytes) 142
        (Decode.int32 1 |> Decode.fromBytes) buffer |> should equal 142

    [<Test>]
    let ``Encoding and Decoding Int64`` () =
        let buffer = (Encode.int64 1 |> Encode.toBytes) 4242L
        (Decode.int64 1 |> Decode.fromBytes) buffer |> should equal 4242L

    [<Test>]
    let ``Encoding and Decoding Float`` () =
        let buffer = (Encode.float 1 |> Encode.toBytes) 42.42
        (Decode.float 1 |> Decode.fromBytes) buffer |> should equal 42.42

    [<Test>]
    let ``Encoding and Decoding Single`` () =
        let buffer = (Encode.float32 1 |> Encode.toBytes) 4.2f
        (Decode.float32 1 |> Decode.fromBytes) buffer |> should equal 4.2f

    [<Test>]
    let ``Encoding and Decoding Decimal`` () =
        let buffer = (Encode.decimal 1 |> Encode.toBytes) 4242.4242m
        (Decode.decimal 1 |> Decode.fromBytes) buffer |> should equal 4242.4242m

    [<Test>]
    let ``Encoding and Decoding Byte`` () =
        let buffer = (Encode.byte 1 |> Encode.toBytes) 42uy
        (Decode.byte 1 |> Decode.fromBytes) buffer |> should equal 42uy

    [<Test>]
    let ``Encoding and Decoding String`` () =
        let expected = "I am the number 42"
        let buffer = (Encode.string 1 |> Encode.toBytes) expected
        (Decode.string 1 |> Decode.fromBytes) buffer |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Bytes`` () =
        let expected = "I am the number 42" |> Text.Encoding.UTF8.GetBytes
        let buffer = (Encode.bytes 1 |> Encode.toBytes) expected
        (Decode.bytes 1 |> Decode.fromBytes) buffer |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Guid`` () =
        let expected = Guid.NewGuid()
        let buffer = (Encode.guid 1 |> Encode.toBytes) expected
        (Decode.guid 1 |> Decode.fromBytes) buffer |> should equal expected

    [<Test>]
    let ``Encoding and Decoding DateTimeOffset`` () =
        let expected = DateTimeOffset.Now
        let buffer = (Encode.dateTimeOffset 1 |> Encode.toBytes) expected
        (Decode.dateTimeOffset 1 |> Decode.fromBytes) buffer |> should equal expected
    
    [<Test>]
    let ``Encoding and Decoding DateTime`` () =
        let expected = DateTime.Now
        let buffer = (Encode.dateTime 1 |> Encode.toBytes) expected
        (Decode.dateTime 1 |> Decode.fromBytes) buffer |> should equal expected

    [<Test>]
    let ``Encoding and Decoding TimeSpan`` () =
        let expected = TimeSpan.FromSeconds(42.)
        let buffer = (Encode.timeSpan 1 |> Encode.toBytes) expected
        (Decode.timeSpan 1 |> Decode.fromBytes) buffer |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Of A Simple Int Array`` () =
        let expected = [| 42 .. 142 |]
        let buffer = (Encode.array (Encode.int32 1) 1 |> Encode.toBytes) expected
        (Decode.array (Decode.int32 1) 1 |> Decode.fromBytes) buffer |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Of A Simple None Option`` () =
        let buffer = (Encode.option (Encode.int32 1) 1 |> Encode.toBytes) None
        (Decode.option (Decode.int32 1) 1 |> Decode.fromBytes) buffer |> should equal None

    [<Test>]
    let ``Encoding and Decoding Of A Simple Some Option`` () =
        let buffer = (Encode.option (Encode.int32 1) 1 |> Encode.toBytes) (Some 42)
        (Decode.option (Decode.int32 1) 1 |> Decode.fromBytes) buffer |> should equal (Some 42)

module ``Back And Forth With More Complex Types`` =
    open System
    open NUnit.Framework
    open Proton.Proto

    [<Test>]
    let ``Encoding and Decoding Circle Shape`` () =
        let expected = Circle 2
        let buffer = Encode.toBytes Encoder.ofShape expected
        let actual = Decode.fromBytes Decoder.ofShape buffer
        expected |> should equal actual
    
    [<Test>]
    let ``Encoding And Decoding Rectangle Shape`` () =
        let expected = Rectangle(2,4)
        let buffer = Encode.toBytes Encoder.ofShape expected
        let actual = Decode.fromBytes Decoder.ofShape buffer
        expected |> should equal actual

    [<Test>]
    let ``Encoding And Decoding Of Book Record`` () =
        let expected = { Id = Guid.NewGuid(); Title = "Don Quixote"; Author = "Miguel de Cervantes"}
        let buffer = Encode.toBytes Encoder.ofBook expected
        let actual = Decode.fromBytes Decoder.ofBook buffer
        expected |> should equal actual

    [<Test>]
    let ``Encoding And Decoding Of Book Record Array`` () =
        let expected = [|
            { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
            { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
            { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
            { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
            { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
        |]
        let buffer = Encode.toBytes (Encode.array Encoder.ofBook 1) expected
        let actual = Decode.fromBytes (Decode.array Decoder.ofBook 1) buffer
        expected |> should equal actual

    [<Test>]
    let ``Encoding And Decoding Of Bookshelf with Books`` () =
        let expected = Filled (
            Some { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"},
            [|
                { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
                { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
                { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
                { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
                { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
            |])

        let buffer = Encode.toBytes Encoder.ofBookShelf expected
        let actual = Decode.fromBytes Decoder.ofBookShelf buffer
        expected |> should equal actual

    [<Test>]
    let ``Encoding And Decoding Of Bookstore`` () =
        let shelf = Filled (
            Some { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"},
            [|
                { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
                { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
                { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
                { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
                { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
            |])
        let expected = {
            Shape = Rectangle (10, 20)
            Bookshelf = shelf
            FeaturedBook = { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
        }

        let buffer = Encode.toBytes Encoder.ofBookstore expected
        let actual = Decode.fromBytes Decoder.ofBookstore buffer
        expected |> should equal actual
