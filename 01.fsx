#r "netstandard"
#r "packages/FSharp.Data/lib/netstandard2.0/FSharp.Data.dll"
open System.IO
open FSharp.Data

let items = 
    File.ReadAllLines("input/01.txt")
    |> Array.map int
    |> Array.toList

List.sum items

// let rec repeat items =
//     seq {
//         yield! items
//         yield! repeat items
//     }

type State =
    | NotFound of int Set
    | Found of int

let acc state item =
    match state with
    | Found n -> Found n
    | NotFound items ->
        if Set.contains item items then
            Found item
        else
            NotFound (Set.add item items)

// repeat items
// |> Seq.scan (+) 0
// |> Seq.scan acc (NotFound Set.empty)
// |> Seq.pick (function Found n -> Some n |_ -> None)
        
type Stream<'a> =
    | Nil 
    | Cons of 'a * (unit -> Stream<'a>)


Cons(2, fun () -> Cons(3, fun () -> Nil))

let rec map f = function
    | Nil -> Nil
    | Cons(x, n) -> Cons(f x,  n >> map f)

let rec filter f =
    function
    | Nil -> Nil
    | Cons(x, n) as e when f x -> e
    | Cons(_, n) -> filter f (n())

let rec fold acc i =
    function
    | Nil -> i
    | Cons(x, n) -> fold acc (acc i x) (n())

let rec scan acc state =
    function
    | Nil -> Cons(state, fun _ -> Nil)
    | Cons(x, n) -> Cons(acc state x, fun () -> scan acc (acc state x) (n()))


let rec concat xs ys =
    match xs with
    | Nil -> ys
    | Cons(x,n) -> Cons(x, fun () -> concat (n()) ys)

let rec concatWith xs fys =
    match xs with
    | Nil -> fys()
    | Cons(x,n) -> Cons(x, fun () -> concat (n()) (fys()))

let rec collect (f: 'a -> 'b Stream) (xs: 'a Stream)  = 
    match xs with
    | Nil -> Nil
    | Cons(x, n) ->
        let ys = f x
        concat ys (collect f (n()))


let combine (f: 'a -> 'b Stream) (g: 'b -> 'c Stream) : 'a -> 'c Stream =
    f >> collect g 

let (>>=) x f = collect f x
let (=<<) f x = collect f x
let (>=>) = combine

let rec toStream = function
    | [] -> Nil
    | h :: tail -> Cons(h, fun _ -> toStream tail) 

let rec repeat stream =
    concatWith stream (fun () -> repeat stream)

let rec tryPick f = function
    | Nil -> None
    | Cons(x, n) ->
        match f x with
        | Some v -> Some v
        | None -> tryPick f (n())


items
|> toStream
|> repeat
|> scan (+) 0
|> scan acc (NotFound Set.empty)
|> tryPick (function Found n -> Some n | _ -> None) 

