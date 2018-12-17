open System
type [<Struct>] Point = { x: int16; y: int16}

let inline x p = p.x 
let inline y p = p.y 

let parse (line: string) =
    match line.Split([|',';' '|], StringSplitOptions.RemoveEmptyEntries) with
    | [| x; y |] -> { x = int16 x ; y = int16 y } 
    | _ -> failwith "invalid format"

let dist a b = abs(a.x-b.x) + abs(a.y-b.y)

let names = Array.append [|'a' .. 'z'|] [|'A' .. 'Z'|] 
let points =
    IO.File.ReadAllLines("input/06.txt")
    |> Array.map parse
    |> Array.mapi(fun i p -> names.[i], p)


points |> Array.minBy (snd >> x)
points |> Array.maxBy (snd >> x)
points |> Array.minBy (snd >> y)
points |> Array.maxBy (snd >> y)

let buffer = [| 
    for j in 0s .. 400s do
    for i in 0s .. 400s do
    let p = {x = i; y = j}
    let closest =
        points
        |> Array.minBy (snd >> dist p)
        |> fst
    yield closest
|]

let print w h (buffer: char[]) =
    for j in 0 .. h do
        for i in 0 .. w do
            printf "%c" buffer.[i + j * w]
        printfn ""

print 400 400 buffer