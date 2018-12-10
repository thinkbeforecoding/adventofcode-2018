open System.IO
open System.Text.RegularExpressions

type [<Struct>] Rect =
    {
        top: int16
        left: int16
        right: int16
        bottom: int16
    }

let pattern = Regex("""^.* @ (?<left>\d+),(?<top>\d+): (?<width>\d+)x(?<height>\d+)$""", RegexOptions.Compiled)

let parse line = 
    let m = pattern.Match(line)
    let left = int16 m.Groups.["left"].Value
    let top = int16 m.Groups.["top"].Value
    let width = int16 m.Groups.["width"].Value
    let height = int16 m.Groups.["height"].Value
    {
        left = left
        top = top
        right = left + width
        bottom = top + height 
    }
 
let items = 
    File.ReadAllLines("input/03.txt")
    // [|"#1 @ 1,3: 4x4"
    //   "#2 @ 3,1: 4x4"
    //   "#3 @ 5,5: 2x2" |]
    |> Array.map parse 
    

let contains x y rect =
    rect.left <= x && x < rect.right
    && rect.top <= y && y < rect.bottom

let inline toInt b =
    if b then 1 else 0    

let atLeastTwo x =
    x >= 2 |> toInt 

let overlap x y =
    items |> Array.sumBy (contains x y >> toInt) |> atLeastTwo

seq {
    for j in 0s..1000s do
        for i in 0s..1000s -> overlap i j
} |> Seq.sum

let bitmap = [|
    for j in 0s..999s do
        for i in 0s..999s -> overlap i j |> byte
|]

let rec intersectLoop rect x y  =
    printfn "%d,%d" x y
    if x = rect.right then
        if y = rect.bottom then
            true
        else
            intersectLoop rect rect.left (y + 1s)
    else
        if bitmap.[int y * 1000 + int x] <> 0uy then
            false
        else
            intersectLoop rect (x + 1s) y

let intersect rect =
    intersectLoop rect rect.left rect.top

intersect items.[0]
items |> Array.mapi (fun i r -> i + 1, r) |> Array.filter (fun (i,r) -> try intersect r with ex -> printfn "%d" i; false) //(snd >> intersect)