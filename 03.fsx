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

