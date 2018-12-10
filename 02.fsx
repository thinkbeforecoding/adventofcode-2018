open System.IO

let items = 
    File.ReadAllLines("input/02.txt")
    // [|"abcde"
    //   "fghij"
    //   "klmno"
    //   "pqrst"
    //   "fguij"
    //   "axcye"
    //   "wvxyz" |]
    |> Array.map Seq.toList
    |> Array.toList

let has x = Set.contains x

let counts =
    items
    |> List.map (List.countBy id >> List.map snd >> Set.ofList)

let ct x =
    counts
    |> List.filter(has x)
    |> List.length

ct 2 * ct 3

let rec diff count input  =
    if count = 2 then
        count
    else
        match input with
        | hx :: tx, hy :: ty-> diff (if hx = hy then count else count+1) (tx,ty)
        | _ -> count 

let rec common result =
    function
    | hx :: tx, hy :: ty ->
        if hx = hy then
            common (hx :: result) (tx,ty)
        else
            common result (tx,ty)
    | _ -> List.rev result |> List.toArray |> System.String


let findMatch input =
    if diff 0 input = 1 then
        Some (common [] input)
    else
        None

let x = items.[1]
let y = items.[4]
let input = x, y
findMatch (items.[1], items.[4])

seq {
        for x in items do
        for y in items do
        match findMatch (x, y) with
        | Some v -> yield v
        | None -> ()
    }
|> Seq.toList


