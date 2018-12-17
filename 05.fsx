let (|Same|_|) = function
    | (x:char) :: y :: t when int x = int y+32 || int y = int x + 32 -> Some t
    | _ -> None

let rec loop result = function
    | Same t -> loop result t
    | h :: t -> loop (h::result) t 
    | t -> t @ List.rev result 

let rec outerloop list =
    let r = loop [] list
    if r <> list then
        outerloop r
    else
        r

let out =
    System.IO.File.ReadAllText("input/05.txt")
    |> Seq.toList
    |> outerloop

List.length out 

let optimize (c: char) =
    out 
    |> List.filter (fun k -> int k <> int c && int k <> int c+32)
    |> outerloop
    |> List.length

['A' .. 'Z'] |> List.minBy optimize

optimize 'Z'

