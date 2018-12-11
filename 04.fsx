open System
open System.IO
open System.Text.RegularExpressions

fsi.AddPrinter<DateTime> (fun d -> d.ToString("yyyy-MM-dd HH:mm"))

type [<Struct>] Guard = Guard of int

type State = New of Guard | Awake | Asleep

type [<Struct>] Log =
    {
        date: DateTime
        state: State
    }

let pattern = Regex("""^\[(?<date>.*)\] (Guard #(?<guardId>\d*))?(?<state>[\w ]*)$""", RegexOptions.Compiled)

let toDate input =
    let format = "yyyy-MM-dd HH:mm" // iso 8601?
    System.DateTime.ParseExact(input, format, null)

let toState (input: string) guard =
    match input, guard with
    | input, None when input.Contains("falls asleep") -> State.Asleep
    | input, None when input.Contains("wakes up") -> State.Awake
    | _, Some guard -> State.New guard
    | _ -> failwith "InvalidFormat"

let toOption (group: Group) =
    if group.Success then
        Some group.Value
    else
        None

let groupToGuard = toOption >> Option.map (int >> Guard)

let parse line = 
    let m = pattern.Match(line)
    let date = toDate m.Groups.["date"].Value
    let state = toState m.Groups.["state"].Value (groupToGuard m.Groups.["guardId"] )
    {
        date = date
        state = state
    }

let rec split acc input =
    match input, acc with
    | { state = New _ } :: tail, _ -> split ([List.head input] :: acc) tail
    | head :: tail, hacc :: tacc -> split ((head :: hacc) :: tacc) tail
    | _, acc -> acc |> List.rev |> List.map List.rev  

let items = 
    File.ReadAllLines("input/04.txt")
    // [|"[1518-07-31 00:54] wakes up"
    //   "[1518-07-31 00:50] falls asleep"
    //   "[1518-04-09 00:01] Guard #3407 begins shift"
    //   "[1518-04-09 00:15] falls asleep"
    //   "[1518-07-30 23:55] Guard #4242 begins shift" |]
    |> Array.map parse
    |> Array.sortBy (fun x -> x.date)
    |> Array.toList
    |> split []

let sumSleep log =
    log
    |> List.pairwise
    |> List.sumBy (
        function
        | { state = Asleep ;  date = s}, { state = Awake ; date = e} -> (e - s).Minutes
        | _ -> 0 )

let guard =
    function
    | { state = New g } :: _ -> g
    | _ -> failwith "No guard"

let sleeper =
    items
    |> List.map (fun x -> guard x, sumSleep x)
    |> List.groupBy fst
    |> List.map (fun (g, es) -> g, List.sumBy snd es)
    |> List.sortBy snd
    |> List.last |> fst


type Series<'a> =
    | Series of 'a * (DateTime * 'a) list 

let map2 f (Series(sx, nx)) (Series(sy,ny)) =
    let rec loop sx sy nx ny result =
        match nx, ny with
        | [], [] -> List.rev result
        | [], (d,hy) :: ty -> loop sx hy nx ty ((d, f sx hy) :: result)
        | (d,hx) :: tx, [] -> loop hx sy tx ny ((d, f hx sy) :: result)
        | (dx,hx) :: tx, (dy,hy) :: ty ->
            if dx = dy then
                loop hx hy tx ty ((dx, f hx hy) :: result)
            elif dx < dy then
                loop hx sy tx ny ((dx, f hx sy) :: result)
            else
                loop sx hy nx ty ((dy, f sx hy) :: result)

    Series(f sx sy, loop sx sy nx ny [])

let time (d:DateTime)=
    DateTime(1,1,1,d.Hour, d.Minute, 0)


let toSeries = function
    | { date = d; state = New _ } :: tail -> 
        Series(0,
            tail |> List.map (function
                | {date = d; state = Asleep} -> time d,1
                | { date = d } -> time d,0)
        )
    | _ -> failwith "invalid"

let empty = Series(0,[])

let minute sleeper =
    items 
    |> List.filter (fun x -> guard x = sleeper)
    |> List.map toSeries
    |> List.fold (map2 (+)) empty 
    |> function (Series(_, l)) -> 
                    match l with
                    | [] -> DateTime.MinValue, 0
                    | _ -> List.maxBy snd l
    |> fun (d, n) -> d.Minute, n

let (Guard g) = sleeper in
let (m,n) = minute sleeper in
m * g


// part II ---------------

let guards =
    items
    |> List.map guard
    |> List.distinct

guards
|> List.map (fun g -> g, minute g)
|> List.maxBy (snd >> snd)
|> function (Guard g, (m,c)) -> g * m

minute (Guard 3203)

items
|> List.filter (fun l -> guard l = (Guard 3203))