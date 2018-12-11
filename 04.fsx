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

items |> List.filter (fun x -> guard x = sleeper)