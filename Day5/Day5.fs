module Day5

let orderMap (map: Map<(string * string), int>) (pair: string * string) =
    let x, y = pair
    Map.add (y, x) 1 (Map.add (x, y) -1 map)

let sortLine (map: Map<(string * string), int>) (a: string array) =
    Array.sortWith (fun a b -> match map.TryFind (a, b) with Some(v) -> v | None -> 0) a

let splitPair (s: string) =
    let split = s.Split("|")
    (split[0], split[1])

let part1 (lines: string list) =
    let ordering = List.map splitPair (List.takeWhile (fun s -> s <> "") lines)
    let map = List.fold orderMap Map.empty ordering
    let rest = List.map (fun (s: string) -> s.Split(",")) (lines.GetSlice (Some (ordering.Length + 1),  None))

    printf "%d\n" (List.sumBy (fun s -> if sortLine map s = s then s[s.Length / 2] |> int else 0) rest)

let pick map s =
    let v = sortLine map s
    if v <> s then v[v.Length / 2] |> int else 0

let part2 (lines: string list) =
    let ordering = List.map splitPair (List.takeWhile (fun s -> s <> "") lines)
    let map = List.fold orderMap Map.empty ordering
    let rest = List.map (fun (s: string) -> s.Split(",")) (lines.GetSlice (Some (ordering.Length + 1),  None))

    printf "%d\n" (List.sumBy (fun s -> pick map s) rest)
