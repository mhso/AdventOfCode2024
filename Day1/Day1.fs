module Day1

let splitLists lines  =
    List.unzip (List.map (fun (l: string) -> (Array.pairwise (Array.map int (l.Split(" ", options=System.StringSplitOptions.RemoveEmptyEntries))))[0]) lines)

let unpack v =
    match v with
    | Some(v) -> v
    | None -> 0

let part1 (lines: string list) =
    let (l1, l2) = splitLists lines
    List.sum (List.map (fun (v1, v2) -> abs(v1 - v2)) (List.zip (List.sort(l1)) (List.sort(l2))))

let part2 lines =
    let (l1, l2) = splitLists lines
    let indexMap = Map(List.countBy id l2)
    List.sum (List.map (fun v -> v * (indexMap.TryFind(v) |> unpack)) l1)
