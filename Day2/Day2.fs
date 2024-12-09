module Day2

let cmpDiff v1 v2 =
    let diff = v1 - v2
    diff >= 1 && diff <= 3

let order (arr: array<int * int>)  =
    Array.fold (fun acc (v1, v2) -> acc && (cmpDiff v1 v2)) true arr

let transformList (lines: string list) =
    List.map (fun (l: string) -> Array.map int (l.Split(" "))) lines

let count (a: array<int>) =
    if (a |> Array.pairwise |> order) || (a |> Array.rev |> Array.pairwise |> order) then 1 else 0

let part1 (lines: string list) =
    let intList = transformList lines

    printf "%d\n" (List.sumBy count intList)

let rec part2_rec index l =
    let new_list = snd (Array.unzip (Array.filter (fun (i, v) -> i <> index) (Array.indexed l)))
    let result = count new_list
    if result = 1 || index = l.Length - 1 then
        result
    else
        part2_rec (index + 1) l

let part2 lines =
    let intList = transformList lines
    printf "%d\n" (List.sumBy (fun v -> part2_rec 0 v) intList)
