module Day9

let explode (s: int array) =
    let e, _, _ = (Array.fold (fun (l, i, f) c -> ((l @ List.init c (fun _ -> if f then i else -1)), (if f then i + 1 else i), not f)) (List.empty, 0, true) s)
    e |> List.toArray

[<TailCall>]
let rec rearrange arr i =
    if i = 0 then arr
    else
        let free = Array.findIndex (fun v -> v = -1) arr
        if free < i then
            arr[free] <- arr[i]
            arr[i] <- -1
            rearrange arr (i - 1)
        else
            rearrange arr (i - 1)

let part1 (lines: string list) =
    let expanded = explode (Array.map (fun c -> System.String([| c |]) |> int) (lines[0].ToCharArray()))
    let arranged: int array = rearrange expanded (expanded.Length - 1)
    printf "%d\n" (Array.sumBy (fun (i, v) -> if v <> -1 then (int64 i) * (int64 v) else int64 0) (Array.indexed arranged))

[<TailCall>]
let rec rearrange2 (arr: int array) i =
    if i = 0 then arr
    else
        if arr[i] = -1
        then rearrange2 arr (i - 1)
        else
            match Array.tryFindIndexBack (fun (j, v) -> j < i && v <> arr[i]) (Array.indexed arr) with
            | Some si ->
                let size = i - si
                let searchVal = Array.init size (fun _ -> -1)
                match Array.tryFindIndex (fun v -> v = searchVal) (Array.windowed size arr) with
                | Some (free) when free < i ->
                    Array.blit arr (si + 1) arr free size
                    Array.blit searchVal 0 arr (si + 1) size
                    rearrange2 arr si
                | _ -> rearrange2 arr si
            | None -> arr

let part2 (lines: string list) =
    let expanded = explode (Array.map (fun c -> System.String([| c |]) |> int) (lines[0].ToCharArray()))
    let arranged: int array = rearrange2 expanded (expanded.Length - 1)
    printf "%d\n" (Array.sumBy (fun (i, v) -> if v <> -1 then (int64 i) * (int64 v) else int64 0) (Array.indexed arranged))
