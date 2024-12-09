module Day8

let addLoc c x y (map: Map<char, list<(int * int)>>) =
    if c <> '.' then Map.add c (match Map.tryFind c map with | Some(v) -> (x, y)::v | None -> [(x, y)]) map
    else map

let rec combinations acc size set = seq {
  match size, set with 
  | n, x::xs -> 
      if n > 0 then yield! combinations (x::acc) (n - 1) xs
      if n >= 0 then yield! combinations acc n xs 
  | 0, [] -> yield acc 
  | _, [] -> () }

let getAntinodes (x1, y1) (x2, y2) =
    let diffX = x2 - x1
    let diffY = y2 - y1
    [(x1 - diffX, y1 - diffY); (x2 + diffX, y2 + diffY)]

let handleLetter (locs: list<(int * int)>) w h =
    Seq.map (fun (l: list<(int * int)>) -> List.filter (fun (x, y) -> x >= 0 && x < w && y >= 0 && y < h) (getAntinodes l[0] l[1])) (combinations [] 2 locs)
    |> Seq.toList


let part1 (lines: string list) =
    let w = lines.Length
    let h = lines[0].Length
    let locMap = List.fold (fun acc1 (y, s: string) -> Array.fold (fun acc2 (x, c) -> addLoc c x y acc2) acc1 (Array.indexed (s.ToCharArray()))) Map.empty (List.indexed lines)

    let locs = Map.fold (fun acc _ v -> acc @ List.collect id (handleLetter v w h)) [] locMap
    printf "%d\n" (List.distinct locs).Length

let rec getAntinodes2 (x1, y1) (x2, y2) w h s acc =
    let diffX = x2 - x1
    let diffY = y2 - y1

    if s then
        if x1 >= 0 && x1 < w && y1 >= 0 && y1 < h
        then getAntinodes2 (x1 - diffX, y1 - diffY) (x1, y1) w h s ((x1, y1)::acc)
        else acc
    else
        if x2 >= 0 && x2 < w && y2 >= 0 && y2 < h
        then getAntinodes2 (x2, y2) (x2 + diffX, y2 + diffY) w h s ((x2, y2)::acc)
        else acc

let handleLetter2 (locs: list<(int * int)>) w h =
    (
        Seq.map (fun (l: list<(int * int)>) -> (getAntinodes2 l[0] l[1] w h true []) @ (getAntinodes2 l[0] l[1] w h false [])) (combinations [] 2 locs)
        |> Seq.toList
    )

let part2 (lines: string list) =
    let w = lines.Length
    let h = lines[0].Length
    let locMap = List.fold (fun acc1 (y, s: string) -> Array.fold (fun acc2 (x, c) -> addLoc c x y acc2) acc1 (Array.indexed (s.ToCharArray()))) Map.empty (List.indexed lines)

    let locs = Map.fold (fun acc _ v -> acc @ List.collect id (handleLetter2 v w h)) [] locMap
    printf "%d\n" (List.distinct locs).Length
