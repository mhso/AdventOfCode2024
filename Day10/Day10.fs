module Day10

let char2int (c: char) = System.String([|c|]) |> int

let rec traverse x y (grid: int array2d) (visited: Map<(int * int), bool>) score =
    if Map.containsKey (x, y) visited then (score, visited)
    else
        if grid[y, x] = 9 && not (Map.containsKey (x, y) visited) then
            (score + 1, Map.add (x, y) true visited)
        else
            let score, visited = if y > 0 && grid[y - 1, x] = grid[y, x] + 1 then traverse x (y - 1) grid visited score else (score, visited)
            let score, visited = if y < (Array2D.length1 grid) - 1 && grid[y + 1, x] = grid[y, x] + 1 then traverse x (y + 1) grid visited score else (score, visited)
            let score, visited = if x > 0 && grid[y, x - 1] = grid[y, x] + 1 then traverse (x - 1) y grid visited score else (score, visited)
            if x < (Array2D.length2 grid) - 1 && grid[y, x + 1] = grid[y, x] + 1 then traverse (x + 1) y grid visited score else (score, visited)

let preprocess (lines: string list) =
    let w = lines[0].Length
    let h = lines.Length
    let intArr = List.map (fun (s: string) -> Array.map char2int (s.ToCharArray())) lines

    let y = List.map fst (List.filter (fun (i, a) -> (Array.tryFind (fun v -> v = 0) a).IsSome) (List.indexed intArr))
    let x = (List.map (fun i -> Array.map fst (Array.filter (fun (j, v) -> v = 0) (Array.indexed intArr[i])) |> Array.toList) y)
    let heads = List.fold (fun acc (xs, y) -> acc @ (List.map (fun x -> (x, y)) xs)) [] (List.zip x y)

    (Array2D.init h w (fun x y -> intArr[x][y]), heads)

let part1 (lines: string list) =
    let grid, heads = preprocess lines

    printf "%d\n" (List.sumBy (fun (x, y) -> fst (traverse x y grid Map.empty 0)) heads)

let rec traverse2 x y (grid: int array2d) score =
    if grid[y, x] = 9 then
        score + 1
    else
        let score = if y > 0 && grid[y - 1, x] = grid[y, x] + 1 then traverse2 x (y - 1) grid score else score
        let score = if y < (Array2D.length1 grid) - 1 && grid[y + 1, x] = grid[y, x] + 1 then traverse2 x (y + 1) grid score else score
        let score = if x > 0 && grid[y, x - 1] = grid[y, x] + 1 then traverse2 (x - 1) y grid score else score
        if x < (Array2D.length2 grid) - 1 && grid[y, x + 1] = grid[y, x] + 1 then traverse2 (x + 1) y grid score else score

let part2 (lines: string list) =
    let grid, heads = preprocess lines
    
    printf "%d\n" (List.sumBy (fun (x, y) -> traverse2 x y grid 0) heads)
