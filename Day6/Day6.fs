module Day6

let guards = ['^'; '>'; 'v'; '<']

let rec tracePath x y dx dy (arr: char array2d) =
    if x < 0 || x >= Array2D.length1 arr || y < 0 || y >= Array2D.length2 arr then arr
    else
    match arr[y,x] with
    | '#' -> tracePath (x - dx) (y - dy) (dy * -1) (dx * 1) arr
    | '.' | 'X' ->
        arr[y,x] <- 'X'
        tracePath (x + dx) (y + dy) dx dy arr
    | _ -> arr


let part1 (lines: string list) =
    let w = lines[0].Length
    let h = lines.Length
    let charArr = List.map (fun (s: string) -> s.ToCharArray()) lines
    let arr = Array2D.init h w (fun x y -> charArr[x][y])

    let y = List.findIndex (fun a -> (Array.tryFind (fun v -> List.contains v guards) a).IsSome) charArr
    let x = Array.findIndex (fun v -> List.contains v guards) charArr[y]
    let dx = (
        match charArr[y][x] with
        | '>' -> 1
        | '<' -> -1
        | _ -> 0
    )
    let dy = (
        match charArr[y][x] with
        | 'v' -> 1
        | '^' -> -1
        | _ -> 0
    )

    arr[y,x] <- 'X'
    let newArr = tracePath x y dx dy arr
    printf "%d\n" (Seq.sumBy (fun c -> if c = 'X' then 1 else 0) (Seq.cast newArr))

let rec tracePathCycle x y dx dy (arr: char array2d) s =
    if x < 0 || x >= Array2D.length1 arr || y < 0 || y >= Array2D.length2 arr then 0
    elif Set.contains (x, y, dx, dy) s then 1
    else
    match arr[y,x] with
    | '#' -> tracePathCycle (x - dx) (y - dy) (dy * -1) (dx * 1) arr s
    | '.' | 'X' ->
        arr[y,x] <- 'X'
        tracePathCycle (x + dx) (y + dy) dx dy arr (Set.add (x, y, dx, dy)  s)
    | _ -> 0

let tryStartPoint sx sy x y dx dy (charArr: char array2d) =
    if List.contains (charArr[y, x]) guards then 0
    else 
        charArr[y,x] <- '#'
        tracePathCycle sx sy dx dy charArr Set.empty

let part2 (lines: string list) =
    let charArr = List.map (fun (s: string) -> s.ToCharArray()) lines
    let sy = List.findIndex (fun a -> (Array.tryFind (fun v -> List.contains v guards) a).IsSome) charArr
    let sx = Array.findIndex (fun v -> List.contains v guards) charArr[sy]
    let w = charArr[0].Length
    let h = charArr.Length
    let arr = Array2D.init h w (fun x y -> charArr[x][y])
    let dx = (
        match charArr[sy][sx] with
        | '>' -> 1
        | '<' -> -1
        | _ -> 0
    )
    let dy = (
        match charArr[sy][sx] with
        | 'v' -> 1
        | '^' -> -1
        | _ -> 0
    )
    arr[sy,sx] <- 'X'

    printf "%d\n" (List.sumBy (fun (y, a) -> Array.sumBy (fun (x, _) -> tryStartPoint sx sy x y dx dy (Array2D.copy arr)) (Array.indexed a)) (List.indexed charArr))
