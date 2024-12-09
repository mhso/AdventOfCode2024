module Day7

let rec calc test acc (cache: Map<char, Map<(int64 * int64), int64>>) = function
    | v::_ when v > test -> false
    | v::t ->
        let plus = if Map.containsKey (v, acc) cache['+'] then cache else Map.add '+' (Map.add (v, acc) (v + acc) cache['+']) cache
        let mul = if Map.containsKey (v, acc) cache['*'] then cache else Map.add '*' (Map.add (v, acc) (v * acc) cache['*']) cache

        calc test (v + acc) plus t || calc test (v * acc) mul t
    | [] -> acc = test

let testOp (vs: string) =
    let split1 = vs.Split(":")
    let split2 = split1[1].Trim().Split(" ")

    let test = split1[0] |> int64
    let nums = (Array.map int64 split2) |> Array.toList

    let cache = Map[('*', Map.empty); ('+', Map.empty); ('|', Map.empty)]
    let s = nums[0]
    let rest = nums.GetSlice (Some 1, None)

    if calc test s cache rest then test else 0 |> int64

let part1 (lines: string list) =
    printf "%d\n" (List.sumBy testOp lines)

let concat a b = ((String.concat "" [(a |> string); (b |> string)]) |> int64)

let rec calc2 test acc (cache: Map<char, Map<(int64 * int64), int64>>) = function
    | v::_ when v > test -> false
    | v::t ->
        let plus = if Map.containsKey (v, acc) cache['+'] then cache else Map.add '+' (Map.add (acc, v) (acc + v) cache['+']) cache
        let mul = if Map.containsKey (v, acc) cache['*'] then cache else Map.add '*' (Map.add (acc, v) (acc * v) cache['*']) cache
        let con = if Map.containsKey (v, acc) cache['|'] then cache else Map.add '|' (Map.add (acc, v) (concat acc v) cache['|']) cache

        calc2 test (acc + v) plus t || calc2 test (acc * v) mul t || calc2 test (concat acc v) con t
    | [] -> acc = test

let testOp2 (vs: string) =
    let split1 = vs.Split(":")
    let split2 = split1[1].Trim().Split(" ")

    let test = split1[0] |> int64
    let nums = (Array.map int64 split2) |> Array.toList

    let cache = Map[('*', Map.empty); ('+', Map.empty); ('|', Map.empty)]
    let s = nums[0]
    let rest = nums.GetSlice (Some 1, None)

    if calc2 test s cache rest then test else 0 |> int64

let part2 (lines: string list) =
    printf "%d\n" (List.sumBy testOp2 lines)
