module GameLogic

let rnd = System.Random()

type Heaps = int array

// getOptimal : Heaps -> Heaps
let getOptimal arr =
    let calc_m arr = Array.fold (fun x m -> x ^^^ m) 0 arr
    let m = calc_m arr
    if m <> 0 then
        let u = Array.findIndex (fun x -> x ^^^ m < x) arr
        arr.[u] <- arr.[u] ^^^ m
    else
        let maxI = Array.findIndex (fun x -> x = Array.max arr) arr
        arr.[maxI] <- arr.[maxI]-1
    arr

// subtract : int -> Heaps -> Heaps
let subtract r (arr:Heaps) = 
    arr.[r] <- arr.[r] - rnd.Next(1, arr.[r])
    arr

// makeMove : Heaps -> int -> Heaps
let makeMove (arr:Heaps) i = 
    if arr.[i] > 0 then arr.[i] <- arr.[i] - 1 
    arr

// getRandom : int * Heaps -> Heaps
let rec getRandom = function
    | (r, arr:Heaps) when Array.sum arr = 0 -> arr
    | (r, arr:Heaps) when arr.[r] = 0 -> getRandom(rnd.Next(0, arr.Length), arr)
    | (r, arr:Heaps) -> subtract r arr

// checkGameState : Heaps -> bool
let checkGameState arr = (Array.sum arr) = 0