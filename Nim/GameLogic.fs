module GameLogic

let rnd = System.Random()

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

let subtract (r, arr:int array) = 
    arr.[r] <- arr.[r] - rnd.Next(1, arr.[r])
    arr

let makeMove (arr:int array) i = 
    if arr.[i] > 0 then arr.[i] <- arr.[i] - 1 
    arr

let rec getRandom = function
    | (r, arr:int array) when Array.sum arr = 0 -> arr
    | (r, arr:int array) when arr.[r] = 0 -> getRandom(rnd.Next(0, arr.Length), arr)
    | (r, arr:int array) -> subtract(r, arr)

let checkGameState arr = (Array.sum arr) = 0