open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let step moons =
        let dv p0 v0 = function
        | a when a > p0 -> v0 + 1
        | a when a < p0 -> v0 - 1
        | _ -> v0
        moons
        |> List.map (fun ((x0, y0, z0), (vx0, vy0, vz0)) ->
            let vx = moons |> List.map (fst >> (fun (a, _, _) -> a)) |> List.fold (dv x0) vx0
            let vy = moons |> List.map (fst >> (fun (_, a, _) -> a)) |> List.fold (dv y0) vy0
            let vz = moons |> List.map (fst >> (fun (_, _, a) -> a)) |> List.fold (dv z0) vz0
            ((x0 + vx, y0 + vy, z0 + vz), (vx, vy, vz)))

    let totalEnergy =
        List.sumBy (fun ((x, y, z), (vx, vy, vz)) ->
            (abs x + abs y + abs z) * (abs vx + abs vy + abs vz))

    let input =
        File.ReadAllLines("input.txt")
        |> List.ofArray
        |> List.map (fun line ->
            match line with
            | Regex @"<x=(-?\d+), y=(-?\d+), z=(-?\d+)>" [x; y; z] -> ((int x, int y, int z), (0, 0, 0))
            | _ ->  failwith "Invalid input")

    let answer1 = {1..1000} |> Seq.fold (fun inp _ -> step inp) input |> totalEnergy
    printfn "Answer 1: %i" answer1

    // x, y, and z axes are independent. Assumption is that for each of them a period can be found)

    let rec findPeriod dim moons =
        let state = List.map (fun (p, v) -> (dim p, dim v))
        let initialState = state moons
        let rec loop n moons =
            match (step moons) with
            | nextMoons when (state nextMoons) = initialState -> n + 1L
            | nextMoons -> loop (n + 1L) nextMoons
        loop 0L moons

    let pery = findPeriod (fun (_, a, _) -> a) input
    let perx = findPeriod (fun (a, _, _) -> a) input
    let perz = findPeriod (fun (_, _, a) -> a) input
    
    let lcm x y =
        let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
        x * y / (gcd x y)

    let answer2 = lcm (lcm perx pery) perz
    printfn "Answer 2: %i" answer2

    Console.ReadKey() |> ignore
    0 // return an integer exit code
