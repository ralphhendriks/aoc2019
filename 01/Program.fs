open System
open System.IO

let calculateFuel mass = mass / 3 - 2

let rec calculateFuelRecursively mass =
    let z = calculateFuel mass
    if z <= 0 then 0
    else z + calculateFuelRecursively z

[<EntryPoint>]
let main _ =
    let masses =
        File.ReadAllLines("input.txt")
        |> Seq.map int
    let answer1 =
        masses
        |> Seq.map calculateFuel
        |> Seq.sum
    printfn "Answer 1: %i" answer1
    let answer2 =
        masses
        |> Seq.map calculateFuelRecursively
        |> Seq.sum
    printfn "Answer 2: %i" answer2
    Console.ReadKey() |> ignore
    0 // return an integer exit code
