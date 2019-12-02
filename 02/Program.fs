open System
open System.IO


let convert (input: string) =
    input
    |> fun line -> line.Split(',')
    |> Array.map int

let evaluate (program: int array) =
    let mem = Array.copy program
    let mutable ip = 0
    while mem.[ip] <> 99 do
        match mem.[ip] with
        | 1 ->
            mem.[mem.[ip + 3]] <- mem.[mem.[ip + 1]] + mem.[mem.[ip + 2]]
            ip <- ip + 4
        | 2 ->
            mem.[mem.[ip + 3]] <- mem.[mem.[ip + 1]] * mem.[mem.[ip + 2]]
            ip <- ip + 4
        | _ -> failwith "Invalid instruction"
    mem

let solve noun verb (program: int array) =
    program.[1] <- noun
    program.[2] <- verb
    let mem = evaluate program
    mem.[0]

let solve1 program = solve 12 2 program

let solve2 output program =
    Seq.allPairs {0 .. 99} {0 .. 99}
    |> Seq.filter (fun (noun, verb) -> (solve noun verb program) = output)
    |> Seq.head
    |> fun (noun, verb) -> 100 * noun + verb

[<EntryPoint>]
let main _ =
    // Test 1 (expected 3500,9,10,70,2,3,11,0,99,30,40,50)
    //let program = "1,9,10,3,2,3,11,0,99,30,40,50" |> convert

    // Test 2 (expected 2,0,0,0,99)
    //let program = "1,0,0,0,99" |> convert
    
    // Test 3 (expected 30,1,1,4,2,5,6,0,99)
    //let program = "1,1,1,4,99,5,6,0,99" |> convert

    //printfn "%A" program
    //printfn "%A" (evaluate program)

    let program =
        File.ReadAllLines("input.txt")
        |> Array.exactlyOne
        |> convert

    let answer1 = solve1 program
    printfn "Answer 1: %i" answer1
    
    let answer2 = solve2 19690720 program
    printfn "Answer 2: %i" answer2

    Console.ReadKey() |> ignore
    0 // return an integer exit code
