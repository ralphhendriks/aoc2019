open System
open System.IO

type state = Running | WaitingForInput | Finished
type parameterMode = Position | Immediate

[<EntryPoint>]
let main _ =

    let cycle (input: int option) (output: int option) (ip, mem: int array) =
        let opCode = mem.[ip]
        let intToMode = function | 0 -> Position | 1 -> Immediate | _ -> invalidArg "intMode" "must be 0 or 1"
        let mode1 = intToMode ((opCode % 1000) / 100)
        let mode2 = intToMode (opCode / 1000)
        let readFrom addr = function
            | Position -> mem.[mem.[addr]]
            | Immediate -> mem.[addr]
        match (opCode % 100) with
        | 1 ->
            mem.[mem.[ip + 3]] <- (readFrom (ip + 1) mode1) + (readFrom (ip + 2) mode2)
            (Running, ip + 4, input, output)
        | 2 ->
            mem.[mem.[ip + 3]] <- (readFrom (ip + 1) mode1) * (readFrom (ip + 2) mode2)
            (Running, ip + 4, input, output)
        | 3 ->
            match input with
            | None -> (WaitingForInput, ip, None, output)
            | Some inp ->
                mem.[mem.[ip + 1]] <- inp
                (Running, ip + 2, None, output)
        | 4 ->
            (Running, ip + 2, input, Some (readFrom (ip + 1) mode1))
        | 5 ->
            if (readFrom (ip + 1) mode1) <> 0 then (Running, (readFrom (ip + 2) mode2), input, output)
            else (Running, ip + 3, input, output)
        | 6 ->
            if (readFrom (ip + 1) mode1) = 0 then (Running, (readFrom (ip + 2) mode2), input, output)
            else (Running, ip + 3, input, output)
        | 7 ->
            if (readFrom (ip + 1) mode1) < (readFrom (ip + 2) mode2) then mem.[mem.[ip + 3]] <- 1
            else mem.[mem.[ip + 3]] <- 0
            (Running, ip + 4, input, output)
        | 8 ->
            if (readFrom (ip + 1) mode1) = (readFrom (ip + 2) mode2) then mem.[mem.[ip + 3]] <- 1
            else mem.[mem.[ip + 3]] <- 0
            (Running, ip + 4, input, output)
        | 99 -> (Finished, ip, input, output)
        | _ -> invalidOp "Invalid opcode"

    let rec solve input output (ip, mem: int array) =
        match (cycle input output (ip, mem)) with
        | (Finished, nextIp, _, nextOutput) -> (nextOutput, (nextIp, mem), true)
        | (WaitingForInput, nextIp, _, nextOutput) -> (nextOutput, (nextIp, mem), false)
        | (Running, nextIp, nextInput, nextOutput) -> solve nextInput nextOutput (nextIp, mem)

    let program =
        File.ReadAllLines("input.txt")
        |> Array.exactlyOne
        |> fun line -> line.Split(',')
        |> Array.map int

    let solveAllOpenLoop input phases =
        let solveSingleOpenLoop input phase =
            match (solve (Some phase) None (0, (Array.copy program))) with
            | (initOutput, initState, false) ->
                match (solve (Some input) initOutput initState) with
                | (Some output, _, true) -> output
                | _ -> failwith "No output"
            | (_, _, true) -> failwith "Initialization failed"
        List.fold solveSingleOpenLoop input phases

    let solveAllClosedLoop input phases =
        let states =
            phases
            |> List.map (fun phase ->
                match (solve (Some phase) None (0, Array.copy program)) with
                | (output, state, false) -> (output, state, false)
                | (_, _, true) -> failwith "Initialization failed")
            |> List.toArray
        let mutable out = input
        while (not (Array.forall (fun (_, _, ready) -> ready) states)) do
            for i = 0 to ((Array.length states) - 1) do
               let (outputX, stateX, _) = states.[i]
               match (solve (Some out) outputX stateX) with
               | (Some outputY, stateY, readyY) ->
                    states.[i] <- ((Some outputY), stateY, readyY)
                    out <- outputY
               | _ -> failwith "No output"
        out


    // http://www.fssnip.net/4u/title/Very-Fast-Permutations
    let rec insertions x = function
    | []             -> [[x]]
    | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

    let rec permutations = function
    | []      -> seq [ [] ]
    | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

    let answer1 = 
        permutations [0; 1; 2; 3; 4]
        |> Seq.map (solveAllOpenLoop 0)
        |> Seq.max
    printfn "Answer 1: %i" answer1

    let answer2 =
        permutations [5; 6; 7; 8; 9]
        |> Seq.map (solveAllClosedLoop 0)
        |> Seq.max
    printfn "Answer 2: %i" answer2

    Console.ReadKey() |> ignore
    0 // return an integer exit code
