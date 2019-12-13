open System
open System.IO

type Direction = Up | Left | Down | Right

type ComputerState = WaitingForInput | Finished
type Computer = {
    State: ComputerState;
    InstructionPointer: int;
    Outputs: int64 list;
    Memory: int64 array;
    RelativeBase: int;
    ExtendedMemory: Map<int, int64>;
}

[<EntryPoint>]
let main _ =

    let turnLeft = function | Up -> Left | Left -> Down | Down -> Right | Right -> Up
    let turnRight = function | Up -> Right | Right -> Down | Down -> Left | Left -> Up
    let move (x, y) = function
    | Up -> (x, y + 1)
    | Left -> (x - 1, y)
    | Down -> (x, y - 1)
    | Right -> (x + 1, y)

    let cycle (input: int64 option, outputs: int64 list, ip: int, mem: int64 array, relBase: int, extMem: Map<int, int64>) =
        let getMem = function
        | addr when addr < mem.Length -> mem.[addr]
        | addr -> match (Map.tryFind addr extMem) with | None -> 0L | Some v -> v

        let setMem value = function
        | addr when addr < mem.Length ->
            mem.[addr] <- value
            extMem
        | addr -> Map.add addr value extMem

        let opCode = int (getMem ip)

        let readFrom par =
            match ((opCode % (100 * (pown 10 par))) / (10 * (pown 10 par))) with
            | 0 -> getMem (int (getMem (ip + par)))
            | 1 -> getMem (ip + par)
            | 2 -> getMem ((int (getMem(ip + par)) + relBase))
            | _ -> failwith "Invalid parameter mode"

        let WriteTo par value =
            match ((opCode % (100 * (pown 10 par))) / (10 * (pown 10 par))) with
            | 0 -> setMem value (int (getMem (ip + par)))
            | 2 -> setMem value ((int (getMem(ip + par)) + relBase))
            | _ -> failwith "Invalid parameter mode"

        match (opCode % 100) with
        | 1 -> (input, outputs, ip + 4, relBase, (WriteTo 3 ((readFrom 1) + (readFrom 2))))
        | 2 -> (input, outputs, ip + 4, relBase, WriteTo 3 ((readFrom 1) * (readFrom 2)))
        | 3 ->
            match input with
            | None -> (None, outputs, ip, relBase, extMem)
            | Some inp -> (None, outputs, ip + 2, relBase, WriteTo 1 inp)
        | 4 -> (input, (readFrom 1)::outputs, ip + 2, relBase, extMem)
        | 5 ->
            if (readFrom 1) <> 0L then (input, outputs, (int (readFrom 2)), relBase, extMem)
            else (input, outputs, ip + 3, relBase, extMem)
        | 6 ->
            if (readFrom 1) = 0L then (input, outputs, (int (readFrom 2)), relBase, extMem)
            else (input, outputs, ip + 3, relBase, extMem)
        | 7 ->
            if (readFrom 1) < (readFrom 2) then (input, outputs, ip + 4, relBase, WriteTo 3 1L)
            else (input, outputs, ip + 4, relBase, WriteTo 3 0L)
        | 8 ->
            if (readFrom 1) = (readFrom 2) then (input, outputs, ip + 4, relBase, WriteTo 3 1L)
            else (input, outputs, ip + 4, relBase, WriteTo 3 0L)
        | 9 ->
            (input, outputs, ip + 2, relBase + (int (readFrom 1)), extMem)
        | 99 -> (input, outputs, -1, relBase, extMem)
        | _ -> invalidOp "Invalid opcode"

    let rec solve (input: int64 option, outputs: int64 list, ip: int, mem: int64 array, relBase: int, extMem: Map<int, int64>) =
        match (cycle (input, outputs, ip, mem, relBase, extMem)) with
        | (_, nextOutputs, nextIp, nextRelBase, nextExtMem) when nextIp < 0 -> (nextOutputs, nextIp, Finished, nextRelBase, nextExtMem)
        | (_, nextOutputs, nextIp, nextRelBase, nextExtMem) when nextIp = ip -> (nextOutputs, nextIp, WaitingForInput, nextRelBase, nextExtMem)
        | (nextInput, nextOutputs, nextIp, nextRelBase, nextExtMem) -> solve (nextInput, nextOutputs, nextIp, mem, nextRelBase, nextExtMem)

    let initialize (program: int64 array) =
        let (outputs, ip, state, relBase, extMem) = solve (None, [], 0, program, 0, Map.empty)
        {
            State = state;
            InstructionPointer = ip;
            Outputs = outputs;
            Memory = program;
            RelativeBase = relBase;
            ExtendedMemory = extMem;
        }

    let input value computer =
        match computer with
        | { Computer.State = Finished } -> computer
        | _ ->
            let (outputs, ip, state, relBase, extMem) = solve ((Some value), computer.Outputs, computer.InstructionPointer, computer.Memory, computer.RelativeBase, computer.ExtendedMemory)
            {
                State = state;
                InstructionPointer = ip;
                Outputs = outputs;
                Memory = computer.Memory;
                RelativeBase = relBase;
                ExtendedMemory = extMem;
            }

    let program =
        File.ReadAllLines("input.txt")
        |> Array.exactlyOne
        |> fun line -> line.Split(',')
        |> Array.map int64
    
    let rec paint computer loc dir (paintMap: Map<int * int, int>) =
        let inputColor = match Map.tryFind loc paintMap with | Some 1 -> 1 | _ -> 0
        let nextComputer = input (int64 inputColor) computer
        match nextComputer with
        | { Computer.Outputs = moveInstruction::paintInstruction::_; Computer.State = state; } ->
            let nextPaintMap = Map.add loc (int paintInstruction) paintMap
            let nextDir =
                match (int moveInstruction) with
                | 0 -> turnLeft dir
                | 1 -> turnRight dir
                | _ -> failwith "Invalid move instruction"
            let nextLoc = move loc nextDir
            match state with
            | Finished -> nextPaintMap
            | WaitingForInput -> paint nextComputer nextLoc nextDir nextPaintMap
        | _ -> failwith "Unexpected number of outputs"

    paint (initialize (Array.copy program)) (0, 0) Up Map.empty
    |> Map.count
    |> printfn "Anwer 1: %i"

    let paintMap = [((0, 0), 1)] |> Map.ofList
    let registrationNo = paint (initialize (Array.copy program)) (0, 0) Up paintMap

    let minX = registrationNo |> Map.toSeq |> Seq.map (fst >> fst) |> Seq.min
    let maxX = registrationNo |> Map.toSeq |> Seq.map (fst >> fst) |> Seq.max
    let minY = registrationNo |> Map.toSeq |> Seq.map (fst >> snd) |> Seq.min
    let maxY = registrationNo |> Map.toSeq |> Seq.map (fst >> snd) |> Seq.max

    printfn "Answer 2:"
    for y = maxY downto minY do
        for x = minX to maxX do
            match Map.tryFind (x, y) registrationNo with
            | Some 1 -> printf "#"
            | _ -> printf " "
        printf "\n"

    Console.ReadKey() |> ignore
    0 // return an integer exit code
