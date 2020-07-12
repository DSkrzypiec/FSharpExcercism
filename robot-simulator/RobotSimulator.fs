module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { CurrentDirection: Direction; CurrentPosition: Position }

let create (direction: Direction) (position: Position) =
    { CurrentDirection = direction; CurrentPosition = position } 

let changeDirection (instruction: char) (direction: Direction): Direction =
    match (instruction, direction) with
    | ('A', _) -> direction
    | ('R', North) -> East
    | ('R', East) -> South
    | ('R', South) -> West
    | ('R', West) -> North
    | ('L', North) -> West
    | ('L', East) -> North
    | ('L', South) -> East
    | ('L', West) -> South
    | (_, _) -> direction

let increasePosition (pos: Position) (currentDirection: Direction) (newDirection: Direction): Position =
    let x = fst pos
    let y = snd pos

    if newDirection <> currentDirection then 
        (x, y)
    else
        match newDirection with
        | North -> (x, y + 1)
        | South -> (x, y - 1)
        | East -> (x + 1, y)
        | West -> (x - 1, y)

let moveSingle (instruction: char) (robot: Robot): Robot =
    let newDir = changeDirection instruction robot.CurrentDirection
    let newPos = increasePosition robot.CurrentPosition robot.CurrentDirection newDir
    { CurrentDirection = newDir; CurrentPosition = newPos }

let move (instructions: string) (robot: Robot) = 
    instructions
        |> Seq.toList
        |> List.fold (fun robot instr -> moveSingle instr robot) robot