module day12

open utils
open System

type Direction = N | S | W | E
type Rotation = L | R
type Action = Direction of Direction | Rotation of Rotation | Forward
type Command = { Action: Action; Value : int }
type Waypoint = int * int

let rec rotateDirection (d: Direction) (r: Rotation * int) = 
    let rotate (direction: Direction) (rotation: Rotation)  =
        match (direction,rotation) with
            | (N,R) -> E
            | (W,R) -> N
            | (S,R) -> W
            | (E,R) -> S
            | (N,L) -> W
            | (W,L) -> S
            | (S,L) -> E
            | (E,L) -> N
    let noOfRotation = (snd r)/90
    let res = rotate d (fst r)
    if noOfRotation = 1 then res
    else rotateDirection res (fst r,(noOfRotation-1)*90)

let mapToCommand (line:string) : Command =
    let firstChar = line.[0]
    let value = Int32.Parse(line.[1..])
    match firstChar with
        | 'N' -> { Action = Action.Direction N; Value = value}
        | 'W' -> { Action = Action.Direction W; Value = value }
        | 'S' -> { Action = Action.Direction S; Value = value }
        | 'E' -> { Action = Action.Direction E; Value = value }
        | 'L' -> { Action = Action.Rotation L; Value = value }
        | 'R' -> { Action = Action.Rotation R; Value = value }
        | 'F' -> { Action = Action.Forward; Value = value }
        | _ ->  raise <| new ArgumentException("invald arg")

let moveWaypoint (waypoint: Waypoint) (c: Command) : Waypoint =
        match c.Action with
            | Action.Direction N ->  (fst waypoint + c.Value, snd waypoint)
            | Action.Direction S ->  (fst waypoint - c.Value, snd waypoint)
            | Action.Direction W ->  (fst waypoint, snd waypoint + c.Value)
            | Action.Direction E ->  (fst waypoint, snd waypoint - c.Value)
            | Action.Rotation r -> match rotateDirection N (r, c.Value) with
                                    | N -> waypoint
                                    | S -> (- fst waypoint, - snd waypoint)
                                    | W -> (- snd waypoint, fst waypoint)
                                    | E -> (snd waypoint, -fst waypoint)
            | _ -> raise <| new ArgumentException("invald arg")

let translateWaypointToCommands (waypoint: Waypoint) (moveValue: int) : Command list =
    let commands = List.empty<Command>
    let (ns,we) = waypoint
    let factor = Math.Abs(moveValue)
    let updated = if ns > 0 then { Action = Action.Direction N; Value = ns * factor}::commands
                  else { Action = Action.Direction S; Value = -ns * factor}::commands
    if we > 0 then { Action = Action.Direction W; Value = we * factor}::updated
              else { Action = Action.Direction E; Value = -we * factor}::updated
            
let loadDataAsCommands () =
    readLines("data\\12-01.txt") |> Seq.map(mapToCommand) 

let aggreateResults commands =
    let grouped = commands |> List.groupBy (fun g -> g.Action) |> List.map(fun (g,e) -> (g, e |> List.sumBy(fun x -> x.Value)))
    let agg = grouped |>
        List.fold(fun acc g -> 
                                match fst g with
                                    | Action.Direction N ->  (fst acc + snd g, snd acc)
                                    | Action.Direction S ->  (fst acc - snd g, snd acc)
                                    | Action.Direction W ->  (fst acc, snd acc + snd g)
                                    | Action.Direction E ->  (fst acc, snd acc - snd g)
                                    | _ -> raise <| new ArgumentException("should not happen")) (0,0)
    let (ns,ew) = agg
    Math.Abs(ns) + Math.Abs(ew)    

let solution1 () =
    let commands = loadDataAsCommands () |> Seq.toArray
    let mappedCommands = commands |> 
        Array.fold (fun acc x -> 
                                  let (prevDirection, prevCommands) = acc
                                  let prevDirectionAction = prevDirection |> Action.Direction
                                  let rotate = rotateDirection prevDirection
                                  match x.Action with
                                    | Action.Forward ->  (prevDirection, {Action = prevDirectionAction; Value = x.Value}::prevCommands)
                                    | Action.Rotation r -> ((rotate (r,x.Value)), prevCommands)
                                    | Action.Direction d -> (prevDirection, x::prevCommands)) (E, List.empty<Command>)
    aggreateResults (snd mappedCommands) 

let solution2 () =
    let commands = loadDataAsCommands () |> Seq.toArray
    let mappedCommands = commands |> 
        Array.fold (fun acc x -> 
                                  let (prevWaypoint, prevCommands) = acc
                                  match x.Action with
                                    | Action.Forward -> (prevWaypoint, translateWaypointToCommands prevWaypoint x.Value |> List.append prevCommands)
                                    | _ -> (moveWaypoint prevWaypoint x, prevCommands )) ((1,-10), List.empty<Command>)
    aggreateResults (snd mappedCommands)