namespace ProxILGen

module Dict =

    open System.Collections.Generic

    let tryGetValue k (dct: IDictionary<'Key, 'Value>) =
        match dct.TryGetValue k with
        | true, v -> Some v
        | _       -> None
        
    let zip (x: IDictionary<'Key, 'T1>) (y: IDictionary<'Key, 'T2>) =
        let dct = Dictionary<'Key, 'T1 * 'T2> ()
        for KeyValue(k, vx) in x do
            match tryGetValue k y with
            | Some vy -> dct.Add (k, (vx, vy))
            | None    -> ()
        dct :> IDictionary<'Key, 'T1 * 'T2>

module Map =

    let zip (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) = Map <| seq {
        for KeyValue(k, vx) in x do
            match Map.tryFind k y with
            | Some vy -> yield (k, (vx, vy))
            | None    -> () }

    let zipMap (x: Map<'Key, 'T1>) (y: Map<'Key, 'T2>) mapper = Map <| seq {
        for KeyValue(k, vx) in x do
            match Map.tryFind k y with
            | Some vy -> yield ( k, mapper k vx vy )
            | None    -> () }
            
module Option =

    let ofBool b =
        if b
        then Some () else None

    let mapValue value option =
        Option.map (fun _ -> value) option

    let ofTupleBool (b, v) =
        if b
        then Some v else None

    let either orSome orNone = function
        | Some s -> orSome s
        | _ -> orNone
        