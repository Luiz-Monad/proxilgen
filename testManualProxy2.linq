<Query Kind="FSharpProgram" />


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

type Url = Url of ref<System.Uri> | Invalid
with
    member this.IsValid = ( function | Url _ -> true | _ -> false ) this
    static member Parse (s: string) =
        Uri.TryCreate ( s, UriKind.Absolute )
        |> Option.ofTupleBool |> Option.either (Url << ref) Invalid
    member this.Value 
        with get () = ( function | Url u -> !u | _ -> failwith "invalid" ) this
        and set v = ( function | Url u -> u := v | _ -> failwith "invalid" ) this        
    member this.String 
        with get () = ( function | Url u -> string !u | _ -> "" ) this
        and set s = ( function | Url u -> u := Url.Parse(s).Value | _ -> failwith "invalid" ) this
    static member empty = Invalid
    member this.Do () = ()
    member this.Do2 (i: int) = ()
    member this.Fun (x: string) = 0

type CustomizedEntity<'T> = 
    abstract String: string with get, set
    abstract Parse: str: string -> 'T 
    abstract empty: 'T
    abstract Do: unit -> unit
    abstract Do2: o: int -> unit
    abstract Fun: str: string -> int

type RealEntity (url: Url) = 
    let mutable url = url
    interface CustomizedEntity<Url> with
        member this.String 
            with get () = url.String
            and set str = url.String <- str
        member this.Parse str = Url.Parse str
        member this.empty = Url.empty
        member this.Do () = url.Do ()
        member this.Do2 o = url.Do2 o
        member this.Fun str = url.Fun str

    
