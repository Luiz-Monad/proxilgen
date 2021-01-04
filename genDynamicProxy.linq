<Query Kind="FSharpProgram">
  <Reference Relative="..\ILPack\src\bin\Debug\netcoreapp2.1\Lokad.ILPack.dll">C:\extern\lab\ILPack\src\bin\Debug\netcoreapp2.1\Lokad.ILPack.dll</Reference>
  <Reference>&lt;NuGet&gt;\system.runtime.caching\4.7.0\lib\netstandard2.0\System.Runtime.Caching.dll</Reference>
  <RemoveNamespace>System.Data</RemoveNamespace>
  <RemoveNamespace>System.Diagnostics</RemoveNamespace>
  <RemoveNamespace>System.IO</RemoveNamespace>
  <RemoveNamespace>System.Linq</RemoveNamespace>
  <RemoveNamespace>System.Linq.Expressions</RemoveNamespace>
  <RemoveNamespace>System.Reflection</RemoveNamespace>
  <RemoveNamespace>System.Text</RemoveNamespace>
  <RemoveNamespace>System.Text.RegularExpressions</RemoveNamespace>
  <RemoveNamespace>System.Threading</RemoveNamespace>
  <RemoveNamespace>System.Transactions</RemoveNamespace>
  <RemoveNamespace>System.Xml</RemoveNamespace>
  <RemoveNamespace>System.Xml.Linq</RemoveNamespace>
  <RemoveNamespace>System.Xml.XPath</RemoveNamespace>
</Query>


let dump<'T> (s: string) (o:'T) : 'T = (box o).Dump(s) |> ignore; o

module Dict =

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
        
module Cache =

    open System.Runtime.Caching

    type Memoization = Forever | Sliding of System.TimeSpan

    /// Lambda memoization.
    let memoize expiry fn arg : 'T =
        let m = MemoryCache.Default
        let p =
            let pol = CacheItemPolicy ()
            match expiry with
            | Sliding e ->
                pol.SlidingExpiration <- e
            | Forever ->
                pol.Priority <- CacheItemPriority.NotRemovable
            pol
        let k = sprintf "%A" arg
        let create () =
            let r : 'T = fn arg
            m.AddOrGetExisting ( k, r, p ) |> ignore
            r
        ( m.Contains(k), m.GetCacheItem(k) )
        |> Option.ofTupleBool
        |> Option.map ( fun i -> unbox i.Value )
        |> Option.defaultWith create
    
open System.Reflection
open System.Reflection.Emit

let specializeTypeFor ( param: Type ) ( t: Type ) =
    match t.IsGenericTypeDefinition with
    | true -> t.MakeGenericType [| param |]
    | _ -> t

[<AutoOpen>]
module Emit =

    let private guid () = Guid.NewGuid().ToString().Replace("-", "")

    let private makeDynamicModule () =
        let assemblyName = sprintf "dynamic_proxy_%s" <| guid ()
        let moduleName = sprintf "dynamic_module_%s" <| guid ()
        let definition = AssemblyName ()
        definition.Name <- assemblyName
        definition.Version <- new Version(1, 0)
        let assembly = AssemblyBuilder.DefineDynamicAssembly ( definition, AssemblyBuilderAccess.RunAndCollect )
        ( definition, AssemblyBuilderAccess.RunAndCollect ) |> dump "makeDynamicModule" |> ignore
        assembly.DefineDynamicModule ( moduleName )

    let private makeType ( baseTypeOrInterface : Type ) ( builder: ModuleBuilder ) =
        let attributes = 
            TypeAttributes.AutoClass ||| 
            TypeAttributes.Class ||| 
            TypeAttributes.Public ||| 
            TypeAttributes.BeforeFieldInit
        let typeName = sprintf "%s_proxy_%s" <| baseTypeOrInterface.Name <| guid ()
        let baseInterfaces = baseTypeOrInterface.GetInterfaces () |> List.ofSeq
        let ( baseType, baseInterfaces ) =
            match baseTypeOrInterface.IsInterface with
            | true -> ( typeof<obj>, [ baseTypeOrInterface ] @ baseInterfaces )
            | _ -> ( baseTypeOrInterface, baseInterfaces )
        ( typeName, attributes, baseType, baseInterfaces ) |> dump "makeType" |> ignore
        builder.DefineType ( typeName, attributes, baseType, Array.ofSeq baseInterfaces )

    let private makeCtor parameters ( builder: TypeBuilder ) =
        let attributes =
            MethodAttributes.Public |||
            MethodAttributes.SpecialName |||
            MethodAttributes.RTSpecialName |||
            MethodAttributes.HideBySig
        let convention =
            CallingConventions.HasThis
        ( attributes, convention, parameters ) |> dump "makeCtor" |> ignore
        builder.DefineConstructor ( attributes, convention, parameters )

    let private makeCtorParam ix name ( builder: ConstructorBuilder ) =
        let attributes =
            ParameterAttributes.None
        ( ix, attributes, name ) |> dump "makeCtorParam" |> ignore
        builder.DefineParameter ( ix, attributes, name )

    let private makeField name fieldType ( builder: TypeBuilder ) =
        let attributes =
            FieldAttributes.Private
        let fieldName = sprintf "__field_%s" name
        ( fieldName, fieldType, attributes ) |> dump "makeField" |> ignore
        builder.DefineField ( fieldName, fieldType, attributes )
        
    let private makeProperty name propertyType ( builder: TypeBuilder ) =
        let attributes =
            PropertyAttributes.HasDefault
        ( name, attributes, propertyType, Array.empty ) |> dump "makeProperty" |> ignore
        builder.DefineProperty ( name, attributes, propertyType, Array.empty )

    let private makeMethod name retType parameters attributes ( builder: TypeBuilder ) =
        let attributes =
            attributes |||
            MethodAttributes.Public ||| 
            MethodAttributes.Virtual ||| 
            MethodAttributes.NewSlot
        ( name, attributes, retType, parameters ) |> dump "makeMethod" |> ignore
        builder.DefineMethod ( name, attributes, retType, parameters )

    let private makeOverride ( builder: TypeBuilder ) ( mbase: MethodInfo ) ( meth: MethodBuilder ) =
        ( string meth, mbase ) |> dump "makeOverride" |> ignore
        builder.DefineMethodOverride ( meth, mbase )
        meth

    [<NoComparison>]
    type private OpCodeArg =
        | OpLdfld of FieldInfo
        | OpStfld of FieldInfo
        | OpLdarg of int
        | OpCallctor of ConstructorInfo
        | OpCallmeth of MethodInfo
        | OpTail
        | OpRet

    let private makeOpCodesEmit opcodes ( il: ILGenerator ) =
        opcodes |> dump "makeOpCodesEmit" |> ignore
        for op in opcodes do
            match op with
            | OpRet -> il.Emit(OpCodes.Ret)
            | OpTail -> il.Emit(OpCodes.Tailcall)
            | OpLdfld field -> il.Emit(OpCodes.Ldfld, field)
            | OpStfld field -> il.Emit(OpCodes.Stfld, field)
            | OpLdarg 0 -> il.Emit(OpCodes.Ldarg_0)
            | OpLdarg 1 -> il.Emit(OpCodes.Ldarg_1)
            | OpLdarg 2 -> il.Emit(OpCodes.Ldarg_2)
            | OpLdarg 3 -> il.Emit(OpCodes.Ldarg_3)
            | OpLdarg n -> il.Emit(OpCodes.Ldarg_S, (byte) n);
            | OpCallctor ctor -> il.Emit(OpCodes.Call, ctor)
            | OpCallmeth method when method.IsVirtual -> il.EmitCall(OpCodes.Callvirt, method, null)
            | OpCallmeth method -> il.EmitCall(OpCodes.Call, method, null)

    let inline private makeOpCodes opcodes m =
        let getIlGen i = (^B : ( member GetILGenerator: unit -> ILGenerator ) (i))
        getIlGen m |> makeOpCodesEmit opcodes
        m

    let private makeMethodFrom instField ( source: MethodInfo ) ( target: MethodInfo ) ( builder: TypeBuilder ) =
        ( string instField, source, target ) |> dump "makeMethodFrom" |> ignore
        let overrides = builder |> makeOverride 
        let retType = source.ReturnType
        let parameters = 
            source.GetParameters ()
            |> Seq.map ( fun p -> p.ParameterType )
            |> Array.ofSeq
        let attributes =
            MethodAttributes.Public
        builder
        |> makeMethod source.Name retType parameters attributes
        |> makeOpCodes (seq { 
            if not target.IsStatic then yield! [|
                OpLdarg 0
                OpLdfld instField
            |]
            if not <| Seq.isEmpty parameters then
                yield OpLdarg <| Seq.length parameters
            yield! [|
                OpTail
                OpCallmeth target
                OpRet
            |]
        }) |> overrides source |> ignore

    let private makePropertyFrom ( instField: FieldInfo ) ( source: PropertyInfo ) ( target: PropertyInfo ) ( builder: TypeBuilder ) =
        ( string instField, source, target ) |> dump "makePropertyFrom" |> ignore
        let name = source.Name
        let propType = source.PropertyType
        let property = builder |> makeProperty name propType
        let overrides = builder |> makeOverride 
        let attributes =
            MethodAttributes.HideBySig |||
            MethodAttributes.SpecialName
        if source.CanRead then
            let targetGet = target.GetGetMethod ()
            let getName = "get_" + name
            builder
            |> makeMethod getName propType Array.empty attributes 
            |> makeOpCodes (seq { 
                if not targetGet.IsStatic then yield! [|
                    OpLdarg 0
                    OpLdfld instField
                |]
                yield! [|
                    OpTail
                    OpCallmeth targetGet
                    OpRet
                |]
            }) |> overrides ( source.GetGetMethod () ) |> property.SetGetMethod
        if source.CanWrite then
            let targetSet = target.GetSetMethod ()
            let setName = "set_" + name
            builder
            |> makeMethod setName null [| propType |] attributes 
            |> makeOpCodes (seq { 
                if not targetSet.IsStatic then yield! [|
                    OpLdarg 0
                    OpLdfld instField
                |]
                yield! [|
                    OpLdarg 1
                    OpTail
                    OpCallmeth targetSet
                    OpRet
                |]
            }) |> overrides ( source.GetSetMethod () ) |> property.SetSetMethod

    let private makeCtorWith instField ( param: Type ) ( builder: TypeBuilder ) =
        ( string instField, param ) |> dump "makeCtorWith" |> ignore
        let baseCtor = typeof<obj>.GetConstructors() |> Seq.head
        let ctor = builder |> makeCtor [| param |]
        ctor |> makeCtorParam 0 param.Name |> ignore
        ctor
        |> makeOpCodes [|
            OpLdarg 0
            OpCallctor baseCtor
            OpLdarg 0
            OpLdarg 1
            OpStfld instField
            OpRet
        |] |> ignore

    [<NoComparison; RequireQualifiedAccess>]
    type MemberInfoMap = {
        Source: MemberInfo
        Target: MemberInfo
    }
    
    let genDynamicProxy baseTypeOrInterface ( impl: MemberInfoMap list ) ( proxiedType: Type ) =
        ( baseTypeOrInterface, impl, proxiedType ) |> dump "genDynamicProxy" |> ignore
        let mainModule = makeDynamicModule ()
        let typeBuilder = mainModule |> makeType baseTypeOrInterface
        let backingField = typeBuilder |> makeField "wrapped" proxiedType
        typeBuilder |> makeCtorWith backingField proxiedType
        for meth in impl do
            match ( meth.Source, meth.Target ) with
            | (:? MethodInfo as mis), (:? MethodInfo as mit) -> 
                typeBuilder |> makeMethodFrom backingField mis mit
            | (:? PropertyInfo as pis), (:? PropertyInfo as pit)  ->
                typeBuilder |> makePropertyFrom backingField pis pit
            | _ -> ()
        let ty = typeBuilder.CreateType ()
        let gen = Lokad.ILPack.AssemblyGenerator ()
        let f = System.IO.Path.GetTempFileName () |> dump "genDynamicProxy"
        gen.GenerateAssembly ( ty.Assembly, f )
        ty
     
let private isSpecial ( m: MemberInfo ) = 
    match m with
    | :? MethodInfo as mi -> mi.IsSpecialName
    | :? PropertyInfo as pi -> pi.IsSpecialName
    | _ -> false

let private getMembers ( t: Type ) =
    let binding =
        BindingFlags.Public |||
        BindingFlags.Static |||
        BindingFlags.Instance |||
        BindingFlags.FlattenHierarchy
    t.GetMembers binding
    |> Seq.filter (not << isSpecial)

let private typeName ( t: Type ) = t.FullName

let private paramKey ( p: ParameterInfo ) = 
    ( p.Position, typeName p.ParameterType )

let private key ( m: MemberInfo ) = 
    let v =
        match m with
        | :? MethodInfo as mi -> 
            ( List.ofSeq << Seq.map paramKey
                <| mi.GetParameters (), 
              Some <| typeName mi.ReturnType )
        | :? PropertyInfo as pi -> 
            ( List.empty, Some <| typeName pi.PropertyType )
        | _ -> 
            ( List.empty, None )
    ( m.Name, m.MemberType, v )
    
let private index =
    let tkey v = ( key v, v )
    Map.ofSeq << Seq.map tkey

let typeStructureOf<'T> ( t: Type ) : Map<_, MemberInfoMap> =
    let templ = typedefof<'T> |> specializeTypeFor t
    let tmemb = templ |> getMembers |> index //|> dump "templ"
    let memb = t |> getMembers |> index //|> dump "memb"
    Map.zipMap tmemb memb <| fun _ l r -> {
        Source = l
        Target = r
    }
    //|> dump "typeStructureOf"

let private typeHasStructureOfCore<'T> ( t: Type ) =
    let templ = typedefof<'T> |> specializeTypeFor t
    let tmemb = templ |> getMembers |> index
    let mmemb = typeStructureOf<'T> t
    ( Map.count mmemb = Map.count tmemb )
    |> Option.ofBool
    |> Option.mapValue t
    //|> dump "typeHasStructureOfCore"

let typeHasStructureOf<'T> t =
    let generic gt =
        let reflected = typeHasStructureOfCore<'T>
        let fr = Cache.memoize Cache.Forever reflected
        //fr |> dump (sprintf "reflected %A" gt) |> ignore
        fr
    let fg = Cache.memoize Cache.Forever generic
    //fg typedefof<'T> |> dump (sprintf "generic %A" typedefof<'T>) |> ignore
    fg typedefof<'T> t
   
let staticTarget<'T> = Unchecked.defaultof<obj>

let private adaptStructureForCore<'T> ( t: Type ) ( o: obj ) : 'T =
    let templ = typedefof<'T> |> specializeTypeFor t
    let mmemb = typeStructureOf<'T> t
    let memb = 
        mmemb
        |> Seq.map ((|KeyValue|) >> snd)
        |> List.ofSeq
    let proxy = o.GetType () |> Emit.genDynamicProxy templ memb
    Activator.CreateInstance ( proxy, [| o |] ) |> unbox
    
let adaptStructureFor<'T> t ( o: obj ) =
    let generic _ =
        let reflected = adaptStructureForCore<'T>
        let fr = Cache.memoize Cache.Forever reflected
        fr
    let fg = Cache.memoize Cache.Forever generic
    fg typedefof<'T> t o

///////////////////////////////////////////////////////////////////////

//type Url = Url of System.Uri | Invalid
//with
//    member inline this.IsValid = ( function | Url _ -> true | _ -> false ) this
//    member inline this.String = ( function | Url u -> string u | _ -> "" ) this
//    member inline this.Value = ( function | Url u -> u | _ -> failwith "invalid" ) this
//    static member inline Parse (s: string) =
//        Uri.TryCreate ( s, UriKind.Absolute )
//        |> Option.ofTupleBool |> Option.either Url Invalid
//    static member inline empty = Invalid
//
//type CustomizedEntity<'T> = 
//    abstract String: string with get
//    abstract Parse: str: string -> 'T 

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
    member this.Do () = this.Dump("Do"); ()
    member this.Do2 (i: int) = this.Dump("Do2"); ()
    member this.Fun (x: string) = this.String.Length

type CustomizedEntity<'T> = 
    abstract String: string with get, set
    abstract Parse: str: string -> 'T 
    abstract empty: 'T
    abstract Do: unit -> unit
    abstract Do2: o: int -> unit
    abstract Fun: str: string -> int


//typeStructureOf<CustomizedEntity<_>> typeof<Url> |> dump "? <CustomizedEntity<_>>"
//typeStructureOf<CustomizedEntity<Url>> typeof<Url> |> dump "? <CustomizedEntity<Url>>"
//typeStructureOf<CustomizedEntity<obj>> typeof<Url> |> dump "? <CustomizedEntity<obj>>"
//typeStructureOf<CustomizedEntity<Url>> typeof<Url> |> dump "? <CustomizedEntity<Url>>"

let o = Url.Parse "https://fuck.it/"
adaptStructureFor<CustomizedEntity<Url>> typeof<Url> o |> dump "= <CustomizedEntity<_>>"

let test = adaptStructureFor<CustomizedEntity<Url>> typeof<Url> o 

test.String |> dump "test1"

test.String <- "http://google.com"
test |> dump "test2"
o |> dump "test2 :: orig"

test.empty |> dump "test3"

test.Parse "http://test.ts:600/" |> dump "test4"

test.Do () |> dump "test5"

test.Do2 1 |> dump "test6"

test.Fun "x" |> dump "test7"
