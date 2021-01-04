namespace ProxILGen

open System
open System.Reflection

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

module Emit =

    open System.Reflection.Emit

    let private guid () = Guid.NewGuid().ToString().Replace("-", "")

    let private makeDynamicModule () =
        let assemblyName = sprintf "dynamic_proxy_%s" <| guid ()
        let moduleName = sprintf "dynamic_module_%s" <| guid ()
        let definition = AssemblyName ()
        definition.Name <- assemblyName
        definition.Version <- new Version(1, 0)
        let assembly = AssemblyBuilder.DefineDynamicAssembly ( definition, AssemblyBuilderAccess.RunAndCollect )
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
        builder.DefineType ( typeName, attributes, baseType, Array.ofSeq baseInterfaces )

    let private makeCtor parameters ( builder: TypeBuilder ) =
        let attributes =
            MethodAttributes.Public |||
            MethodAttributes.SpecialName |||
            MethodAttributes.RTSpecialName |||
            MethodAttributes.HideBySig
        let convention =
            CallingConventions.HasThis
        builder.DefineConstructor ( attributes, convention, parameters )

    let private makeCtorParam ix name ( builder: ConstructorBuilder ) =
        let attributes =
            ParameterAttributes.None
        builder.DefineParameter ( ix, attributes, name )

    let private makeField name fieldType ( builder: TypeBuilder ) =
        let attributes =
            FieldAttributes.Private
        let fieldName = sprintf "__field_%s" name
        builder.DefineField ( fieldName, fieldType, attributes )
        
    let private makeProperty name propertyType ( builder: TypeBuilder ) =
        let attributes =
            PropertyAttributes.HasDefault
        builder.DefineProperty ( name, attributes, propertyType, Array.empty )

    let private makeMethod name retType parameters attributes ( builder: TypeBuilder ) =
        let attributes =
            attributes |||
            MethodAttributes.Public ||| 
            MethodAttributes.Virtual ||| 
            MethodAttributes.NewSlot
        builder.DefineMethod ( name, attributes, retType, parameters )

    let private makeOverride ( builder: TypeBuilder ) ( mbase: MethodInfo ) ( meth: MethodBuilder ) =
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
        typeBuilder.CreateType ()

    let saveProxy ( ty: Type ) path =
        let gen = Lokad.ILPack.AssemblyGenerator ()
        gen.GenerateAssembly ( ty.Assembly, path )

module Generator =
    
    open Emit
         
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
        
    let specializeTypeFor ( param: Type ) ( t: Type ) =
        match t.IsGenericTypeDefinition with
        | true -> t.MakeGenericType [| param |]
        | _ -> t

    let typeStructureOf<'T> ( t: Type ) : Map<_, MemberInfoMap> =
        let templ = typedefof<'T> |> specializeTypeFor t
        let tmemb = templ |> getMembers |> index
        let memb = t |> getMembers |> index
        Map.zipMap tmemb memb <| fun _ l r -> {
            Source = l
            Target = r
        }

    let private typeHasStructureOfCore<'T> ( t: Type ) =
        let templ = typedefof<'T> |> specializeTypeFor t
        let tmemb = templ |> getMembers |> index
        let mmemb = typeStructureOf<'T> t
        ( Map.count mmemb = Map.count tmemb )
        |> Option.ofBool
        |> Option.mapValue t

    let typeHasStructureOf<'T> t =
        let generic gt =
            let reflected = typeHasStructureOfCore<'T>
            let fr = Cache.memoize Cache.Forever reflected
            fr
        let fg = Cache.memoize Cache.Forever generic
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
