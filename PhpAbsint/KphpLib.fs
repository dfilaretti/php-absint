namespace PhpAbsint.KphpLib

(* Locations *)

/// Memory locations as simple integers
type MemLoc = int
/// Semantics-level locations
type SpecialLoc = LocNull
/// Locations consist of actual memory locations together with special ones
type Loc = 
    | MemLoc of MemLoc 
    | SpecialLoc of SpecialLoc
/// Hashtable keys are int or strings
type Key = 
    | IntKey of int
    | StringKey of string
    | UndefKey // TODO: use option instead?

(* HashTables *)

type KvPair = Key * MemLoc // TODO: generalise?

type HashTable = 
    { elements: KvPair list
      current: int option
      nextIntKey: int }

(* Values *)

type PhpType = 
    | TInt
    | TFloat 
    | TString
    | TBool
    | TArray
    //| TObject (todo)
    | TUndefined
    
type PhpValue = 
    | Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Array of HashTable
    //| Object of (todo)
    | Null
    
type Zval = 
    { Value: PhpValue
      Type: PhpType
      RefCount: int }

(* Heap *)

type Heap = 
    { Map: Map<MemLoc, Zval>;
      // TODO: use closure for NextFreshLoc! 
      NextFreshLoc: MemLoc; }

(* References *)

type Ref = 
    | BasicRef of Loc * Key 
    | ComplexRef of Ref * Key * RefType
and RefType = 
    | ScalarRef
    | ArrayRef
    | ObjectRef

[<RequireQualifiedAccess>]
module MyDict = // TODO: move somewhere else (e.g. lib)?
    let rec dictRef kvs k = 
        match kvs with 
        | [] -> None
        | (k', v) :: rest -> 
            if (k = k') then
                Some v
            else
                (dictRef rest k)

    let rec dictSet kvs k v = 
        match kvs with 
        | [] -> [(k, v)]
        | (k', v') :: rest -> 
            if (k' = k) then
                (k, v) :: rest
            else
                (k', v') :: (dictSet rest k v)

    let rec dictHasKey kvs k = 
        match (dictRef kvs k) with 
        | Some _ -> true
        | None -> false

    let rec dictKeys kvs = 
        List.map fst kvs

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HashTable = 
    let emptyHashTable = 
        { elements = []; 
          current = None; 
          nextIntKey = 0 }

    /// Gets the content of the hashtable for the given key.
    /// if none is found 'NULL is returned instead
    /// TODO: what doesk KPHP do in the "null" case?
    let readValue ht k = 
        match (MyDict.dictRef ht.elements k) with 
        | None -> SpecialLoc LocNull
        | Some l -> MemLoc l

    /// checks whether the hashtable contains a key-value pair with given key
    let hasKey ht k = 
        MyDict.dictHasKey ht.elements k
        
    /// inserts the given key-value pair into the hashtable
    /// (if the key-value pair is already present, the value is updated).
    /// If the inserted key is an integer, it updates the next-int-key accordingly.
    /// TODO: should update current!
    let insertKeyValue k l ht = 
        { ht with 
            elements = MyDict.dictSet ht.elements k l;
            nextIntKey = 
                match k with 
                | IntKey i -> 
                    if (i >= ht.nextIntKey) then 
                        i + 1
                    else
                        ht.nextIntKey
                | _ -> ht.nextIntKey
        }

    /// inserts a value into an hashtable, without specifying a key
    /// we'll then pick the next available integer key (as in PHP) 
    let insertValue l ht = 
        insertKeyValue (IntKey ht.nextIntKey) l ht

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Zval = 
    // TODO: move to a (new) PhpValue module of some sort05:50 PM    let getType v = 
    let getType v =     
        match v with 
        | Int _ -> TInt
        | Float _ -> TFloat
        | String _ -> TString
        | Bool _ -> TBool
        | Array _ -> TArray
        | Null -> TUndefined

    /// make a function that applies the given unary function to refcount
    let updateRefcountWithUnaryFun f = 
        fun z -> {z with RefCount = f z.RefCount}

    /// Makes a new Zval containing the given value
    let makeZval v = 
        {Value = v; Type = getType v; RefCount = 0}

    /// A "Null" Zval
    let nullZval = 
        Null |> makeZval
    
    /// An "empty array" Zval
    let emptyArrayZval = 
        Array HashTable.emptyHashTable |> makeZval 

    /// Replaces the value of a zval.
    /// The Type field is refreshed accordingly
    let replaceValue z v =
        {z with Value = v; Type = getType v}
       
    let incRefcount = 
        updateRefcountWithUnaryFun (fun n -> n + 1)
    
    let decRefcount = 
        updateRefcountWithUnaryFun (fun n -> n - 1)

    /// checks whether a zval is aliased, i.e. for us, whether refcount > 1
    /// NB: we can do this since we don't do copy-on-write.
    /// In zend they use 'isref' instead
    let isAliased z = 
        z.RefCount > 1

    /// Whether the RefCount of this Zval is nonzero
    let refCountNonzero z = 
        z.RefCount > 0

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Heap = 
    let emptyHeap = { Map = Map.empty; NextFreshLoc = 0 }

    /// Returns true if l is present in h
    let locInUse h l = Map.containsKey l h.Map

    /// Returns the contents of memory at the given location.
    /// NB throws KeyNotFound exception if l not present
    let readLoc h l = 
        h.Map.[l]
    
    let freshLoc h = 
        let incNextFreshLoc h = 
            let n = h.NextFreshLoc
            { h with NextFreshLoc = n + 1 }

        let freshLoc = h.NextFreshLoc

        (h |> incNextFreshLoc, freshLoc)

    // Stores a Zval into a given Location.
    // If the locations already contagins a Zval, it is replaces (no duplicates!) 
    // Returns updated memory
    let storeZvalInLoc l z h = { h with Map = h.Map.Add (l, z) }

    // Stores a Zval into a fresh location
    // Returns the updated memory
    let storeZval z h = 
        let (h', freshLoc) = freshLoc h
        (storeZvalInLoc freshLoc z h' , freshLoc)

// TODO: maybe this could go on its own file???
module Memory = 
    /// Returns the contents of memory at the given location (value only)
    let memRead h l = (Heap.readLoc h l).Value

    /// Updates the value inside the zval at location loc. 
    let memUpdate h l v = 
        let z = Heap.readLoc h l
        let z' = Zval.replaceValue z v
        Heap.storeZvalInLoc l z' h

    // Inserts a new key-value pair into the hashtable located at scope-loc
    // Returns the updated heap. If scope-loc does not contain a hashtable,
    // the initial unomodified heap is returned insteaf. TODO: contract?
    let memInsertKv heap scopeLoc key loc = 
        let z = memRead heap scopeLoc
        match z with 
        | PhpValue.Array ht ->
            let ht' = HashTable.insertKeyValue key loc ht
            memUpdate heap scopeLoc (PhpValue.Array ht')
        | _ -> invalidOp "ERROR: location doesn't contain a hashtable!"

    let memInsert heap scopeLoc key loc z = 
        Heap.storeZvalInLoc loc z (memInsertKv heap scopeLoc key loc)

    /// Resolves a reference; returns the resulting address (if found) or 'locNull
    let rec rGetRef heap ref = 
        match ref with 
        | ComplexRef (innerRef, key, _) ->
            rGetRef heap (BasicRef ((rGetRef heap innerRef), key))
        | BasicRef (loc, key) ->
            match loc with 
            | MemLoc l ->
                if (Heap.locInUse heap l) then
                    let locContent = Heap.readLoc heap l
                    match locContent.Value with 
                    | PhpValue.Array ht -> 
                        if (HashTable.hasKey ht key) then
                            HashTable.readValue ht key
                        else
                            SpecialLoc LocNull
                    // object
                    // string
                    | _ -> SpecialLoc LocNull
                else
                    SpecialLoc LocNull
            | SpecialLoc LocNull -> SpecialLoc LocNull

    let rec lGetRef heap ref rtype = 
        
        /// returns a new zval of the given type
        let freshZvalForType = 
            match rtype with
            | ScalarRef -> Zval.nullZval
            | ArrayRef -> Zval.emptyArrayZval
            | ObjectRef -> Zval.emptyArrayZval // todo

        let insertFreshZval heap loc key = 
            let newZval = freshZvalForType |> Zval.incRefcount
            let heap', newLoc = (Heap.freshLoc heap)
            memInsert heap' loc key newLoc newZval
            
        match ref with 
        | ComplexRef (innerRef, key, refType) ->
            let newHeap, newLoc = lGetRef heap innerRef refType
            lGetRef newHeap (BasicRef (newLoc, key)) refType
        | BasicRef (loc, key) ->
             match loc with 
             | MemLoc l ->
                    let locContent = Heap.readLoc heap l
                    match locContent.Value with 
                    | PhpValue.Array ht -> 
                        match key with 
                        | UndefKey -> // use default int key
                            let newKey = ht.nextIntKey
                            let newRef = BasicRef (loc, IntKey newKey)
                            lGetRef heap newRef rtype
                        | _ -> // int or string key
                            if (HashTable.hasKey ht key) then
                                (heap, rGetRef heap ref)
                            else
                                let heap' = insertFreshZval heap l key
                                (heap', rGetRef heap' ref)
                    // object (todo)
                    // string (todo)
                    | _ -> 
                        printfn "Cannot use scalar value as an array in on line"
                        (heap, SpecialLoc LocNull)
             | SpecialLoc LocNull -> (heap, SpecialLoc LocNull)

module PrettyPrint = 
    let prettyPrintKv' fKey fVal f kv = 
        f (fKey (fst kv)) (fVal (snd kv)) 

    let prettyPrintMapNotation1 k l = k + " |=> " + l
    let prettyPrintMapNotation2 k l = k + " |-> " + l

    let prettyPrintLoc l = string l

    let prettyPrintKey k = 
        match k with 
        | IntKey i -> string i
        | StringKey i -> "\"" + i + "\""
        | UndefKey -> "UNDEFINED"

    let prettyPrintKeyLocPair = 
        prettyPrintKv' prettyPrintKey prettyPrintLoc prettyPrintMapNotation1

    let prettyPrintHashTable ht = 
        ht.elements
        |> List.map prettyPrintKeyLocPair
        |> List.fold (fun s1 s2 -> s1 + " " + s2) "" 
        |> sprintf "Array(%s)"

    let prettyPrintPhpValue v = 
        match v with 
        | Int n -> string n
        | Float f -> string f
        | String s -> "\"" + string s + "\""
        | Bool b -> string b
        | Array a -> (prettyPrintHashTable a)
        | Null -> "NULL"
    
    let prettyPrintZval (z : Zval) =
        let formattedValue = prettyPrintPhpValue z.Value
        sprintf "[%s, %A, %A]" formattedValue z.Type z.RefCount

    let prettyPrintLocZvalPair = 
        prettyPrintKv' prettyPrintLoc prettyPrintZval prettyPrintMapNotation2

    let prettyPrintHeap h =
        h.Map
        |> Map.toList
        |> List.map  prettyPrintLocZvalPair
        |> List.fold (fun s1 s2 -> s1 + s2 + ", ") "" 
        |> sprintf "Heap(%s)"