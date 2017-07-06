(* Load KphpLib and open required modules *)

#load @"../KphpLib.fs"
open PhpAbsint.KphpLib
open PhpAbsint.KphpLib.PrettyPrint

(* This is how to make an empty array *)

let emptyArray = HashTable.emptyHashTable

(* And here is how to populate one *)

let ht1 = 
    emptyArray
    |> HashTable.insertKeyValue (StringKey "foo") 2
    |> HashTable.insertKeyValue (StringKey "bar") 1
    |> HashTable.insertKeyValue (IntKey 87) 1

(* Some zvalues ... *)

let z1 = 
    { Value = Int 5
      Type = TInt
      RefCount = 0}
(*ghj*)
let z3 = 
    { Value = String "ghjkghj"
      Type = TString
      RefCount = 9}

let z2 = 
    { Value = Array ht1
      Type = TArray
      RefCount = 0}

(* And finally a heap! *)

let h1 = 
    Heap.emptyHeap 
    |> Heap.storeZvalInLoc 0 z1
    |> Heap.storeZvalInLoc 2 z3
    |> Heap.storeZvalInLoc 5 z2

(* Finally, show the heap! Not as cute as K, but we'll get there! *)

printfn "%A" <| prettyPrintHeap h1

