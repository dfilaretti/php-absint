namespace PhpAbsint.Interpreter
open PhpAbsint.KphpLib
open Devsense.PHP.Syntax

type PgmFragment = 
    | StmtList of Ast.Statement list
    | Stmt of Ast.Statement
    | Expr of Ast.Expression
    | Atom of Atom
    | InternalCmd of InternalCmd 
and InternalCmd = 
    | Assign of PgmFragment * PgmFragment 
    | ItemUse of PgmFragment * PgmFragment
and Atom = 
    | PhpValue of PhpValue
    | Reference of Ref
    | Location of Loc
    | Key of Key
    | Void

type Kont = 
    | ExprStmtK of Kont
    | SeqK of PgmFragment * Kont 
    // ValueAssign (i.e. assign by value)
    | ValueAssignExK1 of PgmFragment * Kont
    | ValueAssignExK2 of Atom * Kont
    // ItemUse (i.e. array access)
    | KItemUseEvalL of PgmFragment * Kont
    | KItemUseEvalR of Atom * Kont
    | HaltK

type State = 
    { pgmFragment : PgmFragment
      crntScope : MemLoc
      heap : Heap
      kont : Kont }

// TODO: use F# 4.1 builtins! 
type Result<'TSuccess, 'TFailure> = 
    | Success of 'TSuccess
    | Failure of 'TFailure * string

[<RequireQualifiedAccess>]
module Parsing = 
    open System.Text
    let readFile filename = 
        System.IO.File.ReadAllText filename
    
    /// Parses a php file. Returns a list of statements.
    let parseFile (filename: string) = 
        let fileContent = readFile filename
        let codeSourceUnit = CodeSourceUnit (fileContent, filename, Encoding.UTF8, Lexer.LexicalStates.INITIAL, LanguageFeatures.Php71Set)
        let nodesFactory = Ast.BasicNodesFactory (codeSourceUnit)
        let errors = null
        codeSourceUnit.Parse (nodesFactory, null, null)
        let ast = codeSourceUnit.Ast
        Array.toList ast.Statements

[<RequireQualifiedAccess>]
module Execution = 

    let isKResult p = 
        match p with 
        | Atom _ -> true
        | _ -> false

    let isConvertibleToLoc t = 
        match t with 
        | Atom.Reference _ -> true
        // todo: LiteralValue | ThisTag 
        | _ -> false

    type ConversionToLocMode = Lhs | Rhs

    let convertToLoc mode heap atom =
        match atom with 
        | Reference r -> 
            match mode with 
            | Lhs -> Memory.lGet heap r
            | Rhs -> heap, Memory.rGet heap r
        | _ -> failwith "ERROR: convertToLoc called with wrong params." 

    /// Transforms a PhpValue into a Key.    
    let value2Key v=
        // TODO: convert "numeric strings" into numbers, etc. (see KPHP)
        match v with 
        | Int n -> IntKey n
        | String s -> StringKey s
        | Float f -> UndefKey
        | _ -> failwith "TODO"

    let h0, initialScope = Heap.freshLoc Heap.emptyHeap

    let initialHeap =
        Heap.storeZvalInLoc initialScope Zval.emptyArrayZval h0

    let applyKont state =
        match state.pgmFragment with 
        | Atom value -> 
            match state.kont with 
            | ExprStmtK k ->
                { state with 
                    pgmFragment = Atom Void 
                    kont = k }
                |> Success
            
            | SeqK (restOfPgm, k) ->
                match value with 
                | Void -> 
                    { state with pgmFragment = restOfPgm; kont = k } 
                    |> Success
                | _ -> Failure (state, "Cannot proceed with SeqK, since program fragment is not empty")
            
            | ValueAssignExK1 (r, k) -> 
                let vLhs = Atom value
                let cmd = InternalCmd (Assign (vLhs, r))
                { state with 
                    pgmFragment = cmd 
                    kont = k }
                |> Success

            | ValueAssignExK2 (l, k) -> 
                let vRhs = Atom value
                let cmd = InternalCmd (Assign (Atom l, vRhs))
                { state with 
                    pgmFragment = cmd
                    kont = k }
                |> Success
            
            | KItemUseEvalL (r, k) ->
                let vLhs = Atom value 
                let cmd = InternalCmd (ItemUse (vLhs, r))
                { state with 
                    pgmFragment = cmd 
                    kont = k }
                |> Success
            
            | KItemUseEvalR (l, k) ->
                let vRhs = Atom value 
                let cmd = InternalCmd (ItemUse (Atom l, vRhs))
                { state with 
                    pgmFragment = cmd 
                    kont = k }
                |> Success
            
            | _ -> Failure (state, "Kontinuation not supported: " + state.kont.ToString())
        | _ -> Failure (state, "Attempt to apply a Kontinuation, but PgmFragment does not contain a Value")            

    

      

    let step state = 
        match state.pgmFragment with 
        | StmtList ss ->
            match ss with 
            | [] -> Success state
            | s::pgm -> 
                { state with 
                    pgmFragment = Stmt s 
                    kont = SeqK (StmtList pgm, state.kont) } 
                |> Success                         
        
        // | Stmt s, sl, h, k) -> 
        | Stmt s -> 
            match s with
            // --- Expression Stmt 
            | :? Ast.ExpressionStmt as s -> 
                let exp = s.Expression
                { state with 
                    pgmFragment = Expr exp
                    kont = ExprStmtK state.kont }
                |> Success
            // --- EmptyStmt
            | :? Ast.EmptyStmt ->
                { state with pgmFragment = Atom Void }
                |> Success
            | :? Ast.IfStmt as s -> 
                // TODO:
                { state with pgmFragment = Atom Void } |> Success
            // --- Unsupported 
            | _ -> Failure (state, "Unsupported statement: " + s.ToString())
        
        // | State (Expr expr, s, h, k) -> 
        | Expr expr -> 
            match expr with 
            // --- Variable Access
            | :? Ast.DirectVarUse as e -> 
                let varName = e.VarName.Value
                let ref = BasicRef (MemLoc state.crntScope, StringKey varName)
                { state with pgmFragment = Atom (Reference ref) }
                |> Success

            // --- Array Access                
            | :? Ast.ItemUse as e -> 
                let arrayExp = Expr e.Array
                let keyExp = 
                    if isNull e.Index then
                        Atom Void // no key!
                    else
                        Expr e.Index
                let cmd = InternalCmd (ItemUse (arrayExp, keyExp))
                { state with pgmFragment = cmd } |> Success
            
            // --- Assignment by value
            | :? Ast.ValueAssignEx as e ->
                let l = Expr e.LValue
                let r = Expr e.RValue
                let cmd = InternalCmd (Assign (l, r))
                { state with pgmFragment = cmd } |> Success
            
            // --- Literal
            | :? Ast.LongIntLiteral as l -> 
                let v = int l.Value
                { state with pgmFragment = Atom (PhpValue (Int v)) }
                |> Success

            | :? Ast.StringLiteral as l -> 
                let v = string l.Value
                { state with pgmFragment = Atom (PhpValue (String v)) }
                |> Success
            
            // --- Unsupported
            | _ -> Failure (state, "Unsupported expression: " + expr.ToString())
        | Atom a ->
            applyKont state
        
        // Internal commands
        | InternalCmd cmd ->
            match cmd with 
            
            //--- ASSIGNMENT BY VALUE
            
            // assign-strict-LHS
            | Assign (l,r) when not (isKResult l) ->
                { state with 
                    pgmFragment = l
                    kont = ValueAssignExK1 (r, state.kont) }
                |> Success
            
            // assign-strict-RHS
            | Assign(Atom l,r) when not (isKResult r) -> 
                { state with 
                    pgmFragment = r 
                    kont = ValueAssignExK2 (l, state.kont) }
                |> Success

            // assign-LHS2Loc
            | Assign (Atom (Reference rl), r) when isKResult r -> 
                let (h', loc) = convertToLoc Lhs state.heap (Reference rl)
                let l' = Atom (Location loc)
                let cmd' = InternalCmd (Assign (l', r))
                { state with 
                    pgmFragment = cmd' 
                    heap = h' }
                |> Success

            // assign-RHS2Loc-NonLiteral
            | Assign(l, Atom r) when isKResult l && isConvertibleToLoc r ->
                let h', loc = convertToLoc Rhs state.heap r 
                let rhs = Atom (Location loc)
                let cmd' = InternalCmd (Assign (l, rhs))
                { state with 
                    heap = h' 
                    pgmFragment = cmd' } 
                |> Success
           
            // assign-RHS2LangValue-overflow (TODO)

            // assign-RHS2LangValue-no-overflow
            | Assign(Atom (Location l), Atom (Location (MemLoc n))) -> 
                // TODO: use convertToLanguageValue instead of Memory.memRead
                let v = Atom (PhpValue (Memory.memRead state.heap n))
                let cmd' = InternalCmd (Assign(Atom (Location l), v))
                { state with 
                    pgmFragment = cmd' } 
                |> Success
                
            // assign-RHS2LangValue-locNull
            | Assign(Atom (Location l), Atom (Location (SpecialLoc LocNull))) -> 
                let v = Atom (PhpValue Null)
                let cmd' = InternalCmd (Assign(Atom (Location l), v))
                { state with 
                    pgmFragment = cmd' } 
                |> Success
            
            // assign
            | Assign(Atom (Location l), Atom (PhpValue v)) ->
                match l with 
                | MemLoc n ->
                    // TODO: use CopyValueToLoc
                    let h' = Memory.memUpdate state.heap n v
                    let retVal = Atom (PhpValue v)
                    { state with 
                        pgmFragment = retVal
                        heap = h' }
                    |> Success
                | _ -> Failure (state, "Attempt to write into LocNull")
            
            // assign-error (TODO)

            //--- ARRAY ACCESS 

            // array-access-strict-LHS
            | ItemUse (l, r) when not (isKResult l) -> 
                { state with 
                    pgmFragment = l
                    kont = KItemUseEvalL (r, state.kont) }
                |> Success

            // array-access-strict-RHS
            | ItemUse (Atom l, r) when not (isKResult r) -> 
                { state with 
                    pgmFragment = r 
                    kont = KItemUseEvalR (l, state.kont) }
                |> Success           

            // NB next 3 rules corresponds to the single array-access-LHS2LangValue
            //      in KPHP. This is thanks to ConvertibleToLanguageValue etc.
            // TODO: decide whether to reimplement ConvertibleToLanguageValue etc. here or not
            
            // array-access-LHS2LangValue [ simplified ]
            //| ItemUse (l, Atom (Reference r)) ->
            //    let rhsLoc =  Atom (Location (Memory.rGetRef state.heap r))
            //    { state with 
            //        pgmFragment = InternalCmd (ItemUse (l, rhsLoc)) }
            //    |> Success
            | ItemUse (l, Atom r) when isConvertibleToLoc r ->
                let h', loc = convertToLoc Rhs state.heap r
                let rhsLoc =  Atom (Location loc)
                { state with 
                    heap = h'
                    pgmFragment = InternalCmd (ItemUse (l, rhsLoc)) }
                |> Success

            | ItemUse (l, Atom (Location (MemLoc n))) ->
                let v = Atom (PhpValue (Memory.memRead state.heap n))
                { state with 
                    pgmFragment = InternalCmd (ItemUse (l, v)) }
                |> Success                            
            // locNull case (?)

            // array-access-key-cast-float
            // array-access-key-cast-bool
            // array-access-key-cast-string
            | ItemUse (l, Atom (PhpValue v)) ->
                let key = Atom (Key (value2Key v))
                { state with 
                    pgmFragment = InternalCmd (ItemUse (l, key)) }
                |> Success
            
            // array-access-no-key
            | ItemUse (l, Atom Void) ->
                let key = Atom (Key UndefKey)
                { state with 
                    pgmFragment = InternalCmd (ItemUse (l, key)) }
                |> Success

            // array-access-simple [ I think unused ATM -- has to do with literals? ]
            | ItemUse (Atom (Location l), Atom (Key k)) -> 
                let ref = Atom (Reference (BasicRef (l, k)))
                { state with pgmFragment = ref }
                |> Success                                                    
            
            // array-access-nested
            | ItemUse (Atom (Reference r), Atom (Key k)) -> 
                let ref = Atom (Reference (ComplexRef (r, k, RefType.ArrayRef)))
                { state with pgmFragment = ref }
                |> Success                                        
                
            // Unsupported Internal Command
            | _ -> Failure (state, "Unsupported InternalCmd (semantics-level commands)")

    let inject pgm = 
        { pgmFragment = pgm 
          crntScope = initialScope
          heap = initialHeap
          kont = HaltK }

    let rec step' state = 
        match state with
        | Success s -> 
            match s.pgmFragment with 
            | StmtList [] -> state
            | _ -> step' (step s)  
        | Failure (s, errorMsg) -> Failure (s, "Interpreter could't finish execution: " + errorMsg)

    let run pgm = 
        let s0 = Success (inject pgm)
        step' s0

    let runFile f = 
        run <| StmtList (Parsing.parseFile f)

module PrettyPrint = 
    let prettyState s  = 
        match s with 
        | Success state ->
            PrettyPrint.prettyPrintHeap state.heap
        | Failure (state, msg) -> 
            PrettyPrint.prettyPrintHeap state.heap