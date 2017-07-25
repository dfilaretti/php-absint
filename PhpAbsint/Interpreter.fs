namespace PhpAbsint.Interpreter
open PhpAbsint.KphpLib
open Devsense.PHP.Syntax

type ConversionToLocMode = Lhs | Rhs

type PgmFragment = 
    | StmtList of Ast.Statement list
    | Stmt of Ast.Statement
    | Expr of Ast.Expression
    | KResult of KResult
    | InternalCmd of InternalCmd 
and InternalCmd = 
    | Assign of PgmFragment * PgmFragment 
    | ItemUse of PgmFragment * PgmFragment
    | IfStmt of List<Ast.ConditionalStmt>
and KResult = 
    | PhpValue of PhpValue
    | ConvertibleToLanguageValue of ConvertibleToLanguageValue
    | Key of Key // todo: make DU "subcase" of PhpValue?
    | Void // todo: use Option<KResult> instead?
and ConvertibleToLanguageValue = 
    | Loc of Loc
    | ConvertibleToLoc of ConvertibleToLoc 
and ConvertibleToLoc =
    | Ref of Ref
    //| LiteralValue of LiteralValue
    | ThisTag

type Kont = 
    | ExprStmtK of Kont
    | SeqK of PgmFragment * Kont 
    // ValueAssign (i.e. assign by value)
    | ValueAssignExK1 of PgmFragment * Kont
    | ValueAssignExK2 of KResult * Kont
    // ItemUse (i.e. array access)
    | KItemUseEvalL of PgmFragment * Kont
    | KItemUseEvalR of KResult * Kont
    | HaltK

type State = 
    { pgmFragment : PgmFragment
      crntScope : int
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

    /// Whether the input pgm fragment is a KResult.
    /// TODO: use DU + pattern matching instead
    let isKResult p =
        match p with 
        | KResult _ -> true
        | _ -> false
    
    // todo: move to KphpLib?
    let convertToLoc mode heap t =
        match t with 
        | Ref r -> 
            match mode with 
            | Lhs -> Memory.lGet heap r
            | Rhs -> heap, Memory.rGet heap r
        //| ThisTag -> SpecialLoc LocNull
    
    // todo: move to KphpLib?
    let convertToLanguageValue h t = 
        match t with 
        | Loc l -> h, Memory.memRead h l
        | ConvertibleToLoc c -> 
            let h', l = convertToLoc Rhs h c
            h', Memory.memRead h' l

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
        | KResult value -> 
            match state.kont with 
            | ExprStmtK k ->
                { state with 
                    pgmFragment = KResult Void 
                    kont = k }
                |> Success
            
            | SeqK (restOfPgm, k) ->
                match value with 
                | Void -> 
                    { state with pgmFragment = restOfPgm; kont = k } 
                    |> Success
                | _ -> Failure (state, "Cannot proceed with SeqK, since program fragment is not empty")
            
            | ValueAssignExK1 (r, k) -> 
                let vLhs = KResult value
                let cmd = InternalCmd (Assign (vLhs, r))
                { state with 
                    pgmFragment = cmd 
                    kont = k }
                |> Success

            | ValueAssignExK2 (l, k) -> 
                let vRhs = KResult value
                let cmd = InternalCmd (Assign (KResult l, vRhs))
                { state with 
                    pgmFragment = cmd
                    kont = k }
                |> Success
            
            | KItemUseEvalL (r, k) ->
                let vLhs = KResult value 
                let cmd = InternalCmd (ItemUse (vLhs, r))
                { state with 
                    pgmFragment = cmd 
                    kont = k }
                |> Success
            
            | KItemUseEvalR (l, k) ->
                let vRhs = KResult value 
                let cmd = InternalCmd (ItemUse (KResult l, vRhs))
                { state with 
                    pgmFragment = cmd 
                    kont = k }
                |> Success
            
            | _ -> Failure (state, "Kontinuation not supported: " + state.kont.ToString())
        | _ -> Failure (state, "Attempt to apply a Kontinuation, but PgmFragment does not contain a Value")            

    let constNameAsString (c: Ast.GlobalConstUse) =
        c.Name.Name.Value 

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
                { state with pgmFragment = KResult Void }
                |> Success
            // --- IfStmt
            | :? Ast.IfStmt as s -> 
                let conditions = Seq.toList s.Conditions
                let cmd = InternalCmd (IfStmt conditions)
                { state with pgmFragment = cmd } |> Success
            // --- Unsupported 
            | _ -> Failure (state, "Unsupported statement: " + s.ToString())
        
        | Expr expr -> 
            match expr with
            // --- GlobalconstUse
            | :? Ast.GlobalConstUse as e when constNameAsString e = "true" -> 
                // TODO: this seems to be used for 'true' and 'false' in addition to standard constants
                { state with pgmFragment = KResult (PhpValue (Bool true)) }
                |> Success
            | :? Ast.GlobalConstUse as e when constNameAsString e = "false" -> 
                // TODO: this seems to be used for 'true' and 'false' in addition to standard constants
                { state with pgmFragment = KResult (PhpValue (Bool false)) }
                |> Success
            // TODO: "normal constants"

            // --- Variable Access
            | :? Ast.DirectVarUse as e -> 
                let varName = e.VarName.Value
                let ref = BasicRef (Loc.MemLoc state.crntScope, StringKey varName)
                { state with pgmFragment = KResult (ConvertibleToLanguageValue (ConvertibleToLoc (Ref ref))) }
                |> Success

            // --- Array Access                
            | :? Ast.ItemUse as e -> 
                let arrayExp = Expr e.Array
                let keyExp = 
                    if isNull e.Index then
                        KResult Void // no key!
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
                { state with pgmFragment = KResult (PhpValue (Int v)) }
                |> Success

            | :? Ast.StringLiteral as l -> 
                let v = string l.Value
                { state with pgmFragment = KResult (PhpValue (String v)) }
                |> Success
            
            // --- Unsupported
            | _ -> Failure (state, "Unsupported expression: " + expr.ToString())
        | KResult a ->
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
            | Assign(KResult l,r) when not (isKResult r) -> 
                { state with 
                    pgmFragment = r 
                    kont = ValueAssignExK2 (l, state.kont) }
                |> Success

            // assign-LHS2Loc
            | Assign (KResult (ConvertibleToLanguageValue (ConvertibleToLoc c)), r) when isKResult r -> 
                let (h', loc) = convertToLoc Lhs state.heap c
                let l' = KResult (ConvertibleToLanguageValue (Loc loc))
                let cmd' = InternalCmd (Assign (l', r))
                { state with 
                    pgmFragment = cmd' 
                    heap = h' }
                |> Success

            // assign-RHS2Loc-NonLiteral (todo: side-condition)
            | Assign(l, KResult (ConvertibleToLanguageValue (ConvertibleToLoc r))) when isKResult l ->
                let h', loc = convertToLoc Rhs state.heap r 
                let rhs = KResult (ConvertibleToLanguageValue (Loc loc))
                let cmd' = InternalCmd (Assign (l, rhs))
                { state with 
                    heap = h' 
                    pgmFragment = cmd' } 
                |> Success
           
            // assign-RHS2LangValue-overflow (TODO)

            // assign-RHS2LangValue-no-overflow
            | Assign(KResult (ConvertibleToLanguageValue (Loc l)), KResult (ConvertibleToLanguageValue(Loc (MemLoc n)))) -> 
                // NB: KPHP uses convertToLanguageValue here. However, it's inconvenient since 
                //     we already know l:Loc, hence we can avoid having to update the heap etc...
                let v = KResult (PhpValue (Memory.memRead state.heap (MemLoc n)))
                let cmd' = InternalCmd (Assign(KResult (ConvertibleToLanguageValue (Loc l)), v))
                { state with 
                    pgmFragment = cmd' } 
                |> Success
                
            // assign-RHS2LangValue-locNull
            | Assign(KResult(ConvertibleToLanguageValue (Loc l)), KResult (ConvertibleToLanguageValue(Loc LocNull))) -> 
                let v = KResult (PhpValue Null)
                let cmd' = InternalCmd (Assign(KResult (ConvertibleToLanguageValue (Loc l)), v))
                { state with 
                    pgmFragment = cmd' } 
                |> Success
            
            // assign
            | Assign(KResult (ConvertibleToLanguageValue (Loc l)), KResult (PhpValue v)) ->
                match l with 
                | Loc.MemLoc n ->
                    // TODO: use CopyValueToLoc
                    let h' = Memory.memUpdate state.heap n v
                    let retVal = KResult (PhpValue v)
                    { state with 
                        pgmFragment = retVal
                        heap = h' }
                    |> Success
                | _ -> Failure (state, "Attempt to write into LocNull")
            
            // assign-error (TODO)

            //--- IF STATEMENT 
            // NB: this is shaped a bit differently than in KPHP mostly 'cause 
            //     the AST is shaped differently...

            | IfStmt [] -> 
                { state with pgmFragment = KResult Void } |> Success

            //| IfStmt (h::T) -> 
                
            //    let condition = h. Condition
            //    { state with pgmFragment = InternalCmd (IfStmt T) } |> Success

            //--- ARRAY ACCESS 

            // array-access-strict-LHS
            | ItemUse (l, r) when not (isKResult l) -> 
                { state with 
                    pgmFragment = l
                    kont = KItemUseEvalL (r, state.kont) }
                |> Success

            // array-access-strict-RHS
            | ItemUse (KResult l, r) when not (isKResult r) -> 
                { state with 
                    pgmFragment = r 
                    kont = KItemUseEvalR (l, state.kont) }
                |> Success           

            // array-access-RHS2LangValue
            | ItemUse (l, KResult (ConvertibleToLanguageValue c)) ->
                let h', v =  convertToLanguageValue state.heap c
                let rhsVal =  KResult (PhpValue v)
                { state with 
                    heap = h'
                    pgmFragment = InternalCmd (ItemUse (l, rhsVal)) }
                |> Success

            // array-access-key-cast-float
            // array-access-key-cast-bool
            // array-access-key-cast-string
            | ItemUse (l, KResult (PhpValue v)) ->
                let key = KResult (Key (value2Key v))
                { state with 
                    pgmFragment = InternalCmd (ItemUse (l, key)) }
                |> Success
            
            // array-access-no-key
            | ItemUse (l, KResult Void) ->
                let key = KResult (Key UndefKey)
                { state with 
                    pgmFragment = InternalCmd (ItemUse (l, key)) }
                |> Success

            // array-access-simple [ I think unused ATM -- has to do with literals? ]
            | ItemUse (KResult (ConvertibleToLanguageValue (Loc l)), KResult (Key k)) -> 
                let ref = KResult (ConvertibleToLanguageValue (ConvertibleToLoc (Ref (BasicRef (l, k)))))
                { state with pgmFragment = ref }
                |> Success                                                    
            
            // array-access-nested
            | ItemUse (KResult (ConvertibleToLanguageValue (ConvertibleToLoc (Ref r))), KResult (Key k)) -> 
                let ref = KResult (ConvertibleToLanguageValue (ConvertibleToLoc (Ref (ComplexRef (r, k, RefType.ArrayRef)))))
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

    let runFile f = Parsing.parseFile f |> StmtList |> run

module PrettyPrint = 
    let prettyState s  = 
        match s with 
        | Success state ->
            PrettyPrint.prettyPrintHeap state.heap
        | Failure (state, msg) -> 
            PrettyPrint.prettyPrintHeap state.heap