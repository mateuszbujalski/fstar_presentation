module Helpers

module T = FStar.Tactics

[@@ noextract_to "FSharp"]
let tfail (#a: Type) (s:string) : T.Tac a =
    T.debug ("Tactic failure: " ^ s);
    T.fail s

// Compares if two fully qualified names are equal
[@@ noextract_to "FSharp"]
let rec fv_eq (fv1 : list string) (fv2 : list string) : bool = 
    match fv1, fv2 with
    | [], [] -> true
    | x :: xs, y :: ys -> if x = y then fv_eq xs ys else false
    | _, _ -> false
    
// NOTE: The build-in T.term_eq doesn't work very reliably. The plan is to build my own fully functional
//       equality on terms (with the definition of equal that makes sense for schema generation) so might not 
//       be very reusable
[@@ noextract_to "FSharp"]
let rec termeq (t1 : T.term) (t2 : T.term) : T.Tac bool =
    match T.inspect t1, T.inspect t2 with
    | T.Tv_Var bv1, T.Tv_Var bv2 -> begin
        // We only compare type of the argument, not it's name
        match T.inspect_bv bv1, T.inspect_bv bv2 with
        | { T.bv_sort = typ1; }, { T.bv_sort = typ2; } -> termeq typ1 typ2
        end
    | T.Tv_FVar fv1, T.Tv_FVar fv2 -> fv_eq (T.inspect_fv fv1) (T.inspect_fv fv2)
    // We ignore the fact if the arg is implicit or explicit
    | T.Tv_App lt1 (arg1, _), T.Tv_App lt2 (arg2, _) -> begin  
        // TODO: short circuit for && to avoid a tactic getting stuck - I belive this is already improved in master 
        let l_true = termeq lt1 lt2 in 
        if l_true then termeq arg1 arg2 else false
        end
    // false cases - combine them as _, _ -> false when I get confident enough with how this function works
    | T.Tv_App _ _, T.Tv_FVar _ -> false
    | T.Tv_FVar _, T.Tv_Arrow _ _ -> false
    | _, _ -> tfail ("termeq doesn't support " ^ (FStar.Tactics.Print.term_to_ast_string t1) ^ " or " ^ (FStar.Tactics.Print.term_to_ast_string t2) ^ "\n")

// Adds "Mk" to last segment of the qualified name
[@@ noextract_to "FSharp"]
let rec record_constructor_name (n : list string{Cons? n}) : Tot (list string) = 
    match n with
    | [ x ] -> [ "Mk" ^ x ]
    | x :: xs -> x :: record_constructor_name xs

// Records are syntactic sugar over inductive types
[@@ noextract_to "FSharp"]
let isRecord (env : T.env) (fv : T.fv) : T.Tac bool = 
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Let _ _ _ _ _ -> false
        | T.Sg_Inductive _ _ _ _ cts -> begin
            // A record should have exactly one constructor with at least one field
            // and the name of that constructor should be "Mk{record_type}"
            match cts with
            | [ (ctname, ct) ] -> 
                begin match T.inspect ct with 
                | T.Tv_Arrow _ _ -> 
                    if Cons? qname 
                        then (T.implode_qn ctname) = (T.implode_qn (record_constructor_name qname))
                        else false 
                | _ -> false
                end
            | _ -> false
            end
        | T.Sg_Val _ _ _ -> false
        | T.Unk -> false
        end
    | None -> false