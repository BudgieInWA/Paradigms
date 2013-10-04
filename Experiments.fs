module Paradigms.Experiments

let maxRuleDepth = 8

type exp = A | B | Mix of exp * exp | Var of string

/// The value (e, ee) represents that "e suffices instead of ee" or "e suff ee" for short - see the rule type.
type sufficency = exp * exp  

/// The value (e, ee), [(e1,ee1) ... (eN,eeN)] represents the rule that: "E suff EE provided that for all i in 1..N, Ei suff EEi".  
/// Here the E, EE, Ei, EEi are the same as e, ee, eI, eeI except with each variable (Var Vj) replaced by an experiment EEEj that 
/// contains no vars (the same EEEj each time Vj appears in the rule).  The rule holds for every such substitution for the vars. 
type rule = Rule of sufficency * (sufficency list)  

type ruleGen = unit -> rule      // A rule generator creates new variables for a rule each time it is called, to avoid clashes
                                 // with variable names in other rules, or when the rule is used multiple times.
type labRules = ruleGen list     // Each lab has a list of rules, represented as generators.

type private variableMapping = Map<string, exp>
//type private substitution = variableMapping * set-of-sets

/// The number of Bases and Mixes in an experiment
let rec expSize = function A|B -> 1
                         | Mix (x, y) -> 1+expSize x + expSize y
                         | Var _ -> raise (System.Exception "expSize for a Var")       // This shouldn't happen

let pr d s x = x //printf "%s%s %A\n" (String.replicate d "\t") s x; x

let rec containsVar x = function
    | Var y when x = y -> true
    | Mix (e, ee) -> containsVar x e || containsVar x ee
    | _ -> false

/// Find the experiment that you get when replaceing all vars with their mappings
let rec substitute (mapping:variableMapping) : (exp -> exp) = function
    | Var x ->
        match mapping.TryFind x with
        | None -> Var x
        | Some e -> substitute mapping e
    | Mix(e, ee) -> Mix(substitute mapping e, substitute mapping ee)
    | e -> e

/// Add x->exp to the mapping, checking for consistency 
let rec actuallyAddMapping x exp : (variableMapping option -> variableMapping option) = function
    | None -> None
    | Some mapping ->
        match exp with
        | Var y when y = x -> Some mapping // Mapping var x to var x, don't actually add it
        | e when containsVar x e -> None   // Mapping var x to something bigger to x that contains x (inconsistent)
        | e -> mapping.Add(x, e) |> Some |> replaceFromRHS x // after replacing x from RHS, invariant is maintained

/// Replace all occurences of Var x in RHS with whatever x maps to
and replaceFromRHS x : (variableMapping option -> variableMapping option) = function
    | None -> None
    | Some mapping ->
        match Map.tryFindKey (fun k v -> containsVar x v) mapping with
        | None -> Some mapping
        | Some k -> Some mapping |> actuallyAddMapping k (substitute mapping (Map.find k mapping)) |> replaceFromRHS x
    
/// Invariant: Any variable on the RHS of a mapping does not appear on the LHS
let rec addMapping (x:string) exp : (variableMapping option -> variableMapping option) = function
    | None -> None
    | Some mapping ->
        let e = substitute mapping exp // e does not contain any var that in on the LHS
        match mapping.TryFind x with
        | None -> Some mapping |> actuallyAddMapping x e
        | Some ee -> Some mapping |> addEquivalence e ee
         
/// Returns a mapping in which e is equivalent to ee (which may be none, and may have new mappings)
and addEquivalence e ee : (variableMapping option -> variableMapping option) = function
    | None -> None
    | Some mapping ->
        match e, ee with
        | A,A | B,B -> Some mapping // Equivalent as is.
        | Mix(a, b), Mix(c, d) -> Some mapping |> addEquivalence a c |> addEquivalence b d
        | Var y, _ -> Some mapping |> addMapping y ee // y needs to map to ee to make them equiv
        | _, Var z -> Some mapping |> addMapping z e  // z needs to map to e  to make them equiv
        | _ -> None

/// Determine the variable mapping that unifies two experements, using a partial variable mapping as a starting point.
/// new variables may be mapped, but existing mappings wont be touched.
/// exp2 cannot have variables.
let rec unify (exp1, exp2) : (variableMapping option -> variableMapping option) = function
    | None -> None
    | Some mapping -> 
        match exp1, exp2 with
        | A,A | B,B -> Some mapping
        | Mix(x, y), Mix(xx, yy) -> Some mapping |> unify (x, xx) |> unify (y, yy)
        | Var x, e | e, Var x  ->
            match mapping.TryFind x with
            | Some existingMapping -> Some mapping |> unify (e, existingMapping) // Only works when exp2 has no vars
            | None -> Some mapping |> addMapping x e
        | _ -> None

/// Does the mapping let us satisfy all of the expressions
let rec suffSubgoals depth rules mapping : ((exp * exp) list -> variableMapping seq) = function
    | [] -> seq { yield mapping }
    | (e, ee) :: tail ->
        seq { for m in suffOr depth rules (substitute mapping e, substitute mapping ee) mapping
            do yield! suffSubgoals depth rules m tail }

/// The sequence of mappings m such that suff(exp1, exp2) under m  
and suffOr depth (rules: ruleGen list) (exp1, exp2) mapping : variableMapping seq =
    if depth > maxRuleDepth then printf "WARNING: max depth reached!\n"; Seq.empty
    else seq { for Rule ((e1, e2), subgoals) in List.map (fun rg -> rg()) rules
        do yield! match Some mapping |> unify (e1, exp1) |> unify (e2, exp2) with
                  | None -> Seq.empty
                  | Some m -> suffSubgoals (depth+1) rules m subgoals }

/// Does exp1 suffice for exp2 using the rulelist
let suffices (rules: ruleGen list) (exp1, exp2) =
    Map.empty |> suffOr 1 rules (exp1, exp2) |> Seq.isEmpty |> not

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Hints:  First, see the hints on the project handout. Then the following are hints to help get you started. 
//  1. Decompose the problem of finding possible combined experiments into simpler steps.
//  2. The basic step will be checking "e suffices instead of E" for two experiments, using the set of 
//     rules for a particular lab.  Use a function "suffices" taking the rules and two experiments.
//  3. To use a rule to show a particular sufficiency like "E suffices instead of EE", all you need to do is choose 
//     an appropriate experiment EEEj for each variable Vj in the rule in order to turn e into E and ee into EE, and then 
//     attempt to show sufficiency for each pair (E1, EE1) ... (En, EEn) - these are called the subgoals.  Note that if n=0 there is
//     nothing more to do, and every lab has at least one rule like this.  (The rules will always be designed so that
//     it is impossible to keep following subgoals forever, although not if you have bugs, so you may want to limit this.) 
//
//     Note that variables can appear many times, including many times in the same experiment.
//     E.g., the following represents a rule that roughly means 
//     "doing an experiment x always suffices instead of doing x twice and mixing the results"
//          (Var "x", Mix (Var "x", Var "x")), []
// 4. Each time a rule is used, a single experiment is chosen to replace each variable.  But,
//    if the rule is used again, different experiments can be chosen.
//
// 5. To match experiments to rules, you will probably need a function "unify" that takes two exp's and returns a 
//    substitution that makes the exp's exactly the same when it is used to replace the vars in one of the exp's 
//    (if such a substitution exists).   For example:
//
//           unify (Mix (A, B)) (Mix (Var "x", A)) yields no substitution the two exp's can't be made the same.
//
//           unify (Mix (A, B)) (Mix (Var "x", Var "y")) yields a substitution that maps: "x" to A  and "y" to B
//
//           unify (Mix (A, A)) (Mix (Var "x", Var "x")) yields a substitution that maps: "x" to A 
//
//            unify (Mix (A, B)) (Mix (Var "x", Var "x")) yields no substitution, because no matter
//               what we replace Var "x" with, the two exp's won't end up the same 
//               ("x" can't be equal to both A and B).
//
//  6. Write unify by considering the possible cases for the two exps, and using recursion appropriately.
//  7. You will need a type for substitutions - mappings from variable names to exp's they should be replaced with.
//  8. Be careful not the create substitutions that map a variable to an exp that contains the same variable. (!)
//  9. You can modify the exact form of the unify function if you want.
// 10. Use this function write one that tries unifying one exp with each part of another exp.
// 11. Eventually you need a function that takes a number clients exps, generates all possible combined exps,
//     then chooses between them using an efficiency function like the one above.
// 12. A smaller experiment with the same number of clients should be preferred over a larger one.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////