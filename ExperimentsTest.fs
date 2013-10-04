module Paradigms.ExperimentsTest

open Experiments


/// Create a new var with a generated, unique name
let newVar =   let n = ref 0 in   fun v -> n:=!n+1;
                                           Var (v + string !n)
let newVar2 v1 v2 = newVar v1, newVar v2
let newVar3 (v1,v2,v3) = newVar v1, newVar v2, newVar v3
let newVar4 (v1,v2,v3,v4) = newVar v1, newVar v2, newVar v3, newVar v4


let correctStr = function false -> "FAIL :( "
                        | true ->  "success "
let prTest test expected res = printf "%s %s returned %b\n" (correctStr (res=expected)) test res
let prNl = printf "\n"


// A set of rules for testing
let rule1 () = let x = newVar "x"
               Rule ((x, x), [])

let rule2 () = let x, xx, y = newVar3 ("2x", "2xx", "2y")
               Rule ((Mix(x,xx), y),  [(x,y)])

let rule3 () = let x, xx, y = newVar3 ("3x", "3xx", "3y")
               Rule ((Mix(x,xx), y),  [(xx,y)])

let rule4 () = Rule ((Mix(A, B), Mix(B, A)), [])
let rule5 () = Rule ((Mix(B, A), Mix(A, B)), [])
let rule6 () = let x, xx, y, yy = newVar4 ("6x", "6xx", "6y", "6yy")
               Rule ((Mix(x,xx), Mix(y,yy)),  [(x,y); (xx,yy)])

let rule7 () = let x, xx, y, yy = newVar4 ("7x", "7xx", "7y", "7yy")
               Rule ((Mix(x,xx), Mix(y,yy)),  [(x,yy); (xx,y)])

let rule8 () = let x = newVar "8x"
               Rule ((x, Mix(x,x)),  [])

let rule9 () = let x, xx, y, z = newVar4 ("9x", "9xx", "9y", "9z")
               Rule ( (Mix (x,xx),y),  [(x,z); (z,y)] )

let rule10() = let x, xx, y, z = newVar4 ("0x", "0xx", "0y", "0z")
               Rule ( (Mix (x,xx),y),  [(xx,z); (z,y)] )

let rule11() = Rule ( (A,B),  [] )
let rule12() = Rule ( (B,A),  [] )

let rulesA = [rule1; rule2; rule3; rule6]
let rulesB = [rule1; rule2; rule3; rule4; rule5; rule6; rule7; rule8]
let rulesC = [rule4; rule5; rule9; rule10; rule11; rule12]         // Rules 9,10 are slightly tricky. 
                                                                   // Focus on rules like the others first.


let runTests () =

    suffices [rule1] (A, A) |> prTest "suffices [rule1] (A, A)" true
    suffices [rule1] (A, B) |> prTest "suffices [rule1] (A, B)" false
    

    suffices rulesA (A, B) |> prTest "suffices rulesA (A, B)" false
    suffices rulesA (Mix (A, B), A) |> prTest "suffices rulesA (Mix (A, B),A)" true
    suffices rulesA (Mix (Mix (A, B), B),A) |> prTest "suffices rulesA (Mix (Mix (A, B), B),A)" true
    suffices rulesA (Mix (Mix (B, B), B),A) |> prTest "suffices rulesA (Mix (Mix (B, B), B),A)" false
    suffices rulesA (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesA (Mix (Mix (B, B), B), Mix (B, B))" true
    suffices rulesA (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesA (Mix (Mix (A, B), B), Mix (B, A))" false
    prNl


    suffices rulesB (A, B) |> prTest "suffices rulesB (A, B)" false
    suffices rulesB (Mix (A, B), A) |> prTest "suffices rulesB (Mix (A, B),A)" true
    suffices rulesB (Mix (Mix (A, B), B),A) |> prTest "suffices rulesB (Mix (Mix (A, B), B),A)" true
    suffices rulesB (Mix (Mix (B, B), B),A) |> prTest "suffices rulesB (Mix (Mix (B, B), B),A)" false
    suffices rulesB (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesB (Mix (Mix (B, B), B), Mix (B, B))" true
    suffices rulesB (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesB (Mix (Mix (A, B), B), Mix (B, A))" true
    prNl

    suffices rulesC (A, B) |> prTest "suffices rulesC (A, B)" true
    suffices rulesC (Mix (A, B), A) |> prTest "suffices rulesC (Mix (A, B),A)" true
    suffices rulesC (Mix (Mix (A, B), B),A) |> prTest "suffices rulesC (Mix (Mix (A, B), B),A)" true
    suffices rulesC (Mix (Mix (B, B), B),A) |> prTest "suffices rulesC (Mix (Mix (B, B), B),A)" true
    suffices rulesC (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesC (Mix (Mix (B, B), B), Mix (B, B))" false
    suffices rulesC (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesC (Mix (Mix (A, B), B), Mix (B, A))" false
    