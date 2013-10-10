module Paradigms.ExperimentsTest

open Experiments


/// Create a new var with a generated, unique name
let newVar =   let n = ref 0 in   fun v -> n:=!n+1;
                                           Var (v + string !n)
let newVar2 v1 v2 = newVar v1, newVar v2
let newVar3 (v1,v2,v3) = newVar v1, newVar v2, newVar v3
let newVar4 (v1,v2,v3,v4) = newVar v1, newVar v2, newVar v3, newVar v4
let newVar5 (v1,v2,v3,v4,v5) = newVar v1, newVar v2, newVar v3, newVar v4, newVar v5
let newVar6 (v1,v2,v3,v4,v5,v6) = newVar v1, newVar v2, newVar v3, newVar v4, newVar v5, newVar v6


let correctStr = function false -> "FAIL    :( "
                        | true ->  "Success :) "
let prTest test expected res = printf "%s %s returned %b\n" (correctStr (res=expected)) test res
let prNl() = printf "\n"


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

let rule13() = let x, xx, y , z = newVar4 ("13x", "13xx", "13y", "13z") 
               Rule ((Mix(x,y), Mix(y,xx)), [(x,z); (z,xx)])

let rule14() = let x, xx, y, yy , z, zz = newVar6 ("14x", "14xx", "14y", "14yy", "14z", "14zz") 
               Rule ((Mix(x,y), Mix(xx,yy)), [(x,z); (z,y); (xx,zz);(zz,yy); (z,zz)])
               
let rule15() = let a, b, c, x, xx, y = newVar6("15a", "15b", "15c", "15x", "15xx", "15yy")
               Rule ((Mix(x,xx),y), [(a,b); (b,c)])
               
let rule16() = let x, y, z = newVar3("16x", "16y", "16z")
               Rule ((x,y), [(z,x)])

let rulesA = [rule1; rule2; rule3; rule6]
let rulesB = [rule1; rule2; rule3; rule4; rule5; rule6; rule7; rule8]
let rulesC = [rule4; rule5; rule9; rule10; rule11; rule12]         // Rules 9,10 are slightly tricky. 
                                                                   // Focus on rules like the others first.
let rulesD = [rule13; rule11; rule12]
let rulesE = [rule1; rule13]
let rulesF = [rule1; rule14]
let rulesG = [rule11; rule12; rule14]
let rulesH = [rule15; rule11; rule12]
let rulesI = [rule15; rule11]
let rulesJ = [rule11; rule16]

let runTests () =

    suffices [rule1] (A, A) |> prTest "suffices [rule1] (A, A)" true
    suffices [rule1] (A, B) |> prTest "suffices [rule1] (A, B)" false
    prNl()

    suffices rulesA (A, B) |> prTest "suffices rulesA (A, B)" false
    suffices rulesA (Mix (A, B), A) |> prTest "suffices rulesA (Mix (A, B),A)" true
    suffices rulesA (Mix (Mix (A, B), B),A) |> prTest "suffices rulesA (Mix (Mix (A, B), B),A)" true
    suffices rulesA (Mix (Mix (B, B), B),A) |> prTest "suffices rulesA (Mix (Mix (B, B), B),A)" false
    suffices rulesA (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesA (Mix (Mix (B, B), B), Mix (B, B))" true
    suffices rulesA (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesA (Mix (Mix (A, B), B), Mix (B, A))" false
    prNl()

    suffices rulesB (A, B) |> prTest "suffices rulesB (A, B)" false
    suffices rulesB (Mix (A, B), A) |> prTest "suffices rulesB (Mix (A, B),A)" true
    suffices rulesB (Mix (Mix (A, B), B),A) |> prTest "suffices rulesB (Mix (Mix (A, B), B),A)" true
    suffices rulesB (Mix (Mix (B, B), B),A) |> prTest "suffices rulesB (Mix (Mix (B, B), B),A)" false
    suffices rulesB (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesB (Mix (Mix (B, B), B), Mix (B, B))" true
    suffices rulesB (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesB (Mix (Mix (A, B), B), Mix (B, A))" true
    prNl()

    suffices rulesC (A, B) |> prTest "suffices rulesC (A, B)" true
    suffices rulesC (Mix (A, B), A) |> prTest "suffices rulesC (Mix (A, B),A)" true
    suffices rulesC (Mix (Mix (A, B), B),A) |> prTest "suffices rulesC (Mix (Mix (A, B), B),A)" true
    suffices rulesC (Mix (Mix (B, B), B),A) |> prTest "suffices rulesC (Mix (Mix (B, B), B),A)" true
    suffices rulesC (Mix (Mix (B, B), B), Mix (B, B)) |> prTest "suffices rulesC (Mix (Mix (B, B), B), Mix (B, B))" false
    suffices rulesC (Mix (Mix (A, B), B), Mix (B, A)) |> prTest "suffices rulesC (Mix (Mix (A, B), B), Mix (B, A))" false
    prNl()
    
    suffices [rule13] (Mix(A,B), Mix(A,B)) |> prTest "suffices [rule13] (Mix(A,B), Mix(A,B))" false
    suffices [rule13] (Mix(A,B), Mix(B,B)) |> prTest "suffices [rule13] (Mix(A,B), Mix(B,B))" false
    suffices [rule13] (Mix(A,A), Mix(B,B)) |> prTest "suffices [rule13] (Mix(A,A), Mix(B,B))" false
    suffices [rule13] (Mix(A,B), Mix(B,A)) |> prTest "suffices [rule13] (Mix(A,B), Mix(B,A))" false
    suffices [rule13] (A,A) |> prTest "suffices [rule13] (A,A)" false
    prNl()
   
    suffices rulesD  (Mix(A,B), Mix(A,B)) |> prTest "suffices rulesD (Mix(A,B), Mix(A,B))" false
    suffices rulesD  (Mix(A,B), Mix(B,B)) |> prTest "suffices rulesD (Mix(A,B), Mix(B,B))" false
    suffices rulesD  (Mix(A,A), Mix(B,B)) |> prTest "suffices rulesD (Mix(A,A), Mix(B,B))" false
    suffices rulesD  (Mix(A,B), Mix(B,A)) |> prTest "suffices rulesD (Mix(A,B), Mix(B,A))" true
    prNl()
    
    suffices rulesE  (Mix(A,B), Mix(A,B)) |> prTest "suffices rulesE (Mix(A,B), Mix(A,B))" true
    suffices rulesE  (Mix(A,B), Mix(B,B)) |> prTest "suffices rulesE (Mix(A,B), Mix(B,B))" false
    suffices rulesE  (Mix(A,A), Mix(B,B)) |> prTest "suffices rulesE (Mix(A,A), Mix(B,B))" false
    suffices rulesE  (Mix(A,B), Mix(B,A)) |> prTest "suffices rulesE (Mix(A,B), Mix(B,A))" true
    prNl()
    
    suffices [rule14] (Mix(A,B), Mix(A,B)) |> prTest "suffices [rule14] (Mix(A,B), Mix(A,B))" false
    suffices [rule14] (Mix(A,B), Mix(B,B)) |> prTest "suffices [rule14] (Mix(A,B), Mix(B,B))" false
    suffices [rule14] (Mix(A,A), Mix(B,B)) |> prTest "suffices [rule14] (Mix(A,A), Mix(B,B))" false
    suffices [rule14] (Mix(A,B), Mix(B,A)) |> prTest "suffices [rule14] (Mix(A,B), Mix(B,A))" false
    suffices [rule14] (A,A) |> prTest "suffices [rule14] (A,A)" false
    prNl()

    suffices rulesF  (Mix(A,B), Mix(A,B)) |> prTest "suffices rulesF (Mix(A,B), Mix(A,B))" true
    suffices rulesF  (Mix(A,B), Mix(B,B)) |> prTest "suffices rulesF (Mix(A,B), Mix(B,B))" false
    suffices rulesF  (Mix(A,A), Mix(B,B)) |> prTest "suffices rulesF (Mix(A,A), Mix(B,B))" false
    suffices rulesF  (Mix(A,B), Mix(B,A)) |> prTest "suffices rulesF (Mix(A,B), Mix(B,A))" false
    prNl()
        
    suffices rulesG (Mix(A,A), Mix(B,B)) |> prTest "suffices rulesG (Mix(A,A), Mix(B,B))" true
    suffices rulesG (Mix(B,B), Mix(B,B)) |> prTest "suffices rulesG (Mix(B,B), Mix(B,B))" false
    suffices rulesG (Mix(A,A), Mix(A,A)) |> prTest "suffices rulesG (Mix(A,A), Mix(A,A))" false
    suffices rulesG (Mix(B,B), Mix(A,A)) |> prTest "suffices rulesG (Mix(B,B), Mix(A,A))" true
    prNl()
    
    suffices rulesH (Mix(A,A), A) |> prTest "suffices rulesH (Mix(A,A), A)" true
    suffices rulesH (Mix(B,B), B) |> prTest "suffices rulesH (Mix(B,B), B)" true
    suffices rulesH (Mix(A,A), B) |> prTest "suffices rulesH (Mix(A,A), B)" true
    suffices rulesH (Mix(B,B), A) |> prTest "suffices rulesH (Mix(B,B), A)" true
    suffices rulesH (Mix(A,A), Mix(B,B)) |> prTest "suffices rulesH (Mix(A,A), Mix(B,B))" true
    suffices rulesH (Mix(B,B), Mix(B,B)) |> prTest "suffices rulesH (Mix(B,B), Mix(B,B))" true
    suffices rulesH (Mix(A,A), Mix(A,A)) |> prTest "suffices rulesH (Mix(A,A), Mix(A,A))" true
    suffices rulesH (Mix(B,B), Mix(A,A)) |> prTest "suffices rulesH (Mix(B,B), Mix(A,A))" true
    prNl()

    suffices rulesI (Mix(A,A), A) |> prTest "suffices rulesI (Mix(A,A), A)" false
    suffices rulesI (Mix(B,B), B) |> prTest "suffices rulesI (Mix(B,B), B)" false
    suffices rulesI (Mix(A,A), B) |> prTest "suffices rulesI (Mix(A,A), B)" false
    suffices rulesI (Mix(B,B), A) |> prTest "suffices rulesI (Mix(B,B), A)" false
    suffices rulesI (Mix(A,A), Mix(B,B)) |> prTest "suffices rulesI (Mix(A,A), Mix(B,B))" false
    suffices rulesI (Mix(B,B), Mix(B,B)) |> prTest "suffices rulesI (Mix(B,B), Mix(B,B))" false
    suffices rulesI (Mix(A,A), Mix(A,A)) |> prTest "suffices rulesI (Mix(A,A), Mix(A,A))" false
    suffices rulesI (Mix(B,B), Mix(A,A)) |> prTest "suffices rulesI (Mix(B,B), Mix(A,A))" false
    prNl()
    
    suffices rulesJ (B, Mix(A,A)) |> prTest "suffices rulesJ (B, Mix(A,A))" true
    suffices rulesJ (B, Mix(A,Mix(A,B))) |> prTest "suffices rulesJ (B, Mix(A,Mix(A,B)))" true
    suffices rulesJ (A, Mix(A,A)) |> prTest "suffices rulesJ (A, Mix(A,A))" false
    suffices rulesJ (A, Mix(A,Mix(A,B))) |> prTest "suffices rulesJ (A, Mix(A,Mix(A,B)))" false
    suffices rulesJ (A, B) |> prTest "suffices rulesJ (A, B)" false
