module Paradigms.CoordinationTests

open System
open System.Threading

open Paradigms.Coordination
open Paradigms.Experiments
open Paradigms.ExperimentsTest
open Paradigms.PrintingPrettyStuff
open Paradigms.Concurrency

open System.Collections.Generic
open System.Windows.Forms          // Note that you'll need references for System.Windows.Forms and
open System.Drawing                // System.Drawing  (See the "solution explorer", on the right in VS...)  ----->>>


//////////////////////////////////////////////////////////////// Top level testing code follows.      

// The following creates and initializes the instances of the lab and client classes
let mkClientsAndLabs numClients (labsRules: labRules list) = 
    let numLabs = labsRules.Length
    let clients = [| for i in 0..numClients-1 -> client (i, numLabs) |]
    let labs = [| for (i,rules) in List.zip [0..numLabs-1] labsRules -> lab (i,rules) |]
    Array.iter (fun (cl:client) -> cl.InitClients clients labs) clients
    (clients, labs)

let prClient cl pre str = prIndStr cl (sprintf "Host%d: %s" cl pre) str  // for client output



// Some simple testing code follows.  You almost certainly want to add your own tests to this.
// scheduledClient and randomTest are designed to make it easy to build other tests.

/// This function runs a test where the requests and times for each client are specified in a list (see below).
/// The list contains tuples (delayBeforeRequestTime, busyDoingExpTime, exp) in sequence.
let scheduledClient (clients:client[]) clID sched = mkThread ("Client"+string clID) <| fun () ->
    sched |> List.iteri (fun i (delay,busy,exp) ->
             sleep delay
             prClient clID "" (sprintf "requests %A" exp)
             prIndent clID (sprintf "Exp%d%c result:" clID (char (i+(int 'a')))) (clients.[clID].DoExp busy exp) |> ignore )
    prClient clID "COMPLETED EXPERIMENTS" ""
    
/// Run the threads for a test, and wait for them to finish.    
let doTest threads = startTime:=DateTime.Now
                     prStamp -1 "Test started" ""
                     List.iter (fun (th:Thread)-> th.Start()) threads
                     List.iter (fun (th:Thread)-> th.Join()) threads
                     prStamp -1 "" "ALL HOSTS COMPLETED"


// The following creates a random test via the scheduledTest class above.
let randBase() = match random 2 with 0->A | 1->B

let rec randTerm () =     
    let rec intern maxDepth = match random 2, maxDepth with
                          | 1, _ -> randBase()
                          | _, 1 -> randBase()
                          | _ -> Mix (intern (maxDepth-1), intern (maxDepth-1))
    Mix (intern 2, intern 2)
 
/// Create a random test thread for a client-ID with the given average times and number of experiment requests.
let randomTestClient clients clID avgWait avgBusyTime numExp =
    scheduledClient clients clID 
       ( List.map (fun _ -> (random avgWait*2, random avgBusyTime*2, randTerm() )) [1..numExp] )

let randomTest avgWait avgBusyTime numExp numClients labsRules =
    let clients, _ = mkClientsAndLabs numClients labsRules 
    doTest [for i in 0..numClients-1 -> randomTestClient clients i avgWait avgBusyTime numExp  ]

do let clients, _ = mkClientsAndLabs 5 [rulesA; rulesB] 
   doTest [scheduledClient clients 0 [(0, 500, A)];     // Request a lab at the very start, use for "A" for 0.5 seconds
           scheduledClient clients 1 [(200, 300, Mix (Mix (A,Mix (A,A)),B))] ;   // Request after 0.2s, release 0.3s later.
                 
           scheduledClient clients 2 [(300, 200, Mix (A,Mix (A,A)))];   // These three will all be waiting for a lab.
           scheduledClient clients 3 [(400, 200, Mix (A,A))];           // Client 2 should include the others as guests.
           scheduledClient clients 4 [(400, 200, A)]
          ]


let runRandomCoordinationTests () =
    while true do randomTest 10 50 4 8 [rulesB; rulesB] |> ignore            // A smaller random test.
   // randomTest 5 20 5 20 [rulesA; rulesB; rulesC] |> ignore    // A larger random test.
