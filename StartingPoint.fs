﻿module Paradigms.StartingPoint

#light

// Programming Paradigms Project

//module DistributedExperimentUnification

open System
open System.Collections.Generic

open Experiments
open ExperimentsTest
open Concurrency
open PrintingPrettyStuff

// Types for identifying labs and clients
type labID = int
type clientID = int


//////////  Project part 2 follows

///////////////////////////////////////////////////////////////////////////
/// The following is the simulation of the labs you should use test your code.
//     NOTE: threads must never contact labs except via DoExp, and only when when the lab is unused.
//     This includes creating locks on the lab objects.
//     You do NOT need to change this class. (You can change it, but I'm likely to change it back during marking.)
//////////////////////////////////////////////////////////////////////////
type lab (labID, rules) =
    let busy = ref false
    let usingClID = ref None  // Stores the client currently using the lab, if there is one.

    member this.Rules = rules
    
    member this.LabID = labID
    
    /// Send an experiment to a lab.  The continuation is called with the result of the experiment
    //  when it is complete.
    member this.DoExp delay (exp:exp) clID (continuation : bool->unit) =  
       startThread ("Lab"+ string labID) <| fun ()->
          if !busy then  let str = sprintf "BANG! lab%d explodes - host%d is already using it" labID (!usingClID).Value
                         prStamp -1 str "" //; failwith str // uncomment this to have the whole program fail.
          usingClID := Some clID              
          busy:=true
          sleep delay  // Doing experiment (the delay is provided by the client for ease of testing the prototype)
          busy:=false
          usingClID := None
          if random 2 = 1 then continuation true  else continuation false


//////////////////////////////////////////////////////////////////////////////////////////////////
// You may find the following useful to make actions run after a lock is released - this is otherwise 
// slightly tricky when those actions depend on variables with scopes within the holding of the lock.  E.g.
//     hlock obj <| fun later -> let v = something()
//                               later <| fun()-> someActionUsing v   // Makes the action happen after lock release.

/// Lock an object while running f, but also pass a "hook" to f for making functions run when the lock is released.  
let hLock obj f = let onUnlock = ref (fun() -> ())
                  let later action =  onUnlock := (let u = !onUnlock in fun () -> u(); action())
                  let res = lock obj <| fun()-> f later                 // Execute f, providing the later function
                  (!onUnlock)()
                  res                                   

                                                
///////////////////////////////////////////////////////////////////////////////////
/// Add your code for the second part here and below, including in the client class below.
////////////////////////////////////////////////////////////////////////////////////

// Hints:
// 1. You must be very careful here that your implementation is a suitable prototype for a decentralized system,
//    even though you are only building the prototype, not the final system.
// 2. One client's thread may call members on another client via the clients array - this is considered to 
//    indicate that in the final (non-prototype) system a request will sent via the network (and similarly for
//    the value returned by the member).   (This is usually called a Remote Method Invocation.)
// 3. Clients should NEVER pass (nor return) to each other data that is mutable or contains elements that are mutable.
//    Mutable data cannot be sent over the network in the final version.
// 4. Clients should never be contacted when they do not require a lab, and have no lab, except by other labs that
//    have been inactive and have yet to discover that the first client no longer holds a lab.
// 5. You will need a number of other members in the client class for clients to communicate with each other
//    in various situations.
// 6. You will need locking in some of the code in the this class, but be careful of deadlocks.
// 7. You will need some additional mutable state variables in the client class.
// 8. No static mutable variables are allowed, since these can't be decentralized. 
// 9. You will probably want to define a type for queues, but be careful not to pass mutable values between clients.
      

/// A class for clients that coordinate and unify experiments before sending them to labs.  
/// (You need to finish this class.)

let first  (a, _, _) = a
let second (_, b, _) = b
let third  (_, _, c) = c

type queue = (clientID*exp*int) list

and labMsg = (lab*queue)   

and client (id, numLabs) =
    let clients:client[] ref = ref Array.empty
    let labs:lab[] ref = ref Array.empty
    /// The client coordinating each lab, according to the most recent information known by this client.
    let lastKnownCoord = Array.init numLabs (fun labID -> labID)  // Initially client i has lab i, for i=0..numLabs-1
    let myLab:lab option ref= ref None
    let myQueue:queue option ref = ref None
    
    let IHave l = lastKnownCoord.[l] = id
    
    // printing functions for this client
    let prStr (pre:string) str = prIndStr id (sprintf "Client%d: %s" id pre) str 
    let pr (pre:string) res = prStr pre (sprintf "%A" res);
    
    /// Chooses an experiment to do from the list that suffices for ours (the first in the list) and
    /// the most others, then does it and sends results to people.
    let doExpFromList (theQueue:queue) (theLab:lab) = 
        let _, myExp, myDelay = List.head theQueue // The exp for this client.
        let otherCandidates =
            [for c, e, d in theQueue do if suffices theLab.Rules (e,myExp) then yield (e,d)]
        let candidates =
            if List.head otherCandidates = (myExp, myDelay) then otherCandidates
            else (myExp, myDelay) :: otherCandidates
        let counts = [
            for (ex, delay) in candidates do
                let addOneIfSuffices x (c,e,d) = if suffices theLab.Rules (ex, e) then x+1 else x
                // Count how many experiments this one suffices for.
                yield (ex, delay, List.fold addOneIfSuffices 0 theQueue) 
        ]
        let myMax (e, d, x) (ee, dd, xx) =
            if x > xx then (e,d,x)
            elif x < xx  then (ee,dd,xx)
            elif expSize e <= expSize ee then (e,d,x) //TODO can we make this delay instead?
            else (ee,dd,xx)
        let best = List.fold myMax (List.head counts) (List.tail counts)
        let result = ref None
        theLab.DoExp (second best) (first best) id (fun res -> result:=Some res)
        while (!result).IsNone do ()
        //TODO This is busy waiting, which isn't allowed - you'll need to fix it.
        //TODO tell ppl their exp result
        (!result).Value
    
    /// Passes our lab onto the client
    let rec passLabOn () =
        match !myLab, !myQueue with
            | Some l, Some q ->
                match q with
                    | [] -> ()
                    | (c,e,d) :: tail ->
                        if (!clients).[c].RTakeLab (l, q) then Array.set lastKnownCoord l.LabID c
                        else myQueue := Some tail; passLabOn ()
            | _ -> raise <| Exception "you've asked me to pass a lab on but I don't have one"
        
    member this.ClientID = id  // So other clients can find our ID easily
    member this.InitClients theClients theLabs =  
        clients:=theClients
        labs:=theLabs
        if(Array.length !labs < id) then 
            myLab := Some (!labs).[id]
            myQueue := Some []
            
    
    /// This will be called each time a scientist on this host wants to submit an experiment.
    member this.DoExp delay exp =
        //  The following code doesn't coordinate the clients at all.  Replace it with code that does.
        match !myLab with
            | Some l -> doExpFromList [this.ClientID,exp,delay] l
            | None -> false //TODO ask every lab holder that we be added to their lab's queue
            
    /// Tell this client to add an experiment to the queue for the given lab (if they currently hold it).
    /// Returns the id of the client that is now holding the lab if it has been passed on.
    member this.RAddToQueue (other:client) (l:labID) (e:exp) (d:int) : clientID option =
        if IHave l then
            match !myQueue with
                | None -> raise <| Exception "I thought I had a lab, but I don't"
                | Some q ->
                    myQueue := Some <| q @ [(other.ClientID, e, d)]
                    if q = [] then passLabOn ()
            None
        else
            Some lastKnownCoord.[l] // Tell other who we gave the lab to.

    /// Tell this client to take the lab. Returns true if it accepted it.
    member this.RTakeLab (msg:labMsg) : bool =
        if true then //TODO If I want the lab 
            myLab := Some <| fst msg
            myQueue := Some <| snd msg
            Array.set lastKnownCoord (fst msg).LabID id
            //TODO cancel my requests
            doExpFromList (snd msg) (fst msg)
            true
        else
            false
    
    /// Tell this client that we no longer need our experiment done.
    member this.RRemoveFromQueue cid = ()
        //TODO If we don't have the lab, tell them who has it
        // else, remove them from the queue
        

