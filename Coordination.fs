module Paradigms.Coordination

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

type expResult = bool

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
    member this.DoExp delay (exp:exp) clID (continuation : expResult->unit) =  
       startThread ("Lab"+ string labID) <| fun ()->
          lock this <| fun () -> 
              if !busy then  let str = sprintf "BANG! lab%d explodes - host%d is already using it" labID (!usingClID).Value
                             prStamp -1 str "" //; failwith str // uncomment this to have the whole program fail.
              usingClID := Some clID
              busy:=true
          sleep delay  // Doing experiment (the delay is provided by the client for ease of testing the prototype)
          lock this <| fun () ->
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

type clientState = Requesting | Canceling | Bored

type queue = (clientID*exp*int) list
and labMsg = (lab*queue)   

and client (id, numLabs) =
    let clients:client[] ref = ref Array.empty
    let labs:lab[] ref = ref Array.empty
    
    /// The lab that I own.
    let myLab:lab option ref= ref None
    /// The queue for the lab that I own. Make sure that modifications are mirrored in the
    /// effected clients' inQueueForLab.
    let myQueue:queue option ref = ref None
    /// Are we currently running an exp in our lab?
    let expRunning = ref false
    /// Did we manage to cancel all our requests
    let cancelsDone = ref true
    /// Our current state
    let myState = ref Bored
    /// Are we in the queue for each lab? Make sure to keep in sync with myQueue from each other lab.
    let inQueueForLab = Array.init numLabs (fun i -> false)
    /// The client coordinating each lab, according to the most recent information known by this client.
    let lastKnownOwner = Array.init numLabs (fun labID -> labID)  // Initially client i has lab i
    ///The result of our experiment
    let result:expResult option ref = ref None
    // Helper functions.
    let IHave l = lastKnownOwner.[l] = id
    
    
    // printing functions for this client
    let prStr (pre:string) str = prIndStr id (sprintf "Client%d: %s" id pre) str 
    let pr (pre:string) res = prStr pre (sprintf "%A" res);
    
    
    /// Sets the state to bored if it should be
    let checkIfBored (this:client) =
        lock this <| fun () ->
            pr "CheckIfBored" this.ClientID
            if !cancelsDone && Option.isSome !result then
                myState := Bored
                result := None
                //TODO wake up any DoExp threads that are waiting for a bored state

    
   
    let reportResult res = 
        prStr "result reported" ""
        lock result <| fun() -> if Option.isSome (!result) then () // we have jumped in between a previous
                                                                   // result being written and said result
                                                                   // being returned
                                else result := Some res
                                     wakeWaiters result
    let waitForResult (this:client) =
        lock result <| fun() -> waitFor result
                                pr "waitForResult with client" this.ClientID
                                let res = Option.get !result
                                checkIfBored this
                                res
    
    
    let peekClient maybeQ =
        match maybeQ with
            | None -> None
            | Some q -> match q with
                            | [] -> None
                            | (c,_,_) :: tail -> Some c

    /// Gets the locks for both this and other (in a consistant order) before running f.
    let doSafely (thisID:clientID) (otherID:clientID) f =
        let this = (!clients).[thisID]
        let other = (!clients).[otherID]
        if thisID < otherID then
            lock this <| (fun () -> lock other <| f) 
        else
            lock other <| (fun () -> lock this <| f)
        


    
    
  
    /// Safely passes our lab onto the client at the front of the queue (removing clients that reject the
    /// offer from the queue).
    let rec passLabOn () =
        let co = (peekClient !myQueue)
        if (Option.isNone co) || not (doSafely id co.Value <| (fun () -> // true iff done
            pr "passLabOn with myLab = " !myLab
            if !expRunning then true
            elif co <> peekClient !myQueue then false
            elif (!clients).[co.Value].RTakeLab (Option.get !myLab, Option.get !myQueue) then
                Array.set lastKnownOwner (!myLab).Value.LabID co.Value; true
            else myQueue := Some (!myQueue).Value.Tail; false ))
        then passLabOn ()

        
    /// Safely adds this client to the queue for a given lab, updating laskKnownOwner as needed.
    let rec aAddToQueue (this:client) (l:labID) (e:exp) (d:int) =
        let cid = lastKnownOwner.[l];
        if not (doSafely id cid (fun () ->
            pr ("aAddToQueue with client " + (string cid) + " looking for lab") l
            if !myState <> Requesting then true // We were too late, we are already have a plan for getting a result.
            elif lastKnownOwner.[l] <> cid then false // but lastKnownOwner might have changed before we got the lock
            else match (!clients).[cid].RAddToQueue this l e d with
                        | Some newClient -> Array.set lastKnownOwner l newClient; false
                        | None -> Array.set inQueueForLab l true; true )
        ) then aAddToQueue this l e d
        //TODO notify that the async is done
            
    /// Safely removes this client from the queue for a given lab, updating lastKnownOwner as needed.
    let rec aRemoveFromQueue (this:client) (l:labID) =
        if !myState <> Canceling then raise <| Exception "We are bored before all of our aRemoveFromQueue requests are done"

        let cid = lastKnownOwner.[l];
        if not (doSafely id cid (fun () -> // now we have a lock with cid
            pr ("aRemoveFromQueue with client " + string cid + " and lab") l
            if lastKnownOwner.[l] <> cid then false // but lastKnownOwner might have changed before we got the lock
            elif not inQueueForLab.[l] then true
            else match (!clients).[lastKnownOwner.[l]].RRemoveFromQueue this l with
                        | Some newClient -> Array.set lastKnownOwner l newClient; true
                        | None -> Array.set inQueueForLab l false; false )
        ) then aRemoveFromQueue this l
        //TODO notify that async is done
    
    let cancelAll (this:client) =
        lock this <| fun () ->
            pr "CancelAll" ""
            if !myState = Requesting then
                myState := Canceling
                cancelsDone := false
                Async.Start <| async {
                    [ for l in 0..numLabs-1 do
                        if inQueueForLab.[l] then
                           yield async { aRemoveFromQueue this l } ]
                    |> Async.Parallel
                    |> Async.Ignore
                    |> Async.RunSynchronously
                    lock this <| fun () ->
                        cancelsDone := true
                        checkIfBored this
                }
                

    
    /// Chooses an experiment to do from the list that suffices for ours (the first in the list) and
    /// the most others, then does it and sends results to people.
    let doExpFromList this (theQueue:queue) (theLab:lab) =
        if !myState <> Requesting then raise <| Exception "doExpFromList called while not in requesting state"
        let bestC, bestE, bestD, bestO = lock this <| (fun() -> 
            pr "DoExpFromList with lab " theLab.LabID
            let _, myExp, myDelay = List.head theQueue // The exp for this client.
            let otherCandidates = List.filter (fun (_,e,_) -> suffices theLab.Rules (e, myExp)) theQueue
            let candidates = if first <| List.head otherCandidates = id then otherCandidates
                             else List.head theQueue :: otherCandidates
            let others = List.map (fun (c, ex, delay) ->
                                       (c, ex, delay, List.filter (fun (_,e,_) -> suffices theLab.Rules (ex, e)) theQueue) )
                                  candidates
            let betterOf (c, e, d, o) (cc, ee, dd, oo) =
                if List.length o > List.length oo then (c, e,d,o)
                elif List.length o < List.length oo  then (cc, ee,dd,oo)
                elif expSize e <= expSize ee then (c, e,d,o) //TODO can we make this delay instead?
                else (cc, ee, dd, oo)
            let bestC, bestE, bestD, bestO = List.fold betterOf (List.head others) (List.tail others)
            // Remove done experiments from the queue.
            let sameClient (c,_,_) (cc,_,_) = c = cc
            myQueue := Some <| List.filter (fun x -> Option.isNone <| List.tryFind (sameClient x) bestO) theQueue
            expRunning := true
            cancelAll this
            bestC, bestE, bestD, bestO
        )
        // tell everyone to expect result
        let reportingQ = List.filter (fun (c, _, _)  ->
                                          doSafely id c (fun () -> (!clients).[c].RExpectResult theLab.LabID))
                                     bestO
        
        theLab.DoExp bestD bestE id (fun res ->
            reportResult res
            expRunning := false
            passLabOn ()
            // Give everyone their result.
            ignore <| List.map (fun (c, _, _) -> Async.Start <| async {(!clients).[c].RReportResult res theLab.LabID}) reportingQ
        )


    member this.ClientID = id  // So other clients can find our ID easily
    member this.InitClients theClients theLabs =  
        clients:=theClients
        labs:=theLabs
        if(id < Array.length !labs) then
            myLab := Some (!labs).[id]
            myQueue := Some []
            
    
    /// This will be called each time a scientist on this host wants to submit an experiment.
    member this.DoExp delay ex : expResult =
        lock this <| fun () ->
            prStr "Doing exp" " :) "
            match !myState with
                | Bored ->
                    myState := Requesting
                    match !myLab with
                        | Some l -> doExpFromList this [this.ClientID,ex,delay] l // Assuming the lab is idle
                        | None -> ignore [ for l in 0..numLabs-1 do Async.Start <| async { aAddToQueue this l ex delay } ]
                | _ -> () // TODO we should block until we have finished with the current exp.
        waitForResult this
        
    /// Tell this client to add an experiment to the queue for the given lab (if they currently hold it).
    /// Returns the new lab owner if we don't own it anymore.
    member this.RAddToQueue (other:client) (l:labID) (e:exp) (d:int) : clientID option =
        doSafely id other.ClientID <| (fun () ->
            pr ("RAddToQueue for lab "+string l+" with client") other.ClientID
            if IHave l then
                myQueue := Some <| (Option.get !myQueue) @ [(other.ClientID, e, d)]
                // If they are the only one in the queue, give them the lab right away.
                Async.Start <| async { passLabOn () }
                None
            else Some lastKnownOwner.[l] // Tell other who we gave the lab to.
        )

    /// Tell this client that we no longer need our experiment done.
    /// Returns the new lab owner if we don't own the lab anymore.
    member this.RRemoveFromQueue (other:client) (l:labID) : clientID option =
        doSafely id other.ClientID <| (fun () -> 
            pr ("RRemoveFromQueue for lab "+string l+" with client") other.ClientID
            if IHave l then
                myQueue := Some <| List.filter (fun (c,e,d) -> c <> other.ClientID) (Option.get !myQueue)
                None
            else Some lastKnownOwner.[l]
        )

    /// Tell this client to take the lab. Returns true if it accepted it.
    member this.RTakeLab (msg:labMsg) : bool =
        lock this <| (fun () ->
            pr "RTakeLab" msg
            Array.set inQueueForLab (fst msg).LabID false
            if true then
                myLab := Some <| fst msg
                myQueue := Some <| snd msg
                Array.set lastKnownOwner (fst msg).LabID id
                myState := Requesting //TODO check this
                doExpFromList this (snd msg) (fst msg)
                true
            else false
        )
    
    /// Tell this client we have a result for them (and have thus removed them from the queue).
    member this.RReportResult (r:expResult) (l:labID) =
        lock this (fun () ->
            pr "RREportResult with lab " l
            reportResult r
        )

    member this.RExpectResult (l:labID) = 
        lock this (fun () ->
            pr "RExpectResult with lab " l
            if !myState = Requesting then
                Array.set inQueueForLab l false
                cancelAll this
                true
            else false
        )
