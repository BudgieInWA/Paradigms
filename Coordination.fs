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
       prStamp -1 (sprintf "Lab %d doing exp for client %d" labID clID) "" 
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
              prStamp -1 (sprintf "Lab %d done, calling continuation soon" labID) "" 
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

type clientRequestState = Requesting | Canceling | Bored
type clientLabState     = Searching  | Using     | Idle  | Absent

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
    
    // Self explaining, used to delay starting of new experiments prematurely
    let tellingClientsToExpectResults = ref false
    let tellingClientsResults = ref false
    
    /// Clients that we need to give a result to.
    let clientWantsOurResult:Set<clientID> ref = ref Set.empty
    let lastUsedLab = ref -1
    /// Our current state
    let myRequestState = ref Bored
    let myLabState     = if id < numLabs then ref Idle else ref Absent
    /// Are we in the queue for each lab?
    /// We only set an element to true when we know we have been accepted into the queue.
    /// We only set an element to false when we know that our entry in the queue has been canceled, we have received a lab, or we receive a result.
    let inQueueForLab = Array.init numLabs (fun i -> false)
    /// The client coordinating each lab, according to the most recent information known by this client.
    let lastKnownOwner = Array.init numLabs (fun labID -> labID)  // Initially client i has lab i

    let restartLock = "Wait on me while you're not allowed to restart"

    ///The result of our experiment
    let result:expResult option ref = ref None
    // Helper functions.
    let IHave l = lastKnownOwner.[l] = id
    
    
    // printing functions for this client
    let prStr (pre:string) str = prIndStr id (sprintf "Client%d: %s" id pre) str 
    let pr (pre:string) res = prStr pre (sprintf "%A" res);
    
    /// Should we wait before starting a new experiment?
    /// Be sure to lock all modifications to these vars. TODO
    let cannotStartExp () =    !tellingClientsResults
                            || !tellingClientsToExpectResults
                            || !myRequestState <> Bored
                            || !myLabState = Searching
                            || !myLabState = Using
                            || Array.exists (fun b -> b) inQueueForLab

    let reportResult res = 
        lock result <| fun() -> if Option.isSome (!result) then prStr "result igored" "";() // we have jumped in between a previous
                                                                   // result being written and said result
                                                                   // being returned
                                else result := Some res
                                     prStr "result reported" ""
                                     wakeWaiters result

    let waitForResult (this:client) =
        lock result <| fun() -> prStr "waitForResult" ""
                                waitFor result
                                let res = Option.get !result
                                res
    
    
    let peekClient maybeQ =
        match maybeQ with
            | None -> None
            | Some q -> match q with
                            | [] -> None
                            | (c,_,_) :: tail -> Some c

    /// Gets the locks for both this and other (in a consistant order) before running f.
    let rec doSafely (thisID:clientID) (otherID:clientID) f =
        if thisID < otherID then doSafely otherID thisID f
        else
            prStamp -1 (sprintf "Try   locks with %d, %d" thisID otherID) ""
            let this = (!clients).[thisID]
            let other = (!clients).[otherID]
            
            let ret = lock this <| (fun () -> lock other <| fun () ->   prStamp -1 (sprintf "Got   locks with %d, %d" thisID otherID) ""
                                                                        f ()) 
            prStamp -1 (sprintf "Done  locks with %d, %d" thisID otherID) ""
            ret

        


  
    /// Safely passes our lab onto the client at the front of the queue (removing clients that reject the
    /// offer from the queue).
    /// LabState should be Idle
    let rec passLabOn this =
        let co = (peekClient !myQueue)
        if (Option.isNone co) then prStr "passLabOn but Q is empty," "stopping"; ()
        elif not (doSafely id co.Value <| fun () -> // true iff done
            prStr "passLabOn" ""
            if !myLabState <> Idle then prStr "done, I'm not idle" ""; true
            elif co <> peekClient !myQueue then prStr "queue changed, going again" ""; false
            elif (!clients).[co.Value].RTakeLab this (Option.get !myLab, Option.get !myQueue) then
                Array.set lastKnownOwner (!myLab).Value.LabID co.Value
                pr "Passed on to client" co.Value
                lock restartLock <| fun() ->
                    myLab := None
                    myQueue := None
                    myLabState := Absent
                    wakeWaiters restartLock
                true
            else myQueue := Some (!myQueue).Value.Tail; prStr "was rejected" ""; false
        )
        then passLabOn this

        
    /// Safely adds this client to the queue for a given lab, updating laskKnownOwner as needed.
    let rec aAddToQueue (this:client) (l:labID) (e:exp) (d:int) =
        let cid = lastKnownOwner.[l];
        if not (doSafely id cid <| fun () ->
            pr ("aAddToQueue with client " + (string cid) + " looking for lab") l
            if !myRequestState <> Requesting then prStr "state was not requesting" ""; true // We were too late, we are already have a plan for getting a result.
            elif lastKnownOwner.[l] <> cid then prStr "lastKnownOwners changed" ""; false // but lastKnownOwner might have changed before we got the lock
            else match (!clients).[cid].RAddToQueue this l e d with
                        | Some newClient -> Array.set lastKnownOwner l newClient; prStr "lab moved" ""; false
                        | None -> lock restartLock <| fun () -> Array.set inQueueForLab l true; prStr "accepted" ""; true
        ) then aAddToQueue this l e d
            
    /// Safely removes this client from the queue for a given lab, updating lastKnownOwner as needed.
    let rec aRemoveFromQueue (this:client) (l:labID) =
        if !myRequestState <> Canceling then raise <| Exception "We are bored before all of our aRemoveFromQueue requests are done"

        let cid = lastKnownOwner.[l];
        if not (doSafely id cid <| fun () -> // now we have a lock with cid
            pr ("aRemoveFromQueue with client " + string cid + " and lab") l
            if lastKnownOwner.[l] <> cid then prStr "lastKnownOwner changed" ""; false // but lastKnownOwner might have changed before we got the lock
            elif not inQueueForLab.[l] then pr "I'm not actually in q for lab" l; true
            else match (!clients).[lastKnownOwner.[l]].RRemoveFromQueue this l with
                        | Some -1 -> true // They have the lab, but they didn't remove us, someone else did. (we expect to get a result from them).
                        | Some newClient -> Array.set lastKnownOwner l newClient; prStr "lab moved" ""; false
                        | None -> lock restartLock <| fun () -> (Array.set inQueueForLab l false; wakeWaiters restartLock); prStr "removed" ""; true // None means we were removed from their queue or set
        ) then aRemoveFromQueue this l
    
    let cancelAllExcept (this:client) (thatLab:labID option) =
        lock this <| fun () ->
            prStr "CancelAll" ""
            if !myRequestState = Requesting then
                myRequestState := Canceling
                Async.Start <| async {
                    [ for l in 0..numLabs-1 do
                        if (thatLab.IsNone || thatLab.Value <> l) && inQueueForLab.[l] then
                           yield async { aRemoveFromQueue this l } ]
                    |> Async.Parallel
                    |> Async.Ignore
                    |> Async.RunSynchronously
                    lock this <| fun () ->
                        prStr "all my cancels finished" ""
                        lock restartLock <| fun() ->
                            myRequestState := Bored
                            wakeWaiters restartLock
                }
                

    
    /// Chooses an experiment to do from the list that suffices for ours (the first in the list) and
    /// the most others, then does it and sends results to people.
    /// Must have a lock on this
    let startUsingLab this (theQueue:queue) (theLab:lab) =
        if !myLabState <> Idle then raise <| Exception "startUsingLab called while lab not idle"
        pr "startUsingLab with lab" theLab.LabID
        
        let myID, myExp, myDelay = List.head theQueue // The exp for this client.
        if myID <> id then raise <| Exception "Called startUsingLab when we aren't at the front of the queue"
        let candidates = List.filter (fun (c,e,_) -> c = myID || suffices theLab.Rules (e, myExp)) theQueue // Experiments that we are considering doing.

        // Calculate the set of experiments that each candidate will suffice for.
        let others = List.map (fun (c, e, delay) ->
                                    (c, e, delay, List.filter (fun (cc,ee,_) -> cc = c || suffices theLab.Rules (e, ee)) theQueue) )
                                candidates
        let betterOf (c, e, d, o) (cc, ee, dd, oo) =
            if List.length o > List.length oo then (c, e,d,o)
            elif List.length o < List.length oo  then (cc, ee,dd,oo)
            elif expSize e <= expSize ee then (c, e,d,o)
            else (cc, ee, dd, oo)
        let bestC, bestE, bestD, bestO = List.fold betterOf (List.head others) (List.tail others)
        // Remove done experiments from the queue.
        let sameClient (c,_,_) (cc,_,_) = c = cc
        myQueue := Some <| List.filter (fun x -> not <| List.exists (sameClient x) bestO) theQueue
        
        myLabState := Using

        // tell everyone to expect result
        tellingClientsToExpectResults := true
        clientWantsOurResult := new Set<clientID> ( [ for (c,_,_) in bestO do if c <> id then yield c ] )
        lastUsedLab := theLab.LabID
        async {
            List.map (fun c -> async {
                    doSafely id c <| fun () ->
                        if Option.isNone !result && Set.contains c !clientWantsOurResult then // if not, we're too late
                            prStr ("Telling client " + string c + " to expect our result") ""
                            (!clients).[c].RExpectResult this theLab.LabID
                }) <| Set.toList !clientWantsOurResult
            |> Async.Parallel
            |> Async.Ignore
            |> Async.RunSynchronously
            
            
            lock restartLock <| fun () -> tellingClientsToExpectResults := false
                                          prStr "done telling clients to expect results" ""
                                          wakeWaiters restartLock
        }
        |> Async.Start
        
        theLab.DoExp bestD bestE id <| fun res ->
            prStr "continuation called" ""
            lock this <| fun () ->
                prStr "continuation running" ""
                reportResult res
                myLabState := Idle
                // Give everyone their result.
                tellingClientsResults := true
                async {
                    List.map (fun c -> async {
                            doSafely id c <| fun () ->
                                if (!clientWantsOurResult).Contains c then
                                    pr "Reporting result to client" c
                                    (!clients).[c].RReportResult this res theLab.LabID // c will note that they are no longer in the queue...
                                    clientWantsOurResult := (!clientWantsOurResult).Remove c // ...so we remove them
                                else pr "NOT reporting result to client" c

                        }) (Set.toList !clientWantsOurResult)
                    |> Async.Parallel
                    |> Async.Ignore
                    |> Async.RunSynchronously
            
                    
                    lock restartLock <| fun() -> tellingClientsResults := false
                                                 prStr " done telling clients results" ""
                                                 wakeWaiters restartLock
                }
                |> Async.Start
            // Some time in the future:
            passLabOn this


    member this.ClientID = id  // So other clients can find our ID easily
    member this.InitClients theClients theLabs =  
        clients:=theClients
        labs:=theLabs
        if(id < Array.length !labs) then
            myLab := Some (!labs).[id]
            myQueue := Some []
            
    
    /// This will be called each time a scientist on this host wants to submit an experiment.
    member this.DoExp delay ex : expResult =
        
        lock restartLock <| fun() -> while (cannotStartExp ()) do waitFor restartLock
                
        lock this <| fun () ->
            match !myRequestState, !myLabState with
                    | Bored, Idle ->
                        prStr "Doing exp" ":)"
                        result := None
                        myRequestState := Bored
                        // We jump the queue here, because it would be harder not to.
                        myQueue := Some <| [this.ClientID,ex,delay] @ (!myQueue).Value
                        startUsingLab this (!myQueue).Value (!myLab).Value
                        pr "done startUsingLab, now about to wait" ""
                    | Bored, Absent ->
                        prStr "Requesting a lab" " :( "
                        result := None
                        myRequestState := Requesting
                        myLabState := Searching
                        ignore [ for l in 0..numLabs-1 do Async.Start <| async { prStr "started request async" ""; aAddToQueue this l ex delay } ]
                                
                    | _ -> raise <| Exception "SHOULD NOT BE HERE (client.DoExp)"
        waitForResult this
                    
        
        
    /// Tell this client to add an experiment to the queue for the given lab (if they currently hold it).
    /// Returns the new lab owner if we don't own it anymore.
    member this.RAddToQueue (other:client) (l:labID) (e:exp) (d:int) : clientID option =
        pr ("RAddToQueue for lab "+string l+" from client") other.ClientID
        if IHave l then
            myQueue := Some <| (Option.get !myQueue) @ [(other.ClientID, e, d)]
            Async.Start <| async { passLabOn this }
            pr ("added client "+(string other.ClientID)+" to Q for lab") l
            None
        else
            pr "not adding because we don't have lab" l
            Some lastKnownOwner.[l] // Tell other who we gave the lab to.

    /// Tell this client that we no longer need our experiment done.
    /// Returns the new lab owner if we don't own the lab anymore, or -1 if someone else has them in their set.
    member this.RRemoveFromQueue (other:client) (l:labID) : clientID option =
        if !lastUsedLab = l && Set.contains other.ClientID !clientWantsOurResult then
            pr ("RRemoveFromQueue for lab "+string l+" from client") other.ClientID
            clientWantsOurResult := (!clientWantsOurResult).Remove other.ClientID // Because we removed other...
            None // ...they will note that they are no longer in the queue
        elif IHave l then
            pr ("RRemoveFromQueue for lab "+string l+" from client") other.ClientID
            if List.exists (fun (c,_,_) -> c = other.ClientID) (!myQueue).Value then
                myQueue := Some <| List.filter (fun (c,e,d) -> c <> other.ClientID) (Option.get !myQueue) // because we removed other...
                None // they will note that they are no longer in the queue
            else Some -1 // We have not removed them from the queue, someone else has
        else
            pr ("not removing because we don't have lab" + string l+" from client") other.ClientID
            Some lastKnownOwner.[l]

    /// Tell this client to take the lab. Returns true if it accepted it.
    member this.RTakeLab (other:client) (msg:labMsg) : bool =
        lock restartLock <| fun () -> Array.set inQueueForLab (fst msg).LabID false // other will remove us
                                      wakeWaiters restartLock
        match !myRequestState, !myLabState with
            | Requesting, Searching ->
                pr ("RTake Lab from client " + string other.ClientID) msg
                myLab := Some <| fst msg
                myQueue := Some <| snd msg
                Array.set lastKnownOwner (fst msg).LabID id
                cancelAllExcept this None
                myLabState := Idle
                startUsingLab this (!myQueue).Value (!myLab).Value
                true
            | _ -> pr ("Rejecting the lab " + string other.ClientID ) msg; false
    
    /// Tell this client we have a result for them (and have thus removed them from the queue).
    member this.RReportResult (other:client) (r:expResult) (l:labID) = 
        pr ("RReportResult for lab "+string l+" from client") other.ClientID
        assert inQueueForLab.[l]
        reportResult r
        match !myRequestState, !myLabState with
            | Requesting, Searching ->
                prStr "Also, we were looking for a lab, but not anymore" ""
                myLabState := Absent
                cancelAllExcept this None
            | _ -> ()
        lock restartLock <| fun () -> Array.set inQueueForLab l false // other will remove us from their set
                                      wakeWaiters restartLock

    member this.RExpectResult (other:client) (l:labID) =
        match !myRequestState, !myLabState with
            | Requesting, Searching ->
                pr ("RExpectResult for lab " + string l+" from client") other.ClientID
                myLabState := Absent
                cancelAllExcept this <| Some l
            | _ -> pr ("Told to expect result (though we don't need it) from lab "+string l+" from client") other.ClientID
