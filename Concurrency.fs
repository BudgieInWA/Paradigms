module Paradigms.Concurrency

open System
open System.Threading

/// Make a named thread that runs f
let mkThread name f = Thread (ThreadStart f, Name=name)

/// Make a named thread and start it immediately
let startThread name f = (mkThread name f).Start()

/// wait until another thread signals the given object has changed
let waitFor obj = ignore(Monitor.Wait obj)

/// wake up all threads waiting on a given object
let wakeWaiters obj = Monitor.PulseAll obj

/// Generate a random positive integer in 1..n
let random = let myRand = Random() in myRand.Next
             
/// Sleep for n milliseconds
let sleep (n:int) = Thread.Sleep n

/// Sleep for between 1 and some number of milliseconds
let sleepUpTo (n:int) = sleep (1 + random n)

