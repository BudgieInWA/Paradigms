module Paradigms.Main
open ExperimentsTest
open CoordinationTests

[<EntryPoint>]
let main args =
    //runExperimentTests ()
    runRandomCoordinationTests ()
    0
