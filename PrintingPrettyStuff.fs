module Paradigms.PrintingPrettyStuff

open Paradigms.Concurrency

open System
open System.Threading
open System.Collections.Generic
open System.Windows.Forms          // Note that you'll need references for System.Windows.Forms and
open System.Drawing                // System.Drawing  (See the "solution explorer", on the right in VS...)  ----->>>

////////////////////////////////////// Debugging Output 

Application.EnableVisualStyles()
let newLine = Environment.NewLine

/// A window with colour text output indicating on the object a thread is in.
type outputForm() =
       let pr = ref (fun cl str -> ())  // stub - the actual function is put in later
       let ready = ref false
       member this.Pr str = (!pr) str
       member this.WaitUntilReady() = lock this <| fun()-> while not !ready do waitFor this
       member this.Run() =                              // You can adjust the window size, etc, below as appropriate.
           let form = new Form(Text="Event interleaving",Size=Drawing.Size(1600, 1000),Visible=true)  
           let textB = new RichTextBox(Dock= DockStyle.Fill, Multiline=true, ReadOnly=true, 
                                       Font=new Font(familyName="Courier New", emSize=8.0f), BackColor=Color.Black)
           do textB.SelectionStart<-textB.TextLength ; textB.ScrollToCaret()
           let color n = let colors = [| Color.DodgerBlue; Color.Coral; Color.OliveDrab; Color.Violet;  Color.Gold; 
                                         Color.DimGray; Color.IndianRed;  Color.GreenYellow;  Color.BlueViolet; Color.White |]
                         if n>=0 then colors.[n%colors.Length] else Color.White
           pr := fun cl str -> let doPr() = textB.SelectionStart <- textB.TextLength; textB.SelectionColor<-color cl; 
                                            textB.SelectedText <- str+newLine; form.Show()                                          
                               if textB.InvokeRequired then 
                                  form.Invoke (Action doPr) |> ignore
                               else doPr()
           do form.Controls.Add(textB)
           do form.Show()
           do ready:=true; lock this <| fun()-> wakeWaiters this
           textB.Focus() |> ignore
           Application.Run(form)

let form = outputForm()                        
startThread "WinForms" form.Run
form.WaitUntilReady()

/// The time the system started.
let startTime = ref DateTime.Now // This is reset when a test sequence is started.

// Various functions for printing events to both the console and the coloured events window.
// They generally can be easily added at the end of a line to print a result via |> prIdent n prefixString
// Use a clientID of -1 for a print not related to a client.
let prLock = ref ()
let prRaw clientID str = lock prLock <|fun()->form.Pr clientID str; printfn "%s" str
let prFmt format args = prRaw 0 (sprintf format args)                                
let prStamp clientID pre str = let prefix = sprintf "%6.0f %s" (DateTime.Now - !startTime).TotalMilliseconds pre
                               let indent =  String.map (fun _-> ' ')  prefix
                               prRaw clientID ((sprintf "%s %s" prefix str).Replace("\n","\n"+indent))
let pr clientID pre res = prStamp clientID pre (sprintf "%A" res) ; res
let pr0 pre res = pr 0 pre res
let prIndStr clientID pre str = let indent =  String ( Array.map (fun _-> ' ') [|1..8+8*clientID|] )
                                prStamp clientID (indent+pre) str
let prIndent clientID pre res = prIndStr clientID pre (sprintf "%A" res); res

// These can be handy for debugging: swap them to the commented versions to turn on debug prints.
// (The VS threads window may also be useful for debugging.)
let dbg clientID pre res = res // pr clientID pre res
let dbgIndent clientID pre res = res // prIdent clientID pre res 

