﻿// Code from Hansen and Rischel: Functional Programming using F#     1       6/12 2012
// Chapter 13: Asynchronous and parallel computations.          Revised MRH 25/11 2013 
// Code from Section 13.5: 13.5 Reactive programs.


// Prelude
open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 
open System.Text.RegularExpressions

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;

// An asynchronous event queue kindly provided by Don Syme 
type AsyncEventQueue<'T>() = 
    let mutable cont = None 
    let queue = System.Collections.Generic.Queue<'T>()
    let tryTrigger() = 
        match queue.Count, cont with 
        | _, None -> ()
        | 0, _ -> ()
        | _, Some d -> 
            cont <- None
            d (queue.Dequeue())

    let tryListen(d) = 
        if cont.IsSome then invalidOp "multicast not allowed"
        cont <- Some d
        tryTrigger()

    member x.Post msg = queue.Enqueue msg; tryTrigger()
    member x.Receive() = 
        Async.FromContinuations (fun (cont,econt,ccont) -> 
            tryListen cont)



// The window part
let window =
  new Form(Text="Nim", Size=Size(500, 500))

//let ansBox = new TextBox(Location=Point(150,150),Size=Size(200,25))

let startButton =
  new Button(Location=Point(50,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="HARD")

let diffButton =
  new Button(Location=Point(200,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="EASY")

let clearButton =
  new Button(Location=Point(350,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="CLEAR")

let endTurnButton =
  new Button(Location=Point(750,65),MinimumSize=Size(100,50),
              MaximumSize=Size(100,50),Text="END TURN")

let cancelButton =
  new Button(Location=Point(500,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="ABORT")

let combo = new ComboBox(Location=Point(100,35), DataSource=[|"http://www2.compute.dtu.dk/~mire/02257/nim1.game";"http://www2.compute.dtu.dk/~mire/02257/nim2.game";"http://www2.compute.dtu.dk/~mire/02257/nim3.game";"http://www2.compute.dtu.dk/~mire/02257/nim4.game"|], Width=300)

let mutable buttons = Array.empty

let disable bs = 
    for b in [startButton;clearButton;cancelButton;diffButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

// An enumeration of the possible events 
type Message = | Start of string * bool | Clear | Cancel | Web of string | Error | Cancelled 

//exception UnexpectedMessage

// The dialogue automaton 
let ev = AsyncEventQueue()
let optimal = true
let rec ready() = 
  async {Seq.iter(fun x -> window.Controls.Remove x) buttons

         disable [cancelButton]
         endTurnButton.Visible <- false
         let! msg = ev.Receive()
         match msg with
         | Start (url, diff) -> return! loading(url)
         | Clear     -> return! ready()
         | _         -> failwith("ready: unexpected message")}
  
// Sets up the board from chosen url
and loading(url) =
  async {use ts = new CancellationTokenSource()

          // start the load
         Async.StartWithContinuations
             (async { let webCl = new WebClient()
                      let! html = webCl.AsyncDownloadString(Uri url)
                      return html },
              (fun html -> ev.Post (Web html)),
              (fun _ -> ev.Post Error),
              (fun _ -> ev.Post Cancelled),
              ts.Token)

         disable [startButton; diffButton; clearButton]
         endTurnButton.Visible <- true
         let! msg = ev.Receive()
         match msg with
         | Web html ->
             let l = [ for x in Regex("\d+").Matches(html) -> x.Value ]
             let lulz = List.map (fun x -> if int x <= 9 && int x > 0 then x else string 9) l
             //let ans = System.String.Concat(lulz)

             let buttonPos text x y = new Button(Text=text, Top=(x+100), Left=y, Size=Size(20,20), BackColor=Color.Aqua)
             
             buttons <- Seq.toArray(seq{ for y in 1..lulz.Length -> (buttonPos "-" (y*50) (1300)) } |> Seq.cast<Control>)
             endTurnButton.Top<- (Array.last buttons).Top + 50
             endTurnButton.Left<- (Array.last buttons).Left - 50

             let pb = new PictureBox()
             pb.Image <- Image.FromFile("hatteland.png")
             pb.SizeMode <- PictureBoxSizeMode.AutoSize
             window.Controls.Add(pb)
             window.Controls.AddRange buttons
             let mutable lst = Array.empty

             return! finished("splat")
         | Cancel  -> ts.Cancel()
                      return! cancelling()
         | _       -> failwith("loading: unexpected message")}

and player() =
  async {disable [startButton; diffButton; clearButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}


and ai(lst) =
  async {//ansBox.Text <- "Cancelling"
         
         disable [startButton; diffButton; clearButton; cancelButton]

         //let mutable lst = [|1;2;3|]
         let calc_m lst = Array.fold (fun x m -> x ^^^ m) 0 lst
         let maxIndex lst = Array.findIndex (fun x -> x = Array.max lst) lst
         let m = calc_m lst
         if m <> 0 then
             for i = 0 to lst.Length-1 do
                 let tmp = lst.[i] ^^^ m
                 if tmp < lst.[i] then
                     lst.[i] <- tmp
         else
             let maxI = maxIndex lst
             lst.[maxI] <- lst.[maxI]-1

         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}

and cancelling() =
  async {//ansBox.Text <- "Cancelling"
         
         disable [startButton; diffButton; clearButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}

and finished(s) =
  async {//ansBox.Text <- s
         
         disable [startButton; diffButton; cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Clear -> return! ready()
         | _     ->  failwith("finished: unexpected message")}

// Initialization
//window.Controls.Add ansBox
window.Controls.Add startButton
window.Controls.Add diffButton
window.Controls.Add clearButton
window.Controls.Add cancelButton
window.Controls.Add endTurnButton
window.Controls.Add combo

(*
for i in 0 .. buttons.Length - 1 do
    window.Controls.Add new Button(Location=Point(500,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="CANCEL")

    let array1 = Array.create 10 ""
    for i in 0 .. array1.Length - 1 do
        Array.set array1 i (i.ToString())
*)
//let games = ["http://www2.compute.dtu.dk/~mire/02257/nim1.game";"http://www2.compute.dtu.dk/~mire/02257/nim2.game";"http://www2.compute.dtu.dk/~mire/02257/nim3.game";"http://www2.compute.dtu.dk/~mire/02257/nim4.game"]
//let rnd = System.Random()
//printfn "%s" (rnd.ToString())
//let hat = List.item(rnd.Next(games.Length)) games

startButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), true)))
diffButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), false)))
clearButton.Click.Add (fun _ -> ev.Post Clear)
cancelButton.Click.Add (fun _ -> ev.Post Cancel)

// Start
Async.StartImmediate (ready())
window.Show()
window.WindowState <- FormWindowState.Maximized

//let mutable lst = Array.empty

(*
let rec xorr = function
    | (h, lst, m, i) when h ^^^ m = 0 -> Array.item i lst
    | (h, lst, m, i) -> xorr(Array.item i lst, lst, m, i+1)
    | (h, lst, m, i) when i > Array.length lst-> -1
*)

let mutable lst = [|1;2;3|]
let calc_m lst = Array.fold (fun x m -> x ^^^ m) 0 lst
let maxIndex lst = Array.findIndex (fun x -> x = Array.max lst) lst
let m = calc_m lst
if m <> 0 then
    for i = 0 to lst.Length-1 do
        let tmp = lst.[i] ^^^ m
        if tmp < lst.[i] then
            lst.[i] <- tmp
else
    printf "maxindex: %d " (maxIndex lst)
    let maxI = maxIndex lst
    lst.[maxI] <- lst.[maxI]-1
lst

//lst.[xorr(lst.[0], lst, (calc_m lst), 0)] <- 0


//xorr([1;2;4;3], (xorlist [1;2;3;4]))
(*
let rec pik = function
    | l when xorlist(l) <> 0 -> pik (penis l)
    | _ -> 0

List.filter (fun x -> x <> List.max [1;2;3;4]) 

xorlist [1;2;3;4]

4 ^^^ 4

penis (List.sortDescending [1;2;3;4])
*)



















