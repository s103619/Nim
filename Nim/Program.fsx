// Code from Hansen and Rischel: Functional Programming using F#     1       6/12 2012
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

#r @"AsyncEventQueue.dll"
open AsyncEventQueue

// The window part
let window = new Form(Text="Nim", Size=Size(500, 600))
let rnd = System.Random()

//let ansBox = new TextBox(Location=Point(150,150),Size=Size(200,25))

let easyButton = new Button(Location=Point(50,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="EASY")

let hardButton = new Button(Location=Point(200,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="HARD")

let clearButton = new Button(Location=Point(350,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="CLEAR")

let endTurnButton = new Button(Location=Point(750,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="END TURN")

let cancelButton = new Button(Location=Point(500,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="ABORT")

let combo = new ComboBox(Location=Point(100,35), DataSource=[|"http://www2.compute.dtu.dk/~mire/02257/nim1.game";"http://www2.compute.dtu.dk/~mire/02257/nim2.game";"http://www2.compute.dtu.dk/~mire/02257/nim3.game";"http://www2.compute.dtu.dk/~mire/02257/nim4.game"|], Width=300)

let mutable buttons = Array.empty
let mutable matches = Array.empty

let addMatches (arr:int list) = 
    Seq.toArray(seq{
        for i in 1..arr.Length do
            for x in 1..arr.[i-1] do
                yield new PictureBox(Image=Image.FromFile("hatteland2.png"), Top=(i*50+75), Left=(350-(x*10)), Width=5)
    } |> Seq.cast<Control>)

let addButtons (arr:int list) = 
    Seq.toArray(seq{ 
        for y in 1..arr.Length do
            yield new Button(Text="-", Top=(y*50+100), Left=400, Size=Size(20,20), BackColor=Color.Aqua)
    } |> Seq.cast<Control>)

let mutable optimal = true

let getOptimal arr =
    let calc_m arr = Array.fold (fun x m -> x ^^^ m) 0 arr
    let m = calc_m arr
    printfn "m: %d" m
    if m <> 0 then
        let u = Array.findIndex (fun x -> x ^^^ m < x) arr
        arr.[u] <- arr.[u] ^^^ m
    else
        let maxI = Array.findIndex (fun x -> x = Array.max arr) arr
        arr.[maxI] <- arr.[maxI]-1
    arr

(*
let getRandom (arr:int array) = 
    let mutable lort = true
    let h = rnd.Next(arr.Length-1)
    while lort do
        if arr.[h] = 0 then 
            lort <- false
        else 
            h = rnd.Next(arr.Length-1)
    arr.[h]
*)

let subtract (r, arr:int array) = 
    arr.[r] <- arr.[r] - rnd.Next(1, arr.[r])
    arr

let rec getRandom = function
    | (r, arr:int array) when arr.[r] = 0 -> getRandom(rnd.Next(1, arr.Length-1), arr)
    | (r, arr:int array) -> subtract(r, arr)

let disable bs = 
    for b in [easyButton;clearButton;cancelButton;hardButton;endTurnButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

// An enumeration of the possible events 
type Message = | Start of string * bool | Next | Clear | Cancel | Web of string | Error | Cancelled 

//exception UnexpectedMessage

// The dialogue automaton 
let ev = AsyncEventQueue()
let rec ready() = 
  async {Seq.iter(fun x -> window.Controls.Remove x) buttons
         Seq.iter(fun x -> window.Controls.Remove x) matches

         disable [cancelButton]
         endTurnButton.Visible <- false
         combo.Enabled <- true
         let! msg = ev.Receive()
         match msg with
         | Start (url, diff) -> return! loading(url, diff)
         | Clear     -> return! ready()
         | _         -> failwith("ready: unexpected message")}
  
// Sets up the board from chosen url
and loading(url, diff) =
  async {use ts = new CancellationTokenSource()
         combo.Enabled <- false
         optimal <- diff
          // start the load
         Async.StartWithContinuations
             (async { let webCl = new WebClient()
                      let! html = webCl.AsyncDownloadString(Uri url)
                      return html },
              (fun html -> ev.Post (Web html)),
              (fun _ -> ev.Post Error),
              (fun _ -> ev.Post Cancelled),
              ts.Token)

         disable [easyButton; hardButton; clearButton;endTurnButton]
         endTurnButton.Visible <- true
         let! msg = ev.Receive()
         match msg with
         | Web html ->
             //let arr = List.map (fun x -> if x <= 9 && x > 0 then x else 9) [ for x in Regex("\d+").Matches(html) -> int x.Value ]
             let arr = [1;2;3;4]
             buttons <- addButtons arr
             matches <- addMatches arr

             endTurnButton.Top<- (Array.last buttons).Top + 50
             endTurnButton.Left<- (Array.last buttons).Left - 50

             window.Controls.AddRange matches
             window.Controls.AddRange buttons

             //return! finished("splat")
             return! player(List.toArray arr)
         | Cancel  -> ts.Cancel()
                      return! cancelling()
         | _       -> failwith("loading: unexpected message")}

and player(arr) =
  async {disable [easyButton;hardButton;clearButton;cancelButton]
         let! msg = ev.Receive()
         match msg with
         | Next -> return! ai(arr)
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}


and ai(arr) =
  async {disable [easyButton;hardButton;clearButton;cancelButton;endTurnButton]
         Seq.iter(fun x -> window.Controls.Remove x) matches
         matches <- addMatches (Array.toList arr)
         window.Controls.AddRange matches

         printfn "AI TURN:"
//         let arr = getOptimal arr
         let newArr = if optimal then getOptimal arr else getRandom(rnd.Next(1, arr.Length-1), arr)

         for a in newArr do
            printf "%d " a

         return! player(newArr)}

and cancelling() =
  async {disable [easyButton;hardButton;clearButton;cancelButton;endTurnButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}

and finished(s) =
  async {disable [easyButton;hardButton;cancelButton;endTurnButton]
         let! msg = ev.Receive()
         match msg with
         | Clear -> return! ready()
         | _     ->  failwith("finished: unexpected message")}

// Initialization
//window.Controls.Add ansBox
window.Controls.Add easyButton
window.Controls.Add hardButton
window.Controls.Add clearButton
window.Controls.Add cancelButton
window.Controls.Add endTurnButton
window.Controls.Add combo

easyButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), false)))
hardButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), true)))
clearButton.Click.Add (fun _ -> ev.Post Clear)
cancelButton.Click.Add (fun _ -> ev.Post Cancel)
endTurnButton.Click.Add (fun _ -> ev.Post Next)

// Start
Async.StartImmediate (ready())
window.Show()
//window.WindowState <- FormWindowState.Maximized

















