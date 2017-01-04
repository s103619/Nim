open System 
open System.Net 
open System.Threading 
open System.Windows.Forms 
open System.Drawing 
open System.Text.RegularExpressions

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;

#r @"AsyncEventQueue.dll"
open AsyncEventQueue


// An enumeration of the possible events 
type Message = | Start of string * bool | Next | Clear | Cancel | PlayerTurn of int | Web of string | Error | Cancelled 

// The dialogue automaton 
let ev = AsyncEventQueue()

// The window part
let window = new Form(Text="Nim", Size=Size(500, 600))
let rnd = System.Random()

let easyButton = new Button(Location=Point(50,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="EASY")

let hardButton = new Button(Location=Point(200,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="HARD")

let clearButton = new Button(Location=Point(350,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="CLEAR")

let endTurnButton = new Button(Location=Point(750,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="END TURN")

let cancelButton = new Button(Location=Point(500,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="ABORT")

let label = new Label(Text="", Top=300, Left=200, Visible=false)
let yousuckLabel = new Label(Text="You're going to lose...", Top=10, Left=200, Width=200, Visible=false)

let loser = new PictureBox(Image=Image.FromFile("loser.jpg"), Top=(120), Left=(50), Width=700, Height=700)
let winner = new PictureBox(Image=Image.FromFile("winner.jpg"), Top=(120), Left=(15), Width=700, Height=700)

let combo = new ComboBox(Location=Point(100,35), DataSource=[|"http://www2.compute.dtu.dk/~mire/02257/nim1.game";"http://www2.compute.dtu.dk/~mire/02257/nim2.game";"http://www2.compute.dtu.dk/~mire/02257/nim3.game";"http://www2.compute.dtu.dk/~mire/02257/nim4.game"|], Width=300)

let mutable buttons = Array.empty
let mutable matches = Array.empty
let mutable warned = false
let mutable optimal = true

let addMatches (arr:int array) = 
    Seq.toArray(seq{
        for i in 1..arr.Length do
            for x in 1..arr.[i-1] do
                yield new PictureBox(Image=Image.FromFile("hatteland2.png"), Top=(i*50+75), Left=(350-(x*10)), Width=5)
    } |> Seq.cast<Control>)

let changeLabel s b =
    label.Text <- s
    label.Visible <- b

let btnClick (arr:int array) y = arr.[y] <- arr.[y] - 1

let addButtons (arr:int array) = 
    Seq.toArray(seq{ 
        for y in 1..arr.Length do
            let btn = new Button(Text="-", Top=(y*50+100), Left=400, Size=Size(20,20), BackColor=Color.LightGreen)
            btn.Click.Add (fun _ -> ev.Post (PlayerTurn (y-1)))
            yield btn
    } |> Seq.cast<Control>)

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

let subtract (r, arr:int array) = 
    arr.[r] <- arr.[r] - rnd.Next(1, arr.[r])
    arr

let rec getRandom = function
    | (r, arr:int array) when Array.sum arr = 0 -> arr
    | (r, arr:int array) when arr.[r] = 0 -> getRandom(rnd.Next(0, arr.Length), arr)
    | (r, arr:int array) -> subtract(r, arr)

let disable bs = 
    for b in [easyButton;clearButton;cancelButton;hardButton;endTurnButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

let checkGameState arr = (Array.sum arr) = 0

let updateBoard arr = 
    Seq.iter(fun x -> window.Controls.Remove x) matches
    matches <- addMatches (arr)
    window.Controls.AddRange matches

let rec ready() = 
  async {Seq.iter(fun x -> window.Controls.Remove x) buttons
         Seq.iter(fun x -> window.Controls.Remove x) matches

         winner.Visible <- false
         loser.Visible <- false

         disable [cancelButton;clearButton]
         endTurnButton.Visible <- false
         combo.Enabled <- true
         label.Visible <- false
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
             //let arr = Array.map (fun x -> if x <= 9 && x > 0 then x else 9) [| for x in Regex("\d+").Matches(html) -> int x.Value |]
             let arr = [|1;2;3;4|]
             buttons <- addButtons arr
             matches <- addMatches arr

             endTurnButton.Top<- (Array.last buttons).Top + 50
             endTurnButton.Left<- (Array.last buttons).Left - 50

             window.Controls.AddRange matches
             window.Controls.AddRange buttons

             return! player(arr, false)
         | Cancel  -> ts.Cancel()
                      return! cancelling()
         | _       -> failwith("loading: unexpected message")}

and player(arr, t) =
  async {disable [easyButton;hardButton;clearButton;cancelButton;endTurnButton]
         if t then
            endTurnButton.Enabled <- true
         updateBoard arr

         if checkGameState arr then
            return! finished("lose")

         let! msg = ev.Receive()
         match msg with
         | PlayerTurn(i) -> return! turn(arr, i)
         | Next -> return! ai(arr)
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}

and turn(arr, i) =
  async {disable [easyButton;hardButton;clearButton;cancelButton]
         
         for a in 0..buttons.Length-1 do
            if a <> i then
                buttons.[a].Enabled <- false
                buttons.[a].BackColor <- Color.Red
         if arr.[i] > 0 then
            arr.[i] <- arr.[i] - 1

         if checkGameState arr then
            updateBoard arr
            return! finished("win")

         return! player(arr, true)}


and ai(arr) =
  async {disable [easyButton;hardButton;clearButton;cancelButton;endTurnButton]
         if Array.fold (fun x m -> x ^^^ m) 0 arr <> 0 && not warned && optimal then
            yousuckLabel.Visible <- true
            warned <- false

         updateBoard arr

         let newArr = if optimal then getOptimal arr else getRandom(rnd.Next(0, arr.Length), arr)

         for a in 0..buttons.Length-1 do
            if newArr.[a] <> 0 then
                buttons.[a].Enabled <- true
                buttons.[a].BackColor <- Color.LightGreen
            else
                buttons.[a].Enabled <- false
                buttons.[a].BackColor <- Color.Red
                

         return! player(newArr, false)}

and cancelling() =
  async {disable [easyButton;hardButton;clearButton;cancelButton;endTurnButton]
         let! msg = ev.Receive()
         match msg with
         | Cancelled | Error | Web  _ ->
                   return! finished("Cancelled")
         | _    ->  failwith("cancelling: unexpected message")}

and finished(s) =
  async {disable [easyButton;hardButton;cancelButton;endTurnButton]
         endTurnButton.Visible <- false
         if s = "win" then
            winner.Visible <- true
            //changeLabel "You won. Good for you!" true
         else
            loser.Visible <- true
            //changeLabel "YOU LOSE, SUCKER!" true
         
         let! msg = ev.Receive()
         match msg with
         | Clear -> return! ready()
         | _     ->  failwith("finished: unexpected message")}

// Initialization
window.Controls.Add easyButton
window.Controls.Add hardButton
window.Controls.Add clearButton
window.Controls.Add endTurnButton
window.Controls.Add combo
window.Controls.Add label
window.Controls.Add winner
window.Controls.Add loser

easyButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), false)))
hardButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), true)))
clearButton.Click.Add (fun _ -> ev.Post Clear)
endTurnButton.Click.Add (fun _ -> ev.Post Next)

// Start
Async.StartImmediate (ready())
window.Show()
//window.WindowState <- FormWindowState.Maximized

















