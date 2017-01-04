module GUI

#r @"AsyncEventQueue.dll"
open AsyncEventQueue
open System.Windows.Forms
open System.Drawing
open System

let mutable warned = false
let mutable buttons = Array.empty
let mutable matches = Array.empty    
   
let label = new Label(Text="", Top=300, Left=200, Visible=false)
   
let changeLabel s =
    label.Text <- s
    label.Visible <- true

let addMatches (arr:int array) = 
    Seq.toArray(seq{
        for i in 1..arr.Length do
            for x in 1..arr.[i-1] do
                yield new PictureBox(Image=Image.FromFile("hatteland2.png"), Top=(i*50+75), Left=(350-(x*10)), Width=5)
    } |> Seq.cast<Control>)

let addButtons (arr:int array) (ev:AsyncEventQueue<AsyncEventQueue.Message1>) = 
    Seq.toArray(seq{ 
        for y in 1..arr.Length do
            let btn = new Button(Text="-", Top=(y*50+100), Left=400, Size=Size(20,20), BackColor=Color.LightGreen)
            btn.Click.Add (fun _ -> ev.Post (PlayerTurn (y-1)))
            yield btn
    } |> Seq.cast<Control>)

let mutable easyButton = new Button(Location=Point(50,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="EASY")
let mutable hardButton = new Button(Location=Point(200,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="HARD")
let mutable clearButton = new Button(Location=Point(350,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="CLEAR")
let mutable endTurnButton = new Button(Location=Point(750,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="END TURN", Visible=false)
let mutable ans = new TextBox(Location = Point(125, 10), Text = "http://", Width = 250)
let mutable yousuckLabel = new Label(Text="You're going to lose...", Top=10, Left=200, Width=200, Visible=false)
let mutable combo = new ComboBox(Location=Point(100,35), DataSource=[|"http://www2.compute.dtu.dk/~mire/02257/nim1.game";"http://www2.compute.dtu.dk/~mire/02257/nim2.game";"http://www2.compute.dtu.dk/~mire/02257/nim3.game";"http://www2.compute.dtu.dk/~mire/02257/nim4.game"; "Use url in box above"|], Width=300)
let mutable loser = new PictureBox(Image=Image.FromFile("loser.jpg"), Top=(120), Left=(50), Width=700, Height=700)
let mutable winner = new PictureBox(Image=Image.FromFile("winner.jpg"), Top=(120), Left=(15), Width=700, Height=700)
let mutable window = new Form(Text="Nim", Size=Size(500, 600))
    
let initGUI (ev:AsyncEventQueue<AsyncEventQueue.Message1>) =
    easyButton <- new Button(Location=Point(50,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="EASY")
    hardButton <- new Button(Location=Point(200,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="HARD")
    clearButton <- new Button(Location=Point(350,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="CLEAR")
    endTurnButton <- new Button(Location=Point(750,65),MinimumSize=Size(100,50), MaximumSize=Size(100,50),Text="END TURN", Visible=false)
    yousuckLabel <- new Label(Text="You're going to lose...", Top=10, Left=200, Width=200, Visible=false)
    combo <- new ComboBox(Location=Point(100,35), DataSource=[|"http://www2.compute.dtu.dk/~mire/02257/nim1.game";"http://www2.compute.dtu.dk/~mire/02257/nim2.game";"http://www2.compute.dtu.dk/~mire/02257/nim3.game";"http://www2.compute.dtu.dk/~mire/02257/nim4.game"; "Use url in box above"|], Width=300)
    loser <- new PictureBox(Image=Image.FromFile("loser.jpg"), Top=(120), Left=(50), Width=700, Height=700, Visible=false)
    winner <- new PictureBox(Image=Image.FromFile("winner.jpg"), Top=(120), Left=(50), Width=700, Height=700, Visible=false)
    ans <- new TextBox(Location = Point(125, 10), Text = "http://", Width = 250)
    window.Controls.Add  easyButton
    window.Controls.Add  hardButton
    window.Controls.Add  clearButton
    window.Controls.Add  endTurnButton
    window.Controls.Add  combo
    window.Controls.Add label
    window.Controls.Add  yousuckLabel
    window.Controls.Add ans
    window.Controls.Add loser
    window.Controls.Add winner
    easyButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), false)))
    hardButton.Click.Add (fun _ -> ev.Post (Start (combo.SelectedItem.ToString(), true)))
    clearButton.Click.Add (fun _ -> ev.Post Clear)
    endTurnButton.Click.Add (fun _ -> ev.Post Next)
    

let disable bs = 
    for b in [ easyButton; clearButton; hardButton; endTurnButton] do 
        b.Enabled  <- true
    for (b:Button) in bs do 
        b.Enabled  <- false

let updateBoard arr = 
    Seq.iter(fun y ->  window.Controls.Remove y) matches
    matches <- addMatches arr
    window.Controls.AddRange matches

let toggleCombo b =  combo.Enabled <- b
   
let toggleAns b =  ans.Enabled <- b
   
let toggleEndTurnBtn t =  if t then  endTurnButton.Enabled <- true

let setupBoard ev (arr:(int array))= 
    buttons <- addButtons arr ev
    matches <- addMatches arr
    endTurnButton.Top<- (Array.last buttons).Top + 50
    endTurnButton.Left<- (Array.last buttons).Left - 50
    endTurnButton.Visible <- true
    window.Controls.AddRange matches
    window.Controls.AddRange buttons

let toggleButtons i = 
    for a in 0..buttons.Length-1 do
        if a <> i then
            buttons.[a].Enabled <- false
            buttons.[a].BackColor <- Color.Red

let toggleButtonsAi (arr:int array) = 
    for a in 0..buttons.Length-1 do
        if arr.[a] <> 0 then
            buttons.[a].Enabled <- true
            buttons.[a].BackColor <- Color.LightGreen
        else
            buttons.[a].Enabled <- false
            buttons.[a].BackColor <- Color.Red
    
let checkTease arr optimal = 
    if Array.fold (fun x m -> x ^^^ m) 0 arr <> 0 && not warned && optimal then
            yousuckLabel.Visible <- true
            warned <- true
    else
            yousuckLabel.Visible <- false

let endofgameLabel(s)=
    endTurnButton.Visible <- false
    if s = "win" then winner.Visible <- true
    else loser.Visible <- true

let clearBoard () = 
    Seq.iter(fun y ->  window.Controls.Remove y) buttons
    Seq.iter(fun y ->  window.Controls.Remove y) matches
    label.Visible <- false
    winner.Visible <- false
    loser.Visible <- false
    toggleCombo true
    toggleAns true
    endTurnButton.Visible <- false  