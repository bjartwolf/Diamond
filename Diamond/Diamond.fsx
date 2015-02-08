open System
let radius = 10

let axis = [|-radius .. radius|]
type Point = { x : int; y: int;}
type PointWithLetter = { p: Point; c: Char}

let TaxicabNorm (x, y) =  
    abs x + abs y

let indexedLetters =
    ['A' .. 'Z'] |> Seq.mapi (fun i l -> i,l) 

let findLetter (p: Point) =
    let _,l = Seq.find(fun (i, l) -> i = abs p.x) indexedLetters 
    l

let R2 vector = 
    seq { for x in vector do for y in vector do yield x,y }

let diamond = (R2 axis) 
            |> Seq.filter (fun x -> TaxicabNorm x = radius) 
            |> Seq.map( fun (x, y) -> { p={x=x;y=y}; c=findLetter {x=x;y=y} })

// Everything below is just for drawing the diamond
let whiteLines n = 
    [| for i in axis -> (Array.map (fun _ -> '.') axis) |]

let drawing = whiteLines radius

let findIndexInAxis elem axis =  Array.findIndex (fun x -> x = elem) axis

for pointWithChar in diamond do
    let yindex = findIndexInAxis pointWithChar.p.y axis
    let xindex = findIndexInAxis pointWithChar.p.x axis
    drawing.[xindex].[yindex] <- pointWithChar.c

let printArray (arr: char array) = new string (arr) |> sprintf "%A" |> Console.WriteLine 

for line in drawing do
    printArray line