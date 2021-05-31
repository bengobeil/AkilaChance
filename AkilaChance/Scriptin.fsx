#r "nuget: FSharp.UMX"

open FSharp.UMX

[<Measure>]
type evSum

[<Measure>]
type evCount

[<Measure>]
type x = evSum/evCount

[<Measure>]
type percent

[<Measure>]
type dollar

[<Measure>]
type spins

[<Struct>]
type SlotIcon =
  | Special4
  | Special3
  | Special2
  | Special1
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  
  override x.ToString() =
    match x with
    | Special4 -> "4"
    | Special3 -> "3"
    | Special2 -> "2"
    | Special1 -> "1"
    | Ace  -> "A"
    | King  -> "K"
    | Queen -> "Q"
    | Jack -> "J"
    | Ten -> "T"
    
let indicesFor4Rows = [| 0 .. 3 |]
  
type SlotResultColumn =
  private { Rows: SlotIcon array }
  
  member x.First = x.Rows.[0]
  member x.Second = x.Rows.[1]
  member x.Third = x.Rows.[2]
  member x.Fourth = x.Rows.[3]
  
let defaultWheelIcons =
  [|
    Special4
    Special3
    Special2
    Special1
    Ace
    King
    Queen
    Queen
    Jack |]
//  [|
//    Special4
//    Special3
//    Special3
//    Special2
//    Special2
//    Special2
//    Special1
//    Special1
//    Special1
//    Special1
//    Ace
//    Ace
//    Ace
//    Ace
//    Ace
//    King
//    King
//    King
//    King
//    King
//    King
//    Queen
//    Queen
//    Queen
//    Queen
//    Queen
//    Queen
//    Queen
//    Jack
//    Jack
//    Jack
//    Jack
//    Jack
//    Jack
//    Jack
//    Jack
//    Ten
//    Ten
//    Ten
//    Ten
//    Ten
//    Ten
//    Ten
//    Ten
//    Ten
//  |]
type SlotWheel =
  private { Icons: SlotIcon array }
  
  member private x.GetNextIndex index =
    (index + 1) % x.Icons.Length
  
  member x.GetResultColumn index =
    let i2 = x.GetNextIndex index
    let i3 = x.GetNextIndex i2
    let i4 = x.GetNextIndex i3
       
    let icons =
       [| index
          i2
          i3
          i4 |]
      |> Array.map (fun i -> x.Icons.[i])
      
    { Rows = icons }
  
  static member Default =
    { Icons = defaultWheelIcons }
    
type SpinResult =
  private { Columns: SlotResultColumn array }
  
  member x.First = x.Columns.[0]
  member x.Second = x.Columns.[1]
  member x.Third = x.Columns.[2]
  member x.Fourth = x.Columns.[3]
  
  override x.ToString() =
    let rowString rowIndex =
      x.Columns
      |> Seq.map (fun col -> string col.Rows.[rowIndex])
      |> String.concat " | "
      
    indicesFor4Rows
    |> Seq.map rowString
    |> String.concat "\n"
  
/// Always has 6 items
type SlotWheelConfiguration =
  private { Wheels: SlotWheel array }
  
  
  member private x.Get index =
    x.Wheels.[index]
  
  member x.First = x.Wheels.[0]
  member x.Second = x.Wheels.[1]
  member x.Third = x.Wheels.[2]
  member x.Fourth = x.Wheels.[3]
  member x.Fifth = x.Wheels.[4]
  member x.Sixth = x.Wheels.[5]
  
  member x.AllPossibilities (): Lazy<SpinResult seq> =
    
    let toIndexArray length = [| 0 .. (length - 1) |]
    
    let indexArrays =
      x.Wheels
      |> Array.map (fun wheel -> Array.length wheel.Icons |> toIndexArray)
    
    lazy(seq {
      for i1 in indexArrays.[0] do
        let firstResultColumn = x.First.GetResultColumn i1
        
        for i2 in indexArrays.[1] do
          let secondResultColumn = x.Second.GetResultColumn i2
          
          for i3 in indexArrays.[2] do
            let thirdResultColumn = x.Third.GetResultColumn i3
            
            for i4 in indexArrays.[3] do
              let fourthResultColumn = x.Fourth.GetResultColumn i4
              
              for i5 in indexArrays.[4] do
                let fifthResultColumn = x.Fifth.GetResultColumn i5
                
                for i6 in indexArrays.[5] do
                  let sixthResultColumn = x.Sixth.GetResultColumn i6
                  
                  { SpinResult.Columns =
                    [| firstResultColumn
                       secondResultColumn
                       thirdResultColumn
                       fourthResultColumn
                       fifthResultColumn
                       sixthResultColumn |] }
    })
    
  static member Default = { Wheels = Array.init 6 (fun _ -> SlotWheel.Default) }

(*

1|2|3|4|5|6
-----------
A|A|A|A|A|A
T|T|T|T|T|T
Q|Q|Q|Q|Q|Q
Q|Q|Q|Q|Q|Q

*)

type Bet = private { Amount: decimal<dollar> }

type Payout = private { Amount: decimal<dollar> }
type PayoutMultiplier = private { Times: decimal<x> }

type PayoutAnalyzer = PayoutAnalyzer of (SpinResult -> PayoutMultiplier)
type NumberOfSpins = uint64<spins>

type ExpectedValue =
  private { SumOfEv: decimal<evSum>
            Count: int<evCount> }
  
  member private x.CountAsDecimal =
    x.Count
    |> decimal
    |> (*) 1.m<evCount>
    
  member x.Ev: decimal<x> =
    if x.Count = 0<evCount>
      then 1.m<x>
    else
     x.SumOfEv / x.CountAsDecimal
    
  member this.AddEv (ev: decimal<x>) =
    { this with
        Count = this.Count + 1<evCount>
        SumOfEv = this.SumOfEv + (ev * 1.m<evCount>) }
    
  static member Default = { SumOfEv = 0.m<evSum>; Count = 0<evCount> }
    
type GetExpectedValue = SlotWheelConfiguration -> PayoutAnalyzer -> ExpectedValue

let getExpectedValue: GetExpectedValue =
  fun configuration (PayoutAnalyzer getMultiplierForSpinResult) ->
    configuration.AllPossibilities().Value
    |> Seq.map getMultiplierForSpinResult
    |> Seq.fold (fun (ev: ExpectedValue) newMultiplier -> ev.AddEv newMultiplier.Times) ExpectedValue.Default

(*

npercolumn = 50
6 cols
total possibilities
50^6
*)

(*
0.5 1 = 0.75
((.75*2) + 0) / 3

*)
let standardConfigurationAnalyzer = getExpectedValue SlotWheelConfiguration.Default

let evIsConstantIfMultiplierIsConstant multiplier =
  let analyzer = PayoutAnalyzer <| fun _ -> { Times = multiplier }
  let expectedValue = (standardConfigurationAnalyzer analyzer)
  printfn $"number of data results {expectedValue.Count}"
  expectedValue.Ev = multiplier
  
let always1 = PayoutAnalyzer <| fun _ -> { Times = 1.m<x> }

#time "on"
[
  evIsConstantIfMultiplierIsConstant 1.m<x>
]
#time "off"

9. ** 6.