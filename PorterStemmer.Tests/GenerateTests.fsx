#r "../packages/FSharp.Data.2.0.5/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO

let diffs = seq {
    use sr = new StreamReader(Http.RequestStream("http://snowball.tartarus.org/algorithms/porter/diffs.txt").ResponseStream)
    while not sr.EndOfStream do
        let tokens = sr.ReadLine().Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
        let first = tokens.[0]
        let second = if Array.length tokens <> 2 then "" else tokens.[1]
        yield first, second
}

diffs |> Seq.iter (fun w -> printfn "%s,%s" (fst w) (snd w))