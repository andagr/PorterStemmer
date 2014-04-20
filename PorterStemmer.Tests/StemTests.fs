namespace PorterStemmer.Tests

open NUnit.Framework
open FsUnit
open PorterStemmer.Stemmer
open System.IO

[<TestFixture>]
type ``Given this stemmer`` () =
    member t.OfficialStemmingList:seq<string * string> =
        let lines:seq<string> = Seq.cast (File.ReadLines("..\\..\\OfficialStemmingList.txt"))
        let split (s:string) =
            let sp = s.Split([|','|])
            (sp.[0], sp.[1])
        Seq.map split lines

    [<Test>]
    member t.``Check stemming against official list`` () =
        //Seq.iter (fun p -> printfn "%s -> %s" (fst p) (snd p) ) t.OfficialStemmingList
        Seq.iter (fun p -> (fst p) |> stem |> should equal (snd p)) t.OfficialStemmingList