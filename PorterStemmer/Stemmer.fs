namespace PorterStemmer

module Stemmer =
    type private Kind =
        | V
        | C

    let private (|BaseV|_|) char =
        match char with
        | 'a' | 'e' | 'i' | 'o' | 'u' -> Some BaseV
        | _ -> None

    let private kinds (word:string) =
        let rec kinds chars =
            match chars with
            | [] -> []
            | ['y'] -> [C]
            | 'y'::t -> (kinds t |> List.head |> (function | V -> C | C -> V))::kinds t
            | BaseV::t -> V::kinds t
            | _::t -> C::kinds t
        word |> Seq.toList |> List.rev |> kinds |> List.rev

    let private pack kinds =
        kinds
        |> List.fold
            (fun kl k -> 
                match k::kl with 
                | [] -> [k]
                | V::V::_ | C::C::_ -> kl
                | _ -> k::kl) []
        |> List.rev

    let rec private measurement kinds =
        match kinds with
        | [] -> 0
        | V::C::t -> (measurement t) + 1
        | h::t -> measurement t

    /// The stem ends with e.g. "s" or any letter/word. (*S in Porter algorithm description.)
    let private (|Ends|_|) (s:string list) (word:string) =
        match List.tryFind (fun s -> word.EndsWith(s)) s with
        | Some s -> Some ((word.Substring(0, String.length word - String.length s)), s)
        | None -> None

    let private ends s trunk =
        match trunk with
        | Ends s _ -> true
        | _ -> false

    /// The stem ends with a double -and equal- consonant. (*d in Porter algorithm description.)
    let private (|EndsDoubleC|_|) trunk =
        match trunk |> kinds |> List.rev with
        | C::C::_ when trunk.[String.length trunk - 2] = trunk.[String.length trunk - 1] -> Some ((trunk.Substring(0, String.length trunk - 2)), (trunk.Substring(String.length trunk - 2, 2)))
        | _ -> None

    /// Calculates the measurement of a stem. (m in Porter algorithm description.)
    let private m  =
        kinds >> pack >> measurement

    /// The stem contains a vowel. (*v* in Porter algorithm description.)
    let private hasVowel trunk =
        trunk |> kinds |> List.exists (fun k -> k = V)

    /// The word ends in CVC, where the second C (i.e. the last character) is not w, x or y. (*o in Porter algorithm description.)
    let private (|EndsCVCNotWXY|_|) word =
        match word with
        | Ends ["w"; "x"; "y"] _ -> None
        | t ->
            match t |> kinds |> List.rev with
            | C::V::C::_ -> Some t
            | _ -> None

    let private notEndsCVCNotWXY trunk =
        match trunk with
        | EndsCVCNotWXY _ -> false
        | _ -> true
    
    let private step1a w =
        match w with
        | Ends ["sses"; "ss"] (t, s) -> t + "ss"
        | Ends ["ies"] (t, s) -> t + "i"
        | Ends ["s"] (t, s) -> t
        | _ -> w

    let private step1b w =
        let step1bX w =
            match w with
            | Ends ["at"] (t, s) -> t + "ate"
            | Ends ["bl"] (t, s) -> t + "ble"
            | Ends ["iz"] (t, s) -> t + "ize"
            | EndsDoubleC (t, s) when not (s = "ll" || s = "ss" || s = "zz")  -> t + string (Seq.head s)
            | EndsCVCNotWXY t when m t = 1  -> t + "e"
            | _ -> w
        match w with
        | Ends ["eed"] (t, s) -> if m t > 0 then t + "ee" else t + s
        | Ends ["ed"; "ing"] (t, s) when hasVowel t  -> step1bX t
        | _ -> w

    let private step1c w =
        match w with
        | Ends ["y"] (t, s) when hasVowel t -> t + "i"
        | _ -> w

    let private step2 w =
        match w with
        | Ends ["ational"] (t, s) when m t > 0 -> t + "ate"
        | Ends ["fulness"] (t, s) when m t > 0 -> t + "ful"
        | Ends ["iveness"] (t, s) when m t > 0 -> t + "ive"
        | Ends ["ization"] (t, s) when m t > 0 -> t + "ize"
        | Ends ["ousness"] (t, s) when m t > 0 -> t + "ous"
        | Ends ["biliti"] (t, s) when m t > 0 -> t + "ble"
        | Ends ["tional"] (t, s) when m t > 0 -> t + "tion"
        | Ends ["alism"; "aliti"] (t, s) when m t > 0 -> t + "al"
        | Ends ["ation"] (t, s) when m t > 0 -> t + "ate"
        | Ends ["entli"] (t, s) when m t > 0 -> t + "ent"
        | Ends ["iviti"] (t, s) when m t > 0 -> t + "ive"
        | Ends ["ousli"] (t, s) when m t > 0 -> t + "ous"
        | Ends ["abli"] (t, s) when m t > 0 -> t + "able"
        | Ends ["alli"] (t, s) when m t > 0 -> t + "al"
        | Ends ["anci"] (t, s) when m t > 0 -> t + "ance"
        | Ends ["ator"] (t, s) when m t > 0 -> t + "ate"
        | Ends ["enci"] (t, s) when m t > 0 -> t + "ence"
        | Ends ["izer"] (t, s) when m t > 0 -> t + "ize"
        | Ends ["eli"] (t, s) when m t > 0 -> t + "e"
        | _ -> w

    let private step3 w =
        match w with
        | Ends ["alize"] (t, s) when m t > 0 -> t + "al"
        | Ends ["ative"] (t, s) when m t > 0 -> t
        | Ends ["icate"; "iciti"; "ical"] (t, s) when m t > 0 -> t + "ic"
        | Ends ["ness"; "ful"] (t, s) when m t > 0 -> t
        | _ -> w

    let private step4 w =
        match w with
        | Ends ["al"; "ance"; "ence"; "er"; "ic"; "able"; "ible"; "ant"; "ement"; "ment"; "ent"; "ou"; "ism"; "ate"; "iti"; "ous"; "ive"; "ize"] (t, s) when m t > 1 -> t
        | Ends ["ion"] (t, s) when m t > 1 && ends ["s"; "t"] t -> t
        | _ -> w

    let private step5a w =
        match w with
        | Ends ["e"] (t, s) when m t > 1 -> t
        | Ends ["e"] (t, s) when m t = 1 && notEndsCVCNotWXY t -> t
        | _ -> w

    let private step5b w =
        match w with
        | EndsDoubleC (t, s) when m w > 1 && s = "ll" -> t + string (Seq.head s)
        | _ -> w

    let stem word =
        word |> step1a |> step1b |> step1c |> step2 |> step3 |> step4 |> step5a |> step5b