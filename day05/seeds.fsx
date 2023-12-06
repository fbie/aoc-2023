open System

module Util =
    let parseInt s =
        try Int32.Parse s with _ -> failwith $"Failed to parse {s} as an int"

    let inline flip f a b = f b a

    let inline split (c : char) (s : string) = s.Split (c, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

module Mapping =
    type t =
        { destination: int;
          source : int;
          range : int }

    open Util

    let fromString (s : string) : t =
        match split ' ' s with
            | [| dest; src; rng |] ->
                { destination = parseInt dest;
                  source = parseInt src;
                  range = parseInt rng }
            | _ ->
                failwith "Malformed mapping"

    let iter f (m : t) : unit =
        for i in 0 .. m.range do
            f (m.destination + i) (m.source + i)

module ReverseMap =
    type t = int array

    let make (mappings : Mapping.t list) : t =
        let size = List.fold (fun n (m : Mapping.t) -> max n (m.destination + m.range)) 0 mappings
        let mapping = Array.init (size + 1) id
        let set i x = try mapping[i] <- x with _ -> failwith $"{i} >= |{Array.length mapping}|"
        List.iter (Mapping.iter set) mappings
        mapping

module Almanac =
    open System.IO
    open Util

    let parse file =
        let lines = File.ReadLines file
        let parseLine (initialSeeds, currentMap, reverseMaps) (line : string) =
            if Set.isEmpty initialSeeds then // Must be the first line.
                let initialSeeds =
                    split ':' line
                    |> flip Array.get 1
                    |> split ' '
                    |> Array.fold (fun seeds s -> Set.add (parseInt s) seeds) initialSeeds
                in
                initialSeeds, currentMap, reverseMaps
            else if System.Char.IsDigit (line.AsSpan()[0]) then
                let add currentMap = initialSeeds, Some (Mapping.fromString line :: currentMap), reverseMaps
                match currentMap with
                    | Some currentMap -> add currentMap
                    | _ -> add []
            else
                match currentMap with
                    | Some currentMap ->
                        initialSeeds, None, ReverseMap.make currentMap :: reverseMaps
                    | _ ->
                        initialSeeds, currentMap, reverseMaps
            in
            let parseResult =
                lines
                |> Seq.filter (String.IsNullOrEmpty >> not)
                |> Seq.fold parseLine (Set.empty, None, [])
            in
            match parseResult with
                | initialSeeds, Some currentMap, reverseMaps ->
                    initialSeeds, ReverseMap.make currentMap :: reverseMaps
                | initialSeeds, _, reverseMaps ->
                    initialSeeds, reverseMaps

    let find (initialSeeds : int Set) (reverseMaps : ReverseMap.t list) =
        let mutable result = None
        let mutable i = 0
        let tries = (List.head reverseMaps).Length
        let get i (rm : ReverseMap.t) =
            let j = if i >= Array.length rm then i else rm[i]
            j
        while result = None && i < tries do
            let j = List.fold get i reverseMaps
            if Set.contains j initialSeeds then
                result <- Some i
            else
                i <- i + 1
        result

let initialSeeds, reverseMaps = Almanac.parse "test-input.txt"
match Almanac.find initialSeeds reverseMaps with
    | Some i -> printfn "Result: %d" i
    | None -> printfn "No result :("
