open System

module Util =
    let parseInt s =
        try Int64.Parse s with _ -> failwith $"Failed to parse {s} as an int"

    let inline flip f a b = f b a

    let inline split (c : char) (s : string) = s.Split (c, StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)

module Mapping =
    type t =
        { destination: int64;
          source : int64;
          range : int64 }

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
        for i in 0L .. m.range do
            f (m.destination + i) (m.source + i)

    let isInDestinationRange i (m : t) =
        m.destination <= i && i <= m.destination + m.range

    let getSource i (m : t) =
        m.source + (i - m.destination)

module ReverseMap =
    type t = Mapping.t list

    let make (mappings : Mapping.t list) : t =
        List.sortBy (fun (m : Mapping.t) -> m.destination) mappings

    let get (i : int64) (ms : t) =
        match List.tryFind (Mapping.isInDestinationRange i) ms with
            | Some m -> Mapping.getSource i m
            | _ -> i

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

    let find (initialSeeds : int64 Set) (reverseMaps : ReverseMap.t list) =
        let mutable result = None
        let mutable i = 0L
        while result = None do
            let j = List.fold ReverseMap.get i reverseMaps
            if Set.contains j initialSeeds then
                result <- Some i
            else
                i <- i + 1L
        result

let main file =
    eprintfn "Parsing..."
    let initialSeeds, reverseMaps = Almanac.parse file
    eprintfn "Running algorithm..."
    match Almanac.find initialSeeds reverseMaps with
        | Some i -> printfn "Result: %d" i
        | None -> printfn "No result :("

match fsi.CommandLineArgs with
    | [| _; _; file |] -> main file
    | _ -> eprintfn "Usage: dotnet fsi seeds.fsx -- <file>"
