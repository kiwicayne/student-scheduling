module Student.Core.Utils

open System
open System.Threading

module Probability = 
    /// Return a random int between a min and max inclusive
    /// Note: .Net Random max parameter is EXCLUSIVE, this is INCLUSIVE
    let randomIntGenerator = 
        // Create master seed generator and thread local value
        let seedGenerator = Random()
        
        let localGenerator = 
            new ThreadLocal<Random>(fun _ -> 
            lock seedGenerator (fun _ -> 
                let seed = seedGenerator.Next()
                Random(seed)))
        // Return function that uses thread local random generator
        fun min max -> 
            let inclusiveMax = 
                if max = Int32.MaxValue then Int32.MaxValue
                else max + 1
            localGenerator.Value.Next(min, inclusiveMax)
    
    let rndNext rndInt = rndInt 0 Int32.MaxValue
    let rndMax rndInt max = rndInt 0 max
    let rndMinMax rndInt min max = rndInt min max

module Random = 
    let private rndInt = Probability.randomIntGenerator
    let private rndNext() = Probability.rndNext rndInt
    
    /// Shuffle elements of a list.
    let listShuffle lst = lst |> List.sortBy (fun _ -> rndNext())

module Statistics = 
    /// Calcualte the mean and standard deviation of a list of floats
    let meanAndStdDev (values : float list) = 
        let mean = 
            if values.IsEmpty then 0.0
            else values |> List.average
        
        let stdDev = 
            if mean > 0.0 then 
                let sumDeviation = 
                    values                 
                    |> List.sumBy (fun x -> ((x - mean) * (x - mean) |> float))
                
                let variance = sumDeviation / (float values.Length)
                sqrt variance
            else 0.0
        
        (mean, stdDev)

module List = 
    let private headOrEmpty = 
        function 
        | [] -> []
        | x :: _ -> x
    
    let private tailOrEmpty = 
        function 
        | [] -> []
        | _ :: xs -> xs
    
    /// Distribute the given items across the lists of items by adding them
    /// to the item lists with the least number of items
    /// NOTE: This reorders the lists from smallest to largest, so only use this if order doesn't matter.
    let rec distributeEvenly itemLists items = 
        match items with
        | [] -> itemLists
        | student :: xs -> 
            let sortedGroups = itemLists |> List.sortBy List.length
            let groupWithStudent = student :: (sortedGroups |> headOrEmpty)
            let groupsWithStudent = groupWithStudent :: (sortedGroups |> tailOrEmpty)
            distributeEvenly groupsWithStudent xs
    
    let rec appendToLastList item = 
        function 
        | [] -> [ [ item ] ]
        | [ x ] -> [ item :: x ]
        | x :: xs -> x :: appendToLastList item xs
    
    /// Rebalance the items in lists so that all lists have the same number of items (or as close as possible
    /// if there are not enough items for all lists to have the same size).  The fewest number of changes to 
    /// the lists will be made as possible.
    /// NOTE: This is only effecient when a list is already mostly balanced.  It will also return the final result
    /// sorted from largest to smallest, so only use this if order doesn't matter.
    let rec rebalance lists = 
        match lists |> List.sortByDescending List.length with
        | [] -> []
        | [ x ] -> [ x ]
        | largest :: xs when (List.last xs |> List.length) < (largest |> List.length) - 1 -> 
            largest.Tail :: (xs |> appendToLastList largest.Head) |> rebalance
        | _ -> lists

module Convert =
    /// Active pattern for converting a string to an integer 
    let (|Int|_|) str = 
        match System.Int32.TryParse(str) with
        | (true, int) -> Some(int)
        | _ -> None
