namespace Societies.Core.Tests
open Xunit
open FsUnit.Xunit
open Societies.Core.Utils
open Societies.Core.Utils.Random
open Societies.Core.Utils.Probability

module UtilsTests =        

    module ProbabilityTests =
        [<Fact>]
        let ``Random int generator should return the same number if min and max the same`` () =
            [1..100]
            |> List.iter (fun _ -> randomIntGenerator 0 0 |> should equal 0)

        [<Fact>]
        let ``Random int generator should return min and max when min and max are 1 apart`` () =
            let results = 
                [1..100]
                |> List.map (fun _ -> randomIntGenerator 0 1)
            
            results |> should contain 0
            results |> should contain 1

    module ListTests = 
        open Societies.Core.Utils

        module DistributeEvenly =
            [<Fact>]
            let ``Distribute evenly should return empty list when called with empty list`` () =
                List.distributeEvenly [] [] |> should be Empty

            [<Fact>]
            let ``Distribute evenly should return input list when called with no items to distribute`` () =
                List.distributeEvenly [[1];[2];[3]] [] |> should equal [[1];[2];[3]]

            [<Fact>]
            let ``Distribute evenly should add item to head of first list when all lists the same size`` () =
                List.distributeEvenly [[1];[2];[3]] [4] |> should equal [[4;1];[2];[3]]

            [<Fact>]
            let ``Distribute evenly should add item to second list when first list larger than rest`` () =
                List.distributeEvenly [[4;1];[2];[3]] [5] |> should equal [[5;2];[3];[4;1]]

            [<Fact>]
            let ``Distribute evenly should add item to second list when first and last lists larger than rest`` () =
                List.distributeEvenly [[4;1];[2];[3;6]] [5] |> should equal [[5;2];[4;1];[3;6]]
       
            [<Fact>]
            let ``Distribute evenly should return single list when called with empty list and one item`` () =
                List.distributeEvenly [] [1] |> should equal [[1]]

            [<Fact>]
            let ``Distribute evenly should return single list when called with empty list and multiple items`` () =
                List.distributeEvenly [] [1;2;3] |> should equal [[3;2;1]]

            [<Fact>]
            let ``Distribute evenly should add item to each list when all lists the same size`` () =
                List.distributeEvenly [[1];[2];[3]] [4;5;6] |> should equal [[6;3];[5;2];[4;1]]

        module Rebalance =
            [<Fact>]
            let ``Rebalance should return empty list when called with empty lists`` () =
                List.rebalance [] |> should be Empty

            [<Fact>]
            let ``Rebalance should return input list when called with 1 list`` () =
                List.rebalance [[1]] |> should equal [[1]]

            [<Fact>]
            let ``Rebalance should return input lists when called with equal sized lists`` () =
                List.rebalance [[1];[2]] |> should equal [[1];[2]]

            [<Fact>]
            let ``Rebalance should return input lists when called with lists of lengths no more than 1 apart`` () =
                List.rebalance [[1];[2;3]] |> should equal [[1];[2;3]]

            [<Fact>]
            let ``Rebalance should rebalance lists when called with lists of lengths 1 and 3`` () =
                List.rebalance [[1];[2;3;4]] |> should equal [[3;4];[2;1]]

            [<Fact>]
            let ``Rebalance should rebalance lists when called with lists of lengths 1, 2, 3`` () =
                List.rebalance [[1];[2;3];[4;5;6]] |> should equal [[5;6];[2;3];[4;1]]

            [<Fact>]
            let ``Rebalance should rebalance lists when called with lists of lengths 1, 1, 3, 3`` () =
                List.rebalance [[1];[2];[4;5;6];[7;8;9]] |> should equal [[8;9];[5;6];[4;2];[7;1]]              