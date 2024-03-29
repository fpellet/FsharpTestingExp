﻿open System
open System.Threading.Tasks
open Expecto

let tests =
  testList "A test group" [
    test "one test" {
      Expect.equal (2+2) 4 "2+2"
    }

    test "another test that fails" {
      Expect.equal (3+3) 9 "3+3"
    }

    test "External test" {
      Expect.equal (3+2) (sample2.Say.getValue2()) "3+3"
    }

    testAsync "this is an async test" {
      let! x = async { return 4 }
      Expect.equal x (2+2) "2+2"
    }

    testTask "this is a task test" {
      let! n = Task.FromResult 2
      Expect.equal n 2 "n=2"
    }
    
    Sample1.tests
    Sample2.tests
    Sample3.tests
    Sample4.tests
    Sample5.tests
    Sample6.tests
  ]
  |> testLabel "samples"

[<EntryPoint>]
let main argv =
    printfn "Start test"
    tests
    |> runTestsWithCLIArgs [] [||]
