open System
open System.Threading.Tasks
open Expecto

let tests =
  testList "A test group" [
    test "one test" {
      Expect.equal (2+2) 4 "2+2"
    }

    test "another test that fails" {
      Expect.equal (3+3) 4 "3+3"
    }

    testAsync "this is an async test" {
      let! x = async { return 4 }
      Expect.equal x (2+2) "2+2"
    }

    testTask "this is a task test" {
      let! n = Task.FromResult 2
      Expect.equal n 2 "n=2"
    }
  ]
  |> testLabel "samples"

[<EntryPoint>]
let main argv =
    printfn "Start test"
    tests
    |> runTestsWithCLIArgs [] [||]
