module Sample4

open System
open System.Threading.Tasks
open Expecto

module Test1 =
  let tests =
    testList "A test group1" [
      test "one test" {
        Expect.equal (2+2) 4 "2+2"
      }

      test "another test that fails" {
        Expect.equal (3+3) 6 "3+3"
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
    
module Test2 =
  let tests =
    testList "A test group2" [
      test "one test" {
        Expect.equal (2+2) 4 "2+2"
      }

      test "another test that fails" {
        Expect.equal (3+3) 6 "3+3"
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
    
    
module Test3 =
  let tests =
    testList "A test group3" [
      test "one test" {
        Expect.equal (2+2) 4 "2+2"
      }

      test "another test that fails" {
        Expect.equal (3+3) 6 "3+3"
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
    
    
module Test4 =
  let tests =
    testList "A test group4" [
      test "one test" {
        Expect.equal (2+2) 4 "2+2"
      }

      test "another test that fails" {
        Expect.equal (3+3) 6 "3+3"
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
    
    
module Test5 =
  let tests =
    testList "A test group5" [
      test "one test" {
        Expect.equal (2+2) 4 "2+2"
      }

      test "another test that fails" {
        Expect.equal (3+3) 6 "3+3"
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
    
    
module Test6 =
  let tests =
    testList "A test group6" [
      test "one test" {
        Expect.equal (2+2) 4 "2+2"
      }

      test "another test that fails" {
        Expect.equal (3+3) 6 "3+3"
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
    

let tests = testList "Module4" [Test1.tests;Test2.tests;Test3.tests;Test4.tests;Test5.tests;Test6.tests]