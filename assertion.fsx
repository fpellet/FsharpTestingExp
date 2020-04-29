#r "packages/NFluent/lib/netstandard2.0/NFluent.dll"
#r "packages/Unquote/lib/netstandard2.0/Unquote.dll"

open NFluent
open System
open System.Collections.Generic

module Exp1 =
    type Person = { Name: string }
    
    let contains<'a> (expected: 'a list) (value: 'a[]) =
        Check.That(value :> IEnumerable<'a>).Contains(expected :> IEnumerable<'a>) :> ICheckLink<ICheck<IEnumerable<'a>>>
        
    let isOnlyMadeOf<'a> (expected: 'a list) (value: 'a[]) =
        Check.That(value :> IEnumerable<'a>).IsOnlyMadeOf(expected :> IEnumerable<'a>)
        
    let containsExactly<'a> (expected: 'a list) (value: 'a list) =
        Check.That(value :> IEnumerable<'a>).ContainsExactly(expected :> IEnumerable<'a>)
        
    let isNotEqualTo expected (value: Person) =
        Check.That(value).IsNotEqualTo(box expected)
        
    let notContains (expected: string) (value: string) =
        Check.That(value).Not.Contains(expected) :> ICheckLink<ICheck<string>>
        
    let andStartsWith (expected: string) (value: ICheckLink<ICheck<string>>) =
        value.And.StartsWith(expected)
        
    let andContains (expected: string) (value: ICheckLink<ICheck<string>>) =
        value.And.Contains(expected)

    let hasAValue (value: int option) =
        Check.That(value |> Option.toNullable).HasAValue()
        
    let whichIsPositive (value: INullableOrNumberCheckLink<int>) =
        value.Which.IsStrictlyPositive()
        
    let andIsEqualTo (expected: int) (value: ICheckLink<ICheck<int>>) =
        value.And.IsEqualTo(Nullable expected)

    // It's hard to chain asserts. 
    // In F#, a result should be explicity ignore. 'ignore' adds a lot of noise. 
    // Maybe we can replace 'ignore' by another term, but I have no idea.
    let test () =
        [| 1; 2; 3; 4; 5; 666 |] 
        |> contains [3; 5; 666]
        |> ignore

        [| 1; 2; 3 |] 
        |> isOnlyMadeOf [3; 2; 1]
        |> ignore

        ["Hendrix"; "Paco de Lucia"; "Django Reinhardt"; "Baden Powell"]
        |> containsExactly ["Hendrix"; "Paco de Lucia"; "Django Reinhardt"; "Baden Powell"]
        |> ignore

        let camus = { Name = "Camus" }
        let sartre = { Name = "Sartre" }
        camus
        |> isNotEqualTo sartre
        |> ignore

        "Batman and Robin"
        |> notContains "Joker"
        |> andStartsWith "Bat"
        |> andContains "Robins"
        |> ignore

        Some 1
        |> hasAValue
        |> whichIsPositive
        |> andIsEqualTo 1
        |> ignore
        
module Exp2 =
    type Person = { Name: string }
    
    let contains<'a> (expected: 'a list) (value: 'a[]) =
        Check.That(value :> IEnumerable<'a>).Contains(expected :> IEnumerable<'a>) |> ignore
        
    let isOnlyMadeOf<'a> (expected: 'a list) (value: 'a[]) =
        Check.That(value :> IEnumerable<'a>).IsOnlyMadeOf(expected :> IEnumerable<'a>) |> ignore
        
    let containsExactly<'a> (expected: 'a list) (value: 'a list) =
        Check.That(value :> IEnumerable<'a>).ContainsExactly(expected :> IEnumerable<'a>) |> ignore
        
    let isNotEqualTo expected (value: Person) =
        Check.That(value).IsNotEqualTo(box expected) |> ignore
        
    let notContains (expected: string) (value: string) =
        Check.That(value).Not.Contains(expected) |> ignore
        
    let andStartsWith (expected: string) (value: string) =
        Check.That(value).StartsWith(expected) |> ignore
        
    let andContains (expected: string) (value: string) =
        Check.That(value).Contains(expected) |> ignore

    let hasAValue (value: int option) =
        Check.That(value |> Option.toNullable).HasAValue() |> ignore
        
    let whichIsPositive (value: int option) =
        Check.That(value |> Option.toNullable).HasAValue().Which.IsStrictlyPositive() |> ignore
        
    let andIsEqualTo (expected: int) (value: int option) =
        Check.That(value |> Option.toNullable).HasAValue().Which.IsEqualTo(Nullable expected) |> ignore

    let checkThat value asserts =
        asserts
        |> Seq.iter (fun a -> a value)
        
    let should asserts value = checkThat value asserts
    
    // All asserts return void (not need ignore).
    // Exp2 a : Use checkThat to execute asserts list on the value
    // Exp2 b : Use should to execute asserts list on the value
    let test () =
        [| 1; 2; 3; 4; 5; 666 |] 
        |> contains [3; 5; 666]

        [| 1; 2; 3 |] 
        |> isOnlyMadeOf [3; 2; 1]

        ["Hendrix"; "Paco de Lucia"; "Django Reinhardt"; "Baden Powell"]
        |> containsExactly ["Hendrix"; "Paco de Lucia"; "Django Reinhardt"; "Baden Powell"]

        let camus = { Name = "Camus" }
        let sartre = { Name = "Sartre" }
        camus
        |> isNotEqualTo sartre

        checkThat "Batman and Robin" [
            notContains "Joker"
            andStartsWith "Bat"
            andContains "Robins"
        ]

        checkThat (Some 1) [
            hasAValue
            whichIsPositive
            andIsEqualTo 1
        ]
        
        Some 1
        |> should [
            hasAValue
            whichIsPositive
            andIsEqualTo 1
        ]
        
module Exp3 =
     type CheckThat<'T>(value: 'T) =
         [<CustomOperation("contains")>]
         member x.Contains(comp, func) = comp

         [<CustomOperation("notContains")>]
         member x.NotContains(comp, func) = comp

         [<CustomOperation("andStartsWith")>]
         member x.AndStartsWith(comp, func) = comp
         
         [<CustomOperation("andContains")>]
         member x.AndContains(comp, func) = comp

         member x.Yield(comp) = comp
         member x.For(comp) = comp
 
     let checkThat value = CheckThat value

     // I have no idea how to limit the api depending on the input type
     let test () =
         checkThat [| 1; 2; 3; 4; 5; 666 |] {
            contains [3; 5; 666]
         }

         checkThat "Batman and Robin" {
             notContains "Joker"
             andStartsWith "Bat"
             andContains "Robins"
         }

module Exp4 =
    open Exp1

    type Tester() =
        [<CustomOperation("check")>]
        member x.Check(comp, func) = []

        member x.For(comp) = []
        member x.Yield(comp) = []
 
    let test = Tester()

    // Not need to ignore result, but need bracket
    test {
        check ([| 1; 2; 3; 4; 5; 666 |] |> contains [3; 5; 666])

        check (
            "Batman and Robin"
            |> notContains "Joker"
            |> andStartsWith "Bat"
            |> andContains "Robins"
        )
        
    }

module Exp5 =
    open Exp1
    
    type Tester() =
        member x.Bind(comp, func) = []
        member x.YieldFrom(comp) = []
        member x.Combine(comp, func) = []
        member x.Delay(func) = []
        member x.For(comp) = []
        member x.Yield(comp) = []
        member x.Zero(comp) = []
     
    let test = Tester()
    
    // (!) operator: Not need to ignore result and bracket, but 'yield' or 'do' isn't human friendly
    test {
        yield! [| 1; 2; 3; 4; 5; 666 |] |> contains [3; 5; 666]
    
        do! "Batman and Robin"
            |> notContains "Joker"
            |> andStartsWith "Bat"
            |> andContains "Robins"
    }
          
module Exp6 =
    open Exp1
              
    type Tester() =
        member x.Combine(comp, func) = ()
        member x.Delay(func) = func() |> ignore
        member x.Yield(comp) = ()
        member x.Zero(comp) = ()
               
    let test = Tester()
              
    // Little tricky, use Delay operator. Catch all not ignored values and execute check
    // I don't known if it's usefull on a real project
    let tests =
        test {
            let values = [| 1; 2; 3; 4; 5; 666 |]

            values |> contains [3; 5; 666]
              
            "Batman and Robin"
                |> notContains "Joker"
                |> andStartsWith "Bat"
                |> andContains "Robins"
        }

module Exp7 =
    open Exp1
    open Swensen.Unquote
    open Microsoft.FSharp.Quotations

    let inline test (expr:Expr<#ICheckLink<_>>) =
        expr.Eval()
        |> ignore
        
    let check = test

    // use unquote but improve error message with helper method
    let tests =
        test <@ [| 1; 2; 3; 4; 5; 666 |] |> contains [3; 5; 666] @>

        check <@ "Batman and Robin"
                    |> notContains "Joker"
                    |> andStartsWith "Bat"
                    |> andContains "Robins" @>

module Exp8 =
    type NFluentCheck = Check

    type Check() = class end
        with 
            static member That<'a>(value:'a[]) =
                NFluentCheck.That(value :> IEnumerable<'a>)
            static member That(value: string) =
                NFluentCheck.That(value)
        
    // use dot method. Can use autocompletion, but it's not fsharp style
    // have same problem that exp1, need ignore result
    let tests =
        Check.That([| 1; 2; 3; 4; 5; 666 |])
             .Contains([3; 5; 666]) |> ignore

        Check.That("Batman and Robin")
             .Not.Contains("Joker")
             .And.StartsWith("Bat")
             .And.Contains("Robins") |> ignore

module Exp9 =
    let contains value expected =
        Exp1.contains expected value

    let checkThat value assertion =
        assertion value >> ignore

    let shouldContains expected next =
        next (Exp1.contains expected)

    let withValue asserts value =
        asserts value |> ignore

    let andContains<'a> (asserts: 'a[] -> ICheckLink<ICheck<IEnumerable<'a>>>) (expected: 'a list) next =
        next (fun (value: 'a[]) -> (asserts value).And.Contains(expected :> IEnumerable<'a>) :> ICheckLink<ICheck<IEnumerable<'a>>>) 

    let tests =
        checkThat [| 1; 2; 3; 4; 5; 666 |] contains [3; 5; 666]

        shouldContains [3; 5; 666] withValue [| 1; 2; 3; 4; 5; 666 |]
        shouldContains [3; 5; 666] andContains [1] withValue [| 1; 2; 3; 4; 5; 666 |]

