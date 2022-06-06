namespace ASL.Experiments

open Newtonsoft.Json.Linq
open NUnit.Framework

[<TestFixture>]
type StateMachineParserTests() =

    [<Test>]
    member public this.JsonReadExperiments() =
        let source = "{\"Start\": \"S0\", \"Comment\": \"IDDQD\", \"States\": {\"S0\": {\"Data\": [\"IDDQD\", \"IDKFA\"]}, \"S1\": {\"Id\": \"IdDqD\", \"Value\": 666}}}"
        let root = JToken.Parse(source)
        match root.["Start"] with
        | null -> Assert.Fail()
        | start -> start.ToObject<string>() |> printfn "Start = \"%s\""
        match root.["Comment"] with
        | null -> Assert.Fail()
        | comment -> comment.ToObject<string>() |> printfn "Comment = \"%s\""
        match root.["Comment2"] with
        | null -> ()
        | _ -> Assert.Fail()
        match root.["States"] with
        | null -> Assert.Fail()
        | states ->
            let s0 = states.["S0"]
            if s0 = null then
                Assert.Fail()
            printfn "S0 state:"
            match s0.["Data"] with
            | null -> Assert.Fail()
            | data ->
                System.String.Join(',', data.Children()) |> printfn "data = %s"
            let s1 = states.["S1"]
            if s1 = null then
                Assert.Fail()
            printfn "S1 state:"
            match s1.["Id"] with
            | null -> Assert.Fail()
            | id ->
                id.ToObject<string>() |> printfn "Id = %s"
            match s1.["Value"] with
            | null -> Assert.Fail()
            | value ->
                value.ToObject<int>() |> printfn "value = %d"
