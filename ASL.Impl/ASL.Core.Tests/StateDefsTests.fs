namespace ASL.Core.Tests

open ASL.Core
open Newtonsoft.Json.Linq
open NUnit.Framework

[<TestFixture>]
type StateDefsTests() =

    [<Test>]
    member public this.FailStateValidate() =
        let state = new FailState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Error <- "SomeError"
        state.Validate() |> Assert.False
        state.Cause <- "SomeCause"
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.FailStateNameValidation(name: string, validationResult: bool) =
        let state = new FailState(Name = name, Error = "SomeError", Cause = "SomeCause")
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.SucceedStateValidate() =
        let state = new SucceedState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.SucceedStateNameValidation(name: string, validationResult: bool) =
        let state = new SucceedState(Name = name)
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.ChoiceStateValidate() =
        let state = new ChoiceState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Choices <- [|{Choice.Rule = {Condition.Variable = "SomeVar"; Condition.Check = ConditionCheck.IsPresent true} |> BoolExpr.TrueExpr; Choice.Next = "NextState"}|]
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True
        state.Default <- "SomeDefault"
        state.Validate() |> Assert.True

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.ChoiceStateNameValidation(name: string, validationResult: bool) =
        let choices = [|{Choice.Rule = {Condition.Variable = "SomeVar"; Condition.Check = ConditionCheck.IsPresent true} |> BoolExpr.TrueExpr; Choice.Next = "NextState"}|]
        let state = new ChoiceState(Name = name, Choices = choices)
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.WaitStateValidate() =
        let state = new WaitState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Continuation <- Continuation.End
        state.Validate() |> Assert.False
        state.WaitParameters <- WaitParameters.Seconds 10
        state.Validate() |> Assert.True
        state.Continuation <- Continuation.Next "NextState"
        state.Validate() |> Assert.True
        state.WaitParameters <- WaitParameters.SecondsPath "SomePath"
        state.Validate() |> Assert.True
        state.WaitParameters <- (System.DateTime.Now.AddMinutes(1.0) |> WaitParameters.Timestamp)
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.WaitStateNameValidation(name: string, validationResult: bool) =
        let state = new WaitState(Name = name, Continuation = Continuation.End, WaitParameters = WaitParameters.Seconds 10)
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.PassStateValidate() =
        let state = new PassState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Continuation <- Continuation.End
        state.Validate() |> Assert.True
        state.Continuation <- Continuation.Next "NextState"
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True
        state.ResultPath <- "SomeResultPath"
        state.Validate() |> Assert.True
        state.Parameters <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.Result <- new JObject()
        state.Validate() |> Assert.True

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.PassStateNameValidation(name: string, validationResult: bool) =
        let state = new PassState(Name = name, Continuation = Continuation.End)
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.TaskStateValidate() =
        let state = new TaskState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Continuation <- Continuation.End
        state.Validate() |> Assert.False
        state.Resource <- "SomeResource"
        state.Validate() |> Assert.True
        state.Continuation <- Continuation.Next "NextState"
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True
        state.ResultPath <- "SomeResultPath"
        state.Validate() |> Assert.True
        state.Parameters <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.ResultSelector <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.Retry <- [|{Retrier.ErrorEquals = [|"SomeError"|]; Retrier.IntervalSeconds = 0; Retrier.MaxAttempts = 1; Retrier.BackoffRate = 1.0}|]
        state.Validate() |> Assert.True
        state.Catch <- [|{Catcher.ErrorEquals = [|"SomeError"|]; Catcher.Next = "NextState"; Catcher.ResultPath = "SomePath"}|]
        state.Validate() |> Assert.True
        // unsupported properties
        state.TimeoutParameters <- TimeoutParameters.TimeoutSeconds 10
        state.Validate() |> Assert.False
        state.TimeoutParameters <- TimeoutParameters.TimeoutSecondsPath "SomePath"
        state.Validate() |> Assert.False
        state.TimeoutParameters <- TimeoutParameters.None
        state.HeartbeatParameters <- HeartbeatParameters.HeartbeatSeconds 10
        state.Validate() |> Assert.False
        state.HeartbeatParameters <- HeartbeatParameters.HeartbeatSecondsPath "SomePath"
        state.Validate() |> Assert.False

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.TaskStateNameValidation(name: string, validationResult: bool) =
        let state = new TaskState(Name = name, Continuation = Continuation.End, Resource = "SomeResource")
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.ParallelStateValidate() =
        let state = new ParallelState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Continuation <- Continuation.End
        state.Validate() |> Assert.False
        state.Branches <- [|new StateMachine(StartAt = "SomeStart", States = [|new SucceedState(Name = "SomeStart")|])|]
        state.Validate() |> Assert.True
        state.Continuation <- Continuation.Next "NextState"
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True
        state.ResultPath <- "SomeResultPath"
        state.Validate() |> Assert.True
        state.Parameters <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.ResultSelector <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.Retry <- [|{Retrier.ErrorEquals = [|"SomeError"|]; Retrier.IntervalSeconds = 0; Retrier.MaxAttempts = 1; Retrier.BackoffRate = 1.0}|]
        state.Validate() |> Assert.True
        state.Catch <- [|{Catcher.ErrorEquals = [|"SomeError"|]; Catcher.Next = "NextState"; Catcher.ResultPath = "SomePath"}|]
        state.Validate() |> Assert.True

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.ParallelStateNameValidation(name: string, validationResult: bool) =
        let branches = [|new StateMachine(StartAt = "SomeStart", States = [|new SucceedState(Name = "SomeStart")|])|]
        let state = new ParallelState(Name = name, Continuation = Continuation.End, Branches = branches)
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.MapStateValidate() =
        let state = new MapState()
        state.Validate() |> Assert.False
        state.Name <- "SomeName"
        state.Validate() |> Assert.False
        state.Continuation <- Continuation.End
        state.Validate() |> Assert.False
        state.Iterator <- new StateMachine(StartAt = "SomeStart", States = [|new SucceedState(Name = "SomeStart")|])
        state.Validate() |> Assert.True
        state.ItemProcessor <- new StateMachine(StartAt = "SomeStart", States = [|new SucceedState(Name = "SomeStart")|])
        state.Validate() |> Assert.False
        state.Iterator <- null
        state.Validate() |> Assert.True
        state.Continuation <- Continuation.Next "NextState"
        state.Validate() |> Assert.True
        state.Comment <- "SomeComment"
        state.Validate() |> Assert.True
        state.InputPath <- "SomeInputPath"
        state.Validate() |> Assert.True
        state.OutputPath <- "SomeOutputPath"
        state.Validate() |> Assert.True
        state.ResultPath <- "SomeResultPath"
        state.Validate() |> Assert.True
        state.Parameters <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.ResultSelector <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.True
        state.Retry <- [|{Retrier.ErrorEquals = [|"SomeError"|]; Retrier.IntervalSeconds = 0; Retrier.MaxAttempts = 1; Retrier.BackoffRate = 1.0}|]
        state.Validate() |> Assert.True
        state.Catch <- [|{Catcher.ErrorEquals = [|"SomeError"|]; Catcher.Next = "NextState"; Catcher.ResultPath = "SomePath"}|]
        state.Validate() |> Assert.True
        state.ItemsPath <- "SomeItemPath"
        state.Validate() |> Assert.True
        state.ItemSelector <- [|{PayloadTemplate.Name="SomeName"; PayloadTemplate.Value="SomeValue"}|]
        state.Validate() |> Assert.False
        state.Parameters <- Array.zeroCreate 0
        state.Validate() |> Assert.True
        // unsupported properties
        state.ItemReader <- new JObject()
        state.Validate() |> Assert.False
        state.ItemReader <- null
        state.ItemBatcher <- new JObject()
        state.Validate() |> Assert.False
        state.ItemBatcher <- null
        state.ResultWriter <- new JObject()
        state.Validate() |> Assert.False
        state.ResultWriter <- null
        state.MaxConcurrency <- 1
        state.Validate() |> Assert.False
        state.MaxConcurrency <- 0
        state.ToleratedFailurePercentage <- 1
        state.Validate() |> Assert.False
        state.ToleratedFailurePercentage <- 0
        state.ToleratedFailureCount <- 1
        state.Validate() |> Assert.False
        state.ToleratedFailureCount <- 0

    [<TestCase("iddqd", true)>]
    [<TestCase("Iddqd", true)>]
    [<TestCase("IDDQD", true)>]
    [<TestCase("IDDQD666", true)>]
    [<TestCase("666IDDQD666", true)>]
    [<TestCase("666", true)>]
    [<TestCase("IDDQD_666", true)>]
    [<TestCase("IDDQD-666", true)>]
    [<TestCase("_IDDQD-666_", true)>]
    [<TestCase("-IDDQD_666-", true)>]
    [<TestCase("", false)>]
    [<TestCase(" ", false)>]
    [<TestCase("\t", false)>]
    [<TestCase("IDDQD.666", false)>]
    [<TestCase("IDDQD/666", false)>]
    [<TestCase("IDDQD\\666", false)>]
    [<TestCase("IDDQD\"666", false)>]
    [<TestCase("IDDQD'666", false)>]
    [<TestCase("IDDQD|666", false)>]
    member public this.MapStateNameValidation(name: string, validationResult: bool) =
        let itemProcessor = new StateMachine(StartAt = "SomeStart", States = [|new SucceedState(Name = "SomeStart")|])
        let state = new MapState(Name = name, Continuation = Continuation.End, ItemProcessor = itemProcessor)
        Assert.AreEqual(validationResult, state.Validate())

    [<Test>]
    member public this.StateMachineValidate() =
        let stateMachine = new StateMachine()
        stateMachine.Validate() |> Assert.False
        stateMachine.StartAt <- "SomeStart"
        stateMachine.Validate() |> Assert.False
        stateMachine.States <- [|new SucceedState(Name = "SomeStart")|]
        stateMachine.Validate() |> Assert.True
        stateMachine.Comment <- "SomeComment"
        stateMachine.Validate() |> Assert.True
        stateMachine.Version <- "0.0.1"
        stateMachine.Validate() |> Assert.True
        stateMachine.TimeoutSeconds <- 666
        stateMachine.Validate() |> Assert.True