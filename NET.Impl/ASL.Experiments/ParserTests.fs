namespace ASL.Experiments

open Newtonsoft.Json.Linq
open NUnit.Framework
open System

[<TestFixture>]
type StateMachineParserTests() =

    let parser = new StateMachineParser()

    let createParameter (name: string) (value: string) =
        {Parameter.Name = name; Parameter.Value = value}

    let createRetrier (errorEquals: string[])
                      (intervalSeconds: int)
                      (maxAttempts: int)
                      (backoffRate: float) =
        {Retrier.ErrorEquals = errorEquals;
         Retrier.IntervalSeconds = intervalSeconds;
         Retrier.MaxAttempts = maxAttempts;
         Retrier.BackoffRate = backoffRate}

    let createCatcher (errorEquals: string[]) (next: string) (resultPath: string) =
        {Catcher.ErrorEquals = errorEquals;
         Catcher.Next = next;
         Catcher.ResultPath = resultPath}

    let createStateMachine (startAt: string)
                           (comment: string)
                           (version: string)
                           (timeoutSeconds: int)
                           (states: State[]) =
        {StateMachine.StartAt = startAt;
         StateMachine.Comment = comment;
         StateMachine.Version = version;
         StateMachine.TimeoutSeconds = timeoutSeconds;
         StateMachine.States = states}

    let createFailState (name: string)
                        (comment: string)
                        (error: string)
                        (cause: string) =
        {FailState.Name = name;
         FailState.Comment = comment;
         FailState.Error = error;
         FailState.Cause = cause} |> State.Fail

    let createSucceedState (name: string)
                           (comment: string)
                           (inputPath: string)
                           (outputPath: string) =
        {SucceedState.Name = name;
         SucceedState.Comment = comment;
         SucceedState.InputPath = inputPath;
         SucceedState.OutputPath = outputPath} |> State.Succeed

    let createWaitState (name: string)
                        (comment: string)
                        (inputPath: string)
                        (outputPath: string)
                        (continuation: Continuation)
                        (waitParameters: WaitParameters) =
        {WaitState.Name = name;
         WaitState.Comment = comment;
         WaitState.InputPath = inputPath;
         WaitState.OutputPath = outputPath;
         WaitState.Continuation = continuation;
         WaitState.WaitParameters = waitParameters} |> State.Wait

    let createPassState (name: string)
                        (comment: string)
                        (inputPath: string)
                        (outputPath: string)
                        (continuation: Continuation)
                        (resultPath: string)
                        (parameters: Parameter[])
                        (result: JObject) =
        {PassState.Name = name;
         PassState.Comment = comment;
         PassState.InputPath = inputPath;
         PassState.OutputPath = outputPath;
         PassState.Continuation = continuation;
         PassState.ResultPath = resultPath;
         PassState.Parameters = parameters;
         PassState.Result = result} |> State.Pass

    let createTaskState (name: string)
                        (comment: string)
                        (inputPath: string)
                        (outputPath: string)
                        (continuation: Continuation)
                        (resultPath: string)
                        (parameters: Parameter[])
                        (resultSelector: Parameter[])
                        (retry: Retrier[])
                        (catch: Catcher[])
                        (resource: string)
                        (timeoutParameters: TimeoutParameters)
                        (heartbeatParameters: HeartbeatParameters) =
        {TaskState.Name = name;
         TaskState.Comment = comment;
         TaskState.InputPath = inputPath;
         TaskState.OutputPath = outputPath;
         TaskState.Continuation = continuation;
         TaskState.ResultPath = resultPath;
         TaskState.Parameters = parameters;
         TaskState.ResultSelector = resultSelector;
         TaskState.Retry = retry;
         TaskState.Catch = catch;
         TaskState.Resource = resource;
         TaskState.TimeoutParameters = timeoutParameters;
         TaskState.HeartbeatParameters = heartbeatParameters} |> State.Task

    let createSimpleTaskState (name: string)
                              (continuation: Continuation)
                              (resource: string) =
        createTaskState name null null null continuation null null null Array.empty Array.empty resource (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.NotSpecified)

    let createMapState (name: string)
                       (comment: string)
                       (inputPath: string)
                       (outputPath: string)
                       (continuation: Continuation)
                       (resultPath: string)
                       (parameters: Parameter[])
                       (resultSelector: Parameter[])
                       (retry: Retrier[])
                       (catch: Catcher[])
                       (iterator: StateMachine)
                       (itemsPath: string)
                       (maxConcurrency: int) =
        {MapState.Name = name;
         MapState.Comment = comment;
         MapState.InputPath = inputPath;
         MapState.OutputPath = outputPath;
         MapState.Continuation = continuation;
         MapState.ResultPath = resultPath;
         MapState.Parameters = parameters;
         MapState.ResultSelector = resultSelector;
         MapState.Retry = retry;
         MapState.Catch = catch;
         MapState.Iterator = iterator;
         MapState.ItemsPath = itemsPath;
         MapState.MaxConcurrency = maxConcurrency} |> State.Map

    let createParallelState (name: string)
                            (comment: string)
                            (inputPath: string)
                            (outputPath: string)
                            (continuation: Continuation)
                            (resultPath: string)
                            (parameters: Parameter[])
                            (resultSelector: Parameter[])
                            (retry: Retrier[])
                            (catch: Catcher[])
                            (branches: StateMachine[]) =
        {ParallelState.Name = name;
         ParallelState.Comment = comment;
         ParallelState.InputPath = inputPath;
         ParallelState.OutputPath = outputPath;
         ParallelState.Continuation = continuation;
         ParallelState.ResultPath = resultPath;
         ParallelState.Parameters = parameters;
         ParallelState.ResultSelector = resultSelector;
         ParallelState.Retry = retry;
         ParallelState.Catch = catch;
         ParallelState.Branches = branches} |> State.Parallel

    let createNotExpression (condition: Condition) =
        condition |> BoolExpr.TrueExpr |> BoolExpr.NotExpr

    let createAndExpression (condition: Condition[]) =
        condition |> Array.map BoolExpr.TrueExpr |> BoolExpr.AndExpr

    let createOrExpression (condition: Condition[]) =
        condition |> Array.map BoolExpr.TrueExpr |> BoolExpr.OrExpr

    let createSingleChoice (variable: string) (check: ConditionCheck) (next: string) =
        let condition = {Condition.Variable = variable; Condition.Check = check}
        {Choice.Rule = condition |> BoolExpr.TrueExpr; Choice.Next = next}

    let createChoiceState (name: string)
                          (comment: string)
                          (inputPath: string)
                          (outputPath: string)
                          (choices: Choice[])
                          (defaultState: string) =
        {ChoiceState.Name = name;
         ChoiceState.Comment = comment;
         ChoiceState.InputPath = inputPath;
         ChoiceState.OutputPath = outputPath;
         ChoiceState.Choices = choices;
         ChoiceState.Default = defaultState} |> State.Choice

    [<Test>]
    member public this.ParseFailState() =
        let stateS0 = "{\"Type\": \"Fail\", \"Comment\": \"IDDQD\", \"Error\": \"DNMD\", \"Cause\": \"IDKFA\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createFailState "S0" "IDDQD" "DNMD" "IDKFA"|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseSucceedState() =
        let stateS0 = "{\"Type\": \"Succeed\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createSucceedState "S0" "IDDQD" "$.input" "$.output"|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseWaitState() =
        // End, Seconds
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"Seconds\": 13}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" Continuation.End (WaitParameters.Seconds 13)|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, Seconds
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"Seconds\": 13}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") (WaitParameters.Seconds 13)|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, SecondsPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"SecondsPath\": \"$.seconds\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" Continuation.End (WaitParameters.SecondsPath "$.seconds")|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, SecondsPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"SecondsPath\": \"$.seconds\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") (WaitParameters.SecondsPath "$.seconds")|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, Timestamp
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"Timestamp\": \"2022-01-31T01:02:03Z\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" Continuation.End (new DateTime(2022, 01, 31, 01, 02, 03) |> WaitParameters.Timestamp)|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, Timestamp
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"Timestamp\": \"2022-01-31T01:02:03Z\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") (new DateTime(2022, 01, 31, 01, 02, 03) |> WaitParameters.Timestamp)|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, TimestampPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"TimestampPath\": \"$.timestamp\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" Continuation.End (WaitParameters.TimestampPath "$.timestamp")|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, TimestampPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"TimestampPath\": \"$.timestamp\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") (WaitParameters.TimestampPath "$.timestamp")|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParsePassState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let result = "\"Result\": {\"key\": 666}"
        let resultObj = "{\"key\": 666}" |> JObject.Parse
        // End, without Result
        let stateS0 = "{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", " + inputPath+ ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" ([|createParameter "item.$" "$.value"|]) null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Result
        let stateS0 = "{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" ([|createParameter "item.$" "$.value"|]) null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, Result
        let stateS0 = "{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + result + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" ([|createParameter "item.$" "$.value"|]) resultObj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, Result
        let stateS0 = "{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + result + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" ([|createParameter "item.$" "$.value"|]) resultObj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseTaskState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let parametersObj = [|createParameter "item.$" "$.value"|]
        let resultSelector = "\"ResultSelector\": {\"data.$\": \"$.result\"}"
        let resultSelectorObj = [|createParameter "data.$" "$.result"|]
        let retry = "\"Retry\" : [{\"ErrorEquals\": [\"States.TaskFailed\"], \"IntervalSeconds\": 3, \"MaxAttempts\": 2, \"BackoffRate\": 1.5}]"
        let retryObj = [|createRetrier [|"States.TaskFailed"|] 3 2 1.5|]
        let catch = "\"Catch\": [{\"ErrorEquals\": [\"States.ALL\"], \"Next\": \"S0\", \"ResultPath\": \"$.error-info\"}]"
        let catchObj = [|createCatcher [|"States.ALL"|] "S0" "$.error-info"|]
        let resource = "\"Resource\": \"some-rs\""
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, without Retry, without Catch, TimeoutSeconds, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"TimeoutSeconds\": 13}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 13) (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch, TimeoutSeconds, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"TimeoutSeconds\": 13}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 13) (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, without Retry, without Catch, TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"TimeoutSecondsPath\": \"$.timeout\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSecondsPath "$.timeout") (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch, TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"TimeoutSecondsPath\": \"$.timeout\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSecondsPath "$.timeout") (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSeconds
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"HeartbeatSeconds\": 19}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.HeartbeatSeconds 19)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSeconds
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"HeartbeatSeconds\": 19}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.HeartbeatSeconds 19)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"HeartbeatSecondsPath\": \"$.heartbeat\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.HeartbeatSecondsPath "$.heartbeat")
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", \"HeartbeatSecondsPath\": \"$.heartbeat\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.HeartbeatSecondsPath "$.heartbeat")
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, with Retry, with Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", " + retry + ", " + catch + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj retryObj catchObj "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, with Retry, with Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = "{\"Type\": \"Task\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + resource + ", " + retry + ", " + catch + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj retryObj catchObj "some-rs" (TimeoutParameters.TimeoutSeconds 60) (HeartbeatParameters.NotSpecified)
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseMapState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let parametersObj = [|createParameter "item.$" "$.value"|]
        let resultSelector = "\"ResultSelector\": {\"data.$\": \"$.result\"}"
        let resultSelectorObj = [|createParameter "data.$" "$.result"|]
        let retry = "\"Retry\" : [{\"ErrorEquals\": [\"States.TaskFailed\"], \"IntervalSeconds\": 3, \"MaxAttempts\": 2, \"BackoffRate\": 1.5}]"
        let retryObj = [|createRetrier [|"States.TaskFailed"|] 3 2 1.5|]
        let catch = "\"Catch\": [{\"ErrorEquals\": [\"States.ALL\"], \"Next\": \"S0\", \"ResultPath\": \"$.error-info\"}]"
        let catchObj = [|createCatcher [|"States.ALL"|] "S0" "$.error-info"|]
        let mapDetails = "\"ItemsPath\": \"$.Item\", \"MaxConcurrency\": 1, \"Iterator\": {\"StartAt\": \"It0\", \"States\": {\"It0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs\"}}}"
        let iteratorObj = [|createSimpleTaskState "It0" Continuation.End "rs"|] |> createStateMachine "It0" null null -1
        // End, without Retry, without Catch
        let stateS0 = "{\"Type\": \"Map\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + mapDetails + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty iteratorObj "$.Item" 1
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch
        let stateS0 = "{\"Type\": \"Map\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + mapDetails + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty iteratorObj "$.Item" 1
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, wit Retry, with Catch
        let stateS0 = "{\"Type\": \"Map\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + retry + ", " + catch + ", " + mapDetails + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj retryObj catchObj iteratorObj "$.Item" 1
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, with Retry, with Catch
        let stateS0 = "{\"Type\": \"Map\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + retry + ", " + catch + ", " + mapDetails + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj retryObj catchObj iteratorObj "$.Item" 1
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseParallelState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let parametersObj = [|createParameter "item.$" "$.value"|]
        let resultSelector = "\"ResultSelector\": {\"data.$\": \"$.result\"}"
        let resultSelectorObj = [|createParameter "data.$" "$.result"|]
        let retry = "\"Retry\" : [{\"ErrorEquals\": [\"States.TaskFailed\"], \"IntervalSeconds\": 3, \"MaxAttempts\": 2, \"BackoffRate\": 1.5}]"
        let retryObj = [|createRetrier [|"States.TaskFailed"|] 3 2 1.5|]
        let catch = "\"Catch\": [{\"ErrorEquals\": [\"States.ALL\"], \"Next\": \"S0\", \"ResultPath\": \"$.error-info\"}]"
        let catchObj = [|createCatcher [|"States.ALL"|] "S0" "$.error-info"|]
        let branch1 = "{\"StartAt\": \"B10\", \"States\": {\"B10\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs1\"}}}"
        let branch1Obj = [|createSimpleTaskState "B10" Continuation.End "rs1"|] |> createStateMachine "B10" null null -1
        let branch2 = "{\"StartAt\": \"B20\", \"States\": {\"B20\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs2\"}}}"
        let branch2Obj = [|createSimpleTaskState "B20" Continuation.End "rs2"|] |> createStateMachine "B20" null null -1
        let branches = "\"Branches\": [" + branch1 + ", " + branch2 + "]"
        // End, without Retry, without Catch
        let stateS0 = "{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + branches + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, without Retry, without Catch
        let stateS0 = "{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + branches + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // End, with Retry, with Catch
        let stateS0 = "{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"End\": true, " + resultPath + ", " + parameters + ", " + resultSelector + ", " + retry + ", " + catch + ", " + branches + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState "S0" "IDDQD" "$.input" "$.output" Continuation.End "$.data" parametersObj resultSelectorObj retryObj catchObj [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Next, with Retry, with Catch
        let stateS0 = "{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Next\": \"S1\", " + resultPath + ", " + parameters + ", " + resultSelector + ", " + retry + ", " + catch + ", " + branches + "}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState "S0" "IDDQD" "$.input" "$.output" (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj retryObj catchObj [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseDataTestExpressionChoiceState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let timestampSource = "2022-01-31T01:02:03Z"
        let timestamp = new DateTime(2022, 01, 31, 01, 02, 03)
        // Single StringEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringLessThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThan\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringLessThan) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringLessThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringLessThanPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringGreaterThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThan\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringGreaterThan) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringGreaterThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringGreaterThanPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringLessThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanEquals\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringLessThanEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringLessThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringLessThanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringGreaterThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanEquals\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringGreaterThanEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringGreaterThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringGreaterThanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringMatches expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringMatches\": \"IDDQD*\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD*" |> ConditionCheck.StringMatches) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericEquals with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEquals\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericEqualsI) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericEquals with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEquals\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericEqualsF) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericLessThan with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThan\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericLessThanI) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericLessThan with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThan\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericLessThanF) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericLessThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericLessThanPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericGreaterThan with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThan\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericGreaterThanI) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericGreaterThan with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThan\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericGreaterThanF) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericGreaterThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericGreaterThanPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericLessThanEquals with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEquals\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericLessThanEqualsI) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericLessThanEquals with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEquals\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericLessThanEqualsF) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericLessThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericLessThanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericGreaterThanEquals with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEquals\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericGreaterThanEqualsI) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericGreaterThanEquals with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEquals\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericGreaterThanEqualsF) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single NumericGreaterThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericGreaterThanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single BooleanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"BooleanEquals\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.BooleanEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single BooleanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"BooleanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.BooleanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampEquals\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampLessThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThan\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampLessThan) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampLessThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampLessThanPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampGreaterThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThan\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampGreaterThan) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampGreaterThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampGreaterThanPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampLessThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanEquals\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampLessThanEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampLessThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampLessThanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampGreaterThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanEquals\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampGreaterThanEquals) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single TimestampGreaterThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampGreaterThanEqualsPath) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single IsNull expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsNull\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.IsNull) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single IsPresent expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsPresent\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.IsPresent) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single IsNumeric expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsNumeric\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.IsNumeric) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single IsString expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsString\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.IsString) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single IsBoolean expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsBoolean\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.IsBoolean) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single IsTimestamp expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsTimestamp\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" (false |> ConditionCheck.IsTimestamp) "S1"|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringEquals expression, with Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\", \"Next\": \"S1\"}], \"Default\": \"S2\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringEquals) "S1"|] "S2"|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // Single StringEqualsPath expression, with Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEqualsPath\": \"$.value\", \"Next\": \"S1\"}], \"Default\": \"S2\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringEqualsPath) "S1"|] "S2"|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)

    [<Test>]
    member public this.ParseBooleanExpressionChoiceState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let test1Expression = "{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\"}"
        let test1Obj = {Condition.Variable = "$.var"; Condition.Check = "IDDQD" |> ConditionCheck.StringEquals}
        let test2Expression = "{\"Variable\": \"$.var\", \"StringEqualsPath\": \"$.value\"}"
        let test2Obj = {Condition.Variable = "$.var"; Condition.Check = "$.value" |> ConditionCheck.StringEqualsPath}
        let test3Expression = "{\"Variable\": \"$.var\", \"StringLessThan\": \"IDKFA\"}"
        let test3Obj = {Condition.Variable = "$.var"; Condition.Check = "IDKFA" |> ConditionCheck.StringLessThan}
        // (Not E1) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Not\": " + test1Expression + ", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = test1Obj |> createNotExpression; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // (E1 And E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"And\": [" + test1Expression + ", " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|test1Obj; test2Obj|] |> createAndExpression; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // (E1 Or E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Or\": [" + test1Expression + ", " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|test1Obj; test2Obj|] |> createOrExpression; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // (Not (E1 And E2)) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Not\": {\"And\": [" + test1Expression + ", " + test2Expression + "]}, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|test1Obj; test2Obj|] |> createAndExpression |> BoolExpr.NotExpr; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // (Not (E1 Or E2)) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Not\": {\"Or\": [" + test1Expression + ", " + test2Expression + "]}, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|test1Obj; test2Obj|] |> createOrExpression |> BoolExpr.NotExpr; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // ((Not E1) And E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"And\": [{\"Not\": " + test1Expression + "}, " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|test1Obj |> createNotExpression; test2Obj |> BoolExpr.TrueExpr|] |> BoolExpr.AndExpr; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // ((Not E1) Or E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Or\": [{\"Not\": " + test1Expression + "}, " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|test1Obj |> createNotExpression; test2Obj |> BoolExpr.TrueExpr|] |> BoolExpr.OrExpr; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // ((E1 And E2) Or E3) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"Or\": [{\"And\": [" + test1Expression + ", " + test2Expression + "]}, " + test3Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|[|test1Obj; test2Obj|] |> createAndExpression; test3Obj |> BoolExpr.TrueExpr|] |> BoolExpr.OrExpr; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)
        // ((E1 Or E2) And E3) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + inputPath + ", " + outputPath + ", \"Choices\": [{\"And\": [{\"Or\": [" + test1Expression + ", " + test2Expression + "]}, " + test3Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState "S0" "IDDQD" "$.input" "$.output" [|{Choice.Rule = [|[|test1Obj; test2Obj|] |> createOrExpression; test3Obj |> BoolExpr.TrueExpr|] |> BoolExpr.AndExpr; Choice.Next = "S1"}|] null|] |> createStateMachine "S0" null null -1
        Assert.AreEqual(expected, actual)