namespace ASL.Core.Tests

open ASL.Core
open Newtonsoft.Json.Linq
open NUnit.Framework
open System

[<TestFixture>]
type StateMachineParserTests() =

    let parser = new StateMachineParser()

    let createPayloadTemplate (name: string) (value: string) =
        {PayloadTemplate.Name = name; PayloadTemplate.Value = value}

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
                           (states: IState[]) =
        new StateMachine(StartAt = startAt,
                         Comment = comment,
                         Version = version,
                         TimeoutSeconds = timeoutSeconds,
                         States = states)

    let createSimpleStateMachine (startAt: string) (states: IState[]) =
        new StateMachine(StartAt = startAt,
                         Comment = null,
                         Version = null,
                         TimeoutSeconds = 0,
                         States = states)

    let createFailState (name: string)
                        (comment: string)
                        (error: string)
                        (cause: string): IState =
        new FailState(Name = name, Comment = comment, Error = error, Cause = cause)

    let createSucceedState (name: string)
                           (comment: string)
                           (inputPath: string)
                           (outputPath: string): IState =
        new SucceedState(Name = name, Comment = comment, InputPath = inputPath, OutputPath = outputPath)

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
                          (defaultState: string): IState =
        new ChoiceState(Name = name,
                        Comment = comment,
                        InputPath = inputPath,
                        OutputPath = outputPath,
                        Choices = choices,
                        Default = defaultState)

    let createWaitState (name: string)
                        (comment: string)
                        (inputPath: string)
                        (outputPath: string)
                        (continuation: Continuation)
                        (waitParameters: WaitParameters): IState =
        new WaitState(Name = name,
                      Comment = comment,
                      InputPath = inputPath,
                      OutputPath = outputPath,
                      Continuation = continuation,
                      WaitParameters = waitParameters)

    let createPassState (name: string)
                        (comment: string)
                        (inputPath: string)
                        (outputPath: string)
                        (continuation: Continuation)
                        (resultPath: string)
                        (parameters: PayloadTemplate[])
                        (result: JObject): IState =
        new PassState(Name = name,
                      Comment = comment,
                      InputPath = inputPath,
                      OutputPath = outputPath,
                      Continuation = continuation,
                      ResultPath = resultPath,
                      Parameters = parameters,
                      Result = result)

    let createTaskState (name: string)
                        (comment: string)
                        (inputPath: string)
                        (outputPath: string)
                        (continuation: Continuation)
                        (resultPath: string)
                        (parameters: PayloadTemplate[])
                        (resultSelector: PayloadTemplate[])
                        (retry: Retrier[])
                        (catch: Catcher[])
                        (resource: string)
                        (timeoutParameters: TimeoutParameters)
                        (heartbeatParameters: HeartbeatParameters)
                        (credentials: JObject): IState =
        new TaskState(Name = name,
                      Comment = comment,
                      InputPath = inputPath,
                      OutputPath = outputPath,
                      Continuation = continuation,
                      ResultPath = resultPath,
                      Parameters = parameters,
                      ResultSelector = resultSelector,
                      Retry = retry,
                      Catch = catch,
                      Resource = resource,
                      TimeoutParameters = timeoutParameters,
                      HeartbeatParameters = heartbeatParameters,
                      Credentials = credentials)

    let createSimpleTaskState (name: string)
                              (continuation: Continuation)
                              (resource: string): IState =
        new TaskState(Name = name,
                      Comment = null,
                      InputPath = null,
                      OutputPath = null,
                      Continuation = continuation,
                      ResultPath = null,
                      Parameters = Array.zeroCreate 0,
                      ResultSelector = Array.zeroCreate 0,
                      Retry = Array.zeroCreate 0,
                      Catch = Array.zeroCreate 0,
                      Resource = resource,
                      TimeoutParameters = TimeoutParameters.None,
                      HeartbeatParameters = HeartbeatParameters.None,
                      Credentials = null)

    let createParallelState (name: string)
                            (comment: string)
                            (inputPath: string)
                            (outputPath: string)
                            (continuation: Continuation)
                            (resultPath: string)
                            (parameters: PayloadTemplate[])
                            (resultSelector: PayloadTemplate[])
                            (retry: Retrier[])
                            (catch: Catcher[])
                            (branches: StateMachine[]): IState =
        new ParallelState(Name = name,
                          Comment = comment,
                          InputPath = inputPath,
                          OutputPath = outputPath,
                          Continuation = continuation,
                          ResultPath = resultPath,
                          Parameters = parameters,
                          ResultSelector = resultSelector,
                          Retry = retry,
                          Catch = catch,
                          Branches = branches)

    let createMapState (name: string)
                       (comment: string)
                       (inputPath: string)
                       (outputPath: string)
                       (continuation: Continuation)
                       (resultPath: string)
                       (parameters: PayloadTemplate[])
                       (resultSelector: PayloadTemplate[])
                       (retry: Retrier[])
                       (catch: Catcher[])
                       (iterator: StateMachine)
                       (itemProcessor: StateMachine)
                       (itemsPath: string)
                       (itemReader: JObject)
                       (itemSelector: PayloadTemplate[])
                       (itemBatcher: JObject)
                       (resultWriter: JObject)
                       (maxConcurrency: int)
                       (toleratedFailurePercentage)
                       (toleratedFailureCount): IState =
        new MapState(Name = name,
                     Comment = comment,
                     InputPath = inputPath,
                     OutputPath = outputPath,
                     Continuation = continuation,
                     ResultPath = resultPath,
                     Parameters = parameters,
                     ResultSelector = resultSelector,
                     Retry = retry,
                     Catch = catch,
                     Iterator = iterator,
                     ItemProcessor = itemProcessor,
                     ItemsPath = itemsPath,
                     ItemReader = itemReader,
                     ItemSelector = itemSelector,
                     ItemBatcher = itemBatcher,
                     ResultWriter = resultWriter,
                     MaxConcurrency = maxConcurrency,
                     ToleratedFailurePercentage = toleratedFailurePercentage,
                     ToleratedFailureCount = toleratedFailureCount)

    let createOldMapState (name: string)
                          (comment: string)
                          (inputPath: string)
                          (outputPath: string)
                          (continuation: Continuation)
                          (resultPath: string)
                          (parameters: PayloadTemplate[])
                          (resultSelector: PayloadTemplate[])
                          (retry: Retrier[])
                          (catch: Catcher[])
                          (iterator: StateMachine)
                          (itemsPath: string)
                          (maxConcurrency: int): IState =
        new MapState(Name = name,
                     Comment = comment,
                     InputPath = inputPath,
                     OutputPath = outputPath,
                     Continuation = continuation,
                     ResultPath = resultPath,
                     Parameters = parameters,
                     ResultSelector = resultSelector,
                     Retry = retry,
                     Catch = catch,
                     Iterator = iterator,
                     ItemProcessor = null,
                     ItemsPath = itemsPath,
                     ItemReader = null,
                     ItemSelector = Array.zeroCreate 0,
                     ItemBatcher = null,
                     ResultWriter = null,
                     MaxConcurrency = maxConcurrency,
                     ToleratedFailurePercentage = 0,
                     ToleratedFailureCount = 0)

    member private this.CheckFailState(expected: FailState, actual: FailState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.Error, actual.Error)
        Assert.AreEqual(expected.Cause, actual.Cause)

    member private this.CheckSucceedState(expected: SucceedState, actual: SucceedState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)

    member private this.CheckChoiceState(expected: ChoiceState, actual: ChoiceState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)
        Assert.AreEqual(expected.Choices, actual.Choices)
        Assert.AreEqual(expected.Default, actual.Default)

    member private this.CheckWaitState(expected: WaitState, actual: WaitState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)
        Assert.AreEqual(expected.Continuation, actual.Continuation)
        Assert.AreEqual(expected.WaitParameters, actual.WaitParameters)

    member private this.CheckPassState(expected: PassState, actual: PassState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)
        Assert.AreEqual(expected.Continuation, actual.Continuation)
        Assert.AreEqual(expected.ResultPath, actual.ResultPath)
        Assert.AreEqual(expected.Parameters, actual.Parameters)
        Assert.AreEqual(expected.Result, actual.Result)

    member private this.CheckTaskState(expected: TaskState, actual: TaskState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)
        Assert.AreEqual(expected.Continuation, actual.Continuation)
        Assert.AreEqual(expected.ResultPath, actual.ResultPath)
        Assert.AreEqual(expected.Parameters, actual.Parameters)
        Assert.AreEqual(expected.ResultSelector, actual.ResultSelector)
        Assert.AreEqual(expected.Retry, actual.Retry)
        Assert.AreEqual(expected.Catch, actual.Catch)
        Assert.AreEqual(expected.Resource, actual.Resource)
        Assert.AreEqual(expected.TimeoutParameters, actual.TimeoutParameters)
        Assert.AreEqual(expected.HeartbeatParameters, actual.HeartbeatParameters)

    member private this.CheckParallelState(expected: ParallelState, actual: ParallelState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)
        Assert.AreEqual(expected.Continuation, actual.Continuation)
        Assert.AreEqual(expected.ResultPath, actual.ResultPath)
        Assert.AreEqual(expected.Parameters, actual.Parameters)
        Assert.AreEqual(expected.ResultSelector, actual.ResultSelector)
        Assert.AreEqual(expected.Retry, actual.Retry)
        Assert.AreEqual(expected.Catch, actual.Catch)
        Assert.AreEqual(expected.Branches.Length, actual.Branches.Length)
        for index in 0 .. expected.Branches.Length - 1 do
            this.CheckStateMachine(expected.Branches[index], actual.Branches[index])

    member private this.CheckMapState(expected: MapState, actual: MapState) =
        Assert.IsNotNull(actual)
        Assert.AreEqual(expected.Name, actual.Name)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.InputPath, actual.InputPath)
        Assert.AreEqual(expected.OutputPath, actual.OutputPath)
        Assert.AreEqual(expected.Continuation, actual.Continuation)
        Assert.AreEqual(expected.ResultPath, actual.ResultPath)
        Assert.AreEqual(expected.Parameters, actual.Parameters)
        Assert.AreEqual(expected.ResultSelector, actual.ResultSelector)
        Assert.AreEqual(expected.Retry, actual.Retry)
        Assert.AreEqual(expected.Catch, actual.Catch)
        Assert.AreEqual(expected.Iterator <> null, actual.Iterator <> null)
        if expected.Iterator <> null then
            this.CheckStateMachine(expected.Iterator,  actual.Iterator)
        Assert.AreEqual(expected.ItemProcessor <> null, actual.ItemProcessor <> null)
        if expected.ItemProcessor <> null then
            this.CheckStateMachine(expected.ItemProcessor,  actual.ItemProcessor)
        Assert.AreEqual(expected.ItemsPath, actual.ItemsPath)
        Assert.AreEqual(expected.ItemReader, actual.ItemReader)
        Assert.AreEqual(expected.ItemSelector, actual.ItemSelector)
        Assert.AreEqual(expected.ItemBatcher, actual.ItemBatcher)
        Assert.AreEqual(expected.ResultWriter, actual.ResultWriter)
        Assert.AreEqual(expected.MaxConcurrency, actual.MaxConcurrency)
        Assert.AreEqual(expected.ToleratedFailurePercentage, actual.ToleratedFailurePercentage)
        Assert.AreEqual(expected.ToleratedFailureCount, actual.ToleratedFailureCount)

    member private this.CheckState(expected: IState, actual: IState) =
        Assert.AreEqual(expected.StateType, actual.StateType)
        match expected.StateType with
        | StateType.Fail -> this.CheckFailState(expected :?> FailState, actual :?> FailState)
        | StateType.Succeed -> this.CheckSucceedState(expected :?> SucceedState, actual :?> SucceedState)
        | StateType.Choice -> this.CheckChoiceState(expected :?> ChoiceState, actual :?> ChoiceState)
        | StateType.Wait -> this.CheckWaitState(expected :?> WaitState, actual :?> WaitState)
        | StateType.Pass -> this.CheckPassState(expected :?> PassState, actual :?> PassState)
        | StateType.Task -> this.CheckTaskState(expected :?> TaskState, actual :?> TaskState)
        | StateType.Parallel -> this.CheckParallelState(expected :?> ParallelState, actual :?> ParallelState)
        | StateType.Map -> this.CheckMapState(expected :?> MapState, actual :?> MapState)
        | _ -> failwith "Unexpected control branch"

    member private this.CheckStateMachine(expected: StateMachine, actual: StateMachine) =
        Assert.AreEqual(expected.StartAt, actual.StartAt)
        Assert.AreEqual(expected.Comment, actual.Comment)
        Assert.AreEqual(expected.Version, actual.Version)
        Assert.AreEqual(expected.TimeoutSeconds, actual.TimeoutSeconds)
        Assert.AreEqual(expected.States.Length, actual.States.Length)
        for index in 0 .. expected.States.Length - 1 do
            this.CheckState(expected.States[index], actual.States[index])

    [<Test>]
    member public this.ParseCommonErrors() =
        (fun() -> "{\"States\": {\"S0\": {\"Type\": \"Succeed\"}}}" |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        (fun() -> "{\"States\": {\"S0\": {\"Comment\": \"IDDQD\"}}}" |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        (fun() -> "{\"StartAt\": \"S0\"}" |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore

    [<Test>]
    member public this.ParseFailState() =
        let stateS0 = "{\"Type\": \"Fail\", \"Comment\": \"IDDQD\", \"Error\": \"DNMD\", \"Cause\": \"IDKFA\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createFailState "S0" "IDDQD" "DNMD" "IDKFA"|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseFailStateWithErrors() =
        // Without Error
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Fail\", \"Cause\": \"IDKFA\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Without Cause
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Fail\", \"Error\": \"DNMD\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore

    [<Test>]
    member public this.ParseSucceedState() =
        let stateS0 = "{\"Type\": \"Succeed\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createSucceedState "S0" "IDDQD" "$.input" "$.output"|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseSucceedStateWithErrors() =
        // Nothing
        ()

    [<Test>]
    member public this.ParseDataTestExpressionChoiceState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let ioPath = $"{inputPath}, {outputPath}"
        let timestampSource = "2022-01-31T01:02:03Z"
        let timestamp = new DateTime(2022, 01, 31, 01, 02, 03)
        let createChoiceState = createChoiceState "S0" "IDDQD" "$.input" "$.output"
        // Single StringEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringLessThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThan\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringLessThan) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringLessThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringLessThanPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringGreaterThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThan\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringGreaterThan) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringGreaterThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringGreaterThanPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringLessThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanEquals\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringLessThanEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringLessThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringLessThanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringGreaterThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanEquals\": \"IDDQD\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringGreaterThanEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringGreaterThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringGreaterThanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringMatches expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringMatches\": \"IDDQD*\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD*" |> ConditionCheck.StringMatches) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericEquals with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEquals\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericEqualsI) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericEquals with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEquals\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericEqualsF) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericLessThan with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThan\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericLessThanI) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericLessThan with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThan\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericLessThanF) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericLessThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericLessThanPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericGreaterThan with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThan\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericGreaterThanI) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericGreaterThan with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThan\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericGreaterThanF) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericGreaterThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericGreaterThanPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericLessThanEquals with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEquals\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericLessThanEqualsI) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericLessThanEquals with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEquals\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericLessThanEqualsF) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericLessThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericLessThanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericGreaterThanEquals with int expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEquals\": 666, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (666 |> ConditionCheck.NumericGreaterThanEqualsI) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericGreaterThanEquals with double expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEquals\": 3.14, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (3.14 |> ConditionCheck.NumericGreaterThanEqualsF) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single NumericGreaterThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.NumericGreaterThanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single BooleanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"BooleanEquals\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.BooleanEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single BooleanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"BooleanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.BooleanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampEquals\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampLessThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThan\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampLessThan) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampLessThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampLessThanPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampGreaterThan expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThan\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampGreaterThan) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampGreaterThanPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampGreaterThanPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampLessThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanEquals\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampLessThanEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampLessThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampLessThanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampGreaterThanEquals expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanEquals\": \"" + timestampSource + "\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (timestamp |> ConditionCheck.TimestampGreaterThanEquals) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single TimestampGreaterThanEqualsPath expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanEqualsPath\": \"$.value\", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.TimestampGreaterThanEqualsPath) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single IsNull expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsNull\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.IsNull) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single IsPresent expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsPresent\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.IsPresent) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single IsNumeric expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsNumeric\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.IsNumeric) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single IsString expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsString\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.IsString) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single IsBoolean expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsBoolean\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.IsBoolean) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single IsTimestamp expression, without Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"IsTimestamp\": false, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" (false |> ConditionCheck.IsTimestamp) "S1"|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringEquals expression, with Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\", \"Next\": \"S1\"}], \"Default\": \"S2\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("IDDQD" |> ConditionCheck.StringEquals) "S1"|] "S2"|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Single StringEqualsPath expression, with Default
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Variable\": \"$.var\", \"StringEqualsPath\": \"$.value\", \"Next\": \"S1\"}], \"Default\": \"S2\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|createSingleChoice "$.var" ("$.value" |> ConditionCheck.StringEqualsPath) "S1"|] "S2"|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseBooleanExpressionChoiceState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let ioPath = $"{inputPath}, {outputPath}"
        let test1Expression = "{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\"}"
        let test1Obj = {Condition.Variable = "$.var"; Condition.Check = "IDDQD" |> ConditionCheck.StringEquals}
        let test2Expression = "{\"Variable\": \"$.var\", \"StringEqualsPath\": \"$.value\"}"
        let test2Obj = {Condition.Variable = "$.var"; Condition.Check = "$.value" |> ConditionCheck.StringEqualsPath}
        let test3Expression = "{\"Variable\": \"$.var\", \"StringLessThan\": \"IDKFA\"}"
        let test3Obj = {Condition.Variable = "$.var"; Condition.Check = "IDKFA" |> ConditionCheck.StringLessThan}
        let createChoiceState = createChoiceState "S0" "IDDQD" "$.input" "$.output"
        // (Not E1) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Not\": " + test1Expression + ", \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = test1Obj |> createNotExpression; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // (E1 And E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"And\": [" + test1Expression + ", " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|test1Obj; test2Obj|] |> createAndExpression; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // (E1 Or E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Or\": [" + test1Expression + ", " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|test1Obj; test2Obj|] |> createOrExpression; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // (Not (E1 And E2)) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Not\": {\"And\": [" + test1Expression + ", " + test2Expression + "]}, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|test1Obj; test2Obj|] |> createAndExpression |> BoolExpr.NotExpr; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // (Not (E1 Or E2)) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Not\": {\"Or\": [" + test1Expression + ", " + test2Expression + "]}, \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|test1Obj; test2Obj|] |> createOrExpression |> BoolExpr.NotExpr; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // ((Not E1) And E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"And\": [{\"Not\": " + test1Expression + "}, " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|test1Obj |> createNotExpression; test2Obj |> BoolExpr.TrueExpr|] |> BoolExpr.AndExpr; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // ((Not E1) Or E2) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Or\": [{\"Not\": " + test1Expression + "}, " + test2Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|test1Obj |> createNotExpression; test2Obj |> BoolExpr.TrueExpr|] |> BoolExpr.OrExpr; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // ((E1 And E2) Or E3) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"Or\": [{\"And\": [" + test1Expression + ", " + test2Expression + "]}, " + test3Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|[|test1Obj; test2Obj|] |> createAndExpression; test3Obj |> BoolExpr.TrueExpr|] |> BoolExpr.OrExpr; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // ((E1 Or E2) And E3) expression
        let stateS0 = "{\"Type\": \"Choice\", \"Comment\": \"IDDQD\", " + ioPath + ", \"Choices\": [{\"And\": [{\"Or\": [" + test1Expression + ", " + test2Expression + "]}, " + test3Expression + "], \"Next\": \"S1\"}]}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createChoiceState [|{Choice.Rule = [|[|test1Obj; test2Obj|] |> createOrExpression; test3Obj |> BoolExpr.TrueExpr|] |> BoolExpr.AndExpr; Choice.Next = "S1"}|] null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseChoiceStateWithErrors() =
        // without Choices
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // without variable
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"StringEquals\": \"IDDQD\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // without data-test expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<ArgumentException> |> ignore
        // with unknown data-test expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringEqualsXXX\": \"IDDQD\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // without next
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // with next inside boolean expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Not\": {\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\", \"Next\": \"S1\"}}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // with empty AND expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"And\": [], \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // with single item AND expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"And\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\"}], \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // with empty OR expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Or\": [], \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // with single item OR expression
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Or\": [{\"Variable\": \"$.var\", \"StringEquals\": \"IDDQD\"}], \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringEquals argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringEquals\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringLessThan argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThan\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringLessThanPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringGreaterThan argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThan\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringGreaterThanPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringLessThanEquals argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanEquals\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringLessThanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringLessThanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringGreaterThanEquals argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanEquals\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringGreaterThanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringGreaterThanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // StringMatches argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"StringMatches\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericEquals argument isn't a number
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEquals\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericLessThan argument isn't a number
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThan\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericLessThanPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericGreaterThan argument isn't a number
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThan\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericGreaterThanPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericLessThanEquals argument isn't a number
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEquals\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericLessThanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericLessThanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericGreaterThanEquals argument isn't a number
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEquals\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // NumericGreaterThanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"NumericGreaterThanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // BooleanEquals argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"BooleanEquals\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // BooleanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"BooleanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampEquals argument isn't a timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampEquals\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampLessThan argument isn't a timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThan\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampLessThanPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampGreaterThan argument isn't a timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThan\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampGreaterThanPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampLessThanEquals argument isn't a timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanEquals\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampLessThanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampLessThanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampGreaterThanEquals argument isn't a timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanEquals\": \"666\", \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TimestampGreaterThanEqualsPath argument isn't a string
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"TimestampGreaterThanEqualsPath\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // IsNull argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"IsNull\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // IsPresent argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"IsPresent\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // IsNumeric argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"IsNumeric\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // IsString argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"IsString\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // IsBoolean argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"IsBoolean\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // IsTimestamp argument isn't a bool
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Choice\", \"Choices\": [{\"Variable\": \"$.var\", \"IsTimestamp\": 666, \"Next\": \"S1\"}]}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore

    [<Test>]
    member public this.ParseWaitState() =
        let createWaitState = createWaitState "S0" "IDDQD" "$.input" "$.output"
        // End, Seconds
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"Seconds\": 13}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState Continuation.End (WaitParameters.Seconds 13)|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, Seconds
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"Seconds\": 13}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState (Continuation.Next "S1") (WaitParameters.Seconds 13)|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, SecondsPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"SecondsPath\": \"$.seconds\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState Continuation.End (WaitParameters.SecondsPath "$.seconds")|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, SecondsPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"SecondsPath\": \"$.seconds\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState (Continuation.Next "S1") (WaitParameters.SecondsPath "$.seconds")|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, Timestamp
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"Timestamp\": \"2022-01-31T01:02:03Z\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState Continuation.End (new DateTime(2022, 01, 31, 01, 02, 03) |> WaitParameters.Timestamp)|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, Timestamp
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"Timestamp\": \"2022-01-31T01:02:03Z\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState (Continuation.Next "S1") (new DateTime(2022, 01, 31, 01, 02, 03) |> WaitParameters.Timestamp)|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, TimestampPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"End\": true, \"TimestampPath\": \"$.timestamp\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState Continuation.End (WaitParameters.TimestampPath "$.timestamp")|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, TimestampPath
        let stateS0 = "{\"Type\": \"Wait\", \"Comment\": \"IDDQD\", \"InputPath\": \"$.input\", \"OutputPath\": \"$.output\", \"Next\": \"S1\", \"TimestampPath\": \"$.timestamp\"}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createWaitState (Continuation.Next "S1") (WaitParameters.TimestampPath "$.timestamp")|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseWaitStateWithErrors() =
        // End, Without Seconds, SecondsPath, Timestamp, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, Without Seconds, SecondsPath, Timestamp, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With Seconds, SecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true, \"Seconds\": 13, \"SecondsPath\": \"$.seconds\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With Seconds, SecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\", \"Seconds\": 13, \"SecondsPath\": \"$.seconds\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With Seconds, Timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true, \"Seconds\": 13, \"Timestamp\": \"2022-01-31T01:02:03Z\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With Seconds, Timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\", \"Seconds\": 13, \"Timestamp\": \"2022-01-31T01:02:03Z\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With Seconds, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true, \"Seconds\": 13, \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With Seconds, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\", \"Seconds\": 13, \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With SecondsPath, Timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true, \"SecondsPath\": \"$.seconds\", \"Timestamp\": \"2022-01-31T01:02:03Z\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With SecondsPath, Timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\", \"SecondsPath\": \"$.seconds\", \"Timestamp\": \"2022-01-31T01:02:03Z\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With SecondsPath, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true, \"SecondsPath\": \"$.seconds\", \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With SecondsPath, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\", \"SecondsPath\": \"$.seconds\", \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With Timestamp, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"End\": true, \"Timestamp\": \"2022-01-31T01:02:03Z\", \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With Timestamp, TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Next\": \"S1\", \"Timestamp\": \"2022-01-31T01:02:03Z\", \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Without End, Next, With Seconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Seconds\": 13}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Without End, Next, With SecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"SecondsPath\": \"$.seconds\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Without End, Next, With Timestamp
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"Timestamp\": \"2022-01-31T01:02:03Z\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Without End, Next, With TimestampPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Wait\", \"TimestampPath\": \"$.timestamp\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore

    [<Test>]
    member public this.ParsePassState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let ioPath = $"{inputPath}, {outputPath}"
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let result = "\"Result\": {\"key\": 666}"
        let resultObj = "{\"key\": 666}" |> JObject.Parse
        let parametersObj = [|createPayloadTemplate "item.$" "$.value"|]
        let createPassState = createPassState "S0" "IDDQD" "$.input" "$.output"
        // End, without Result
        let stateS0 = $"{{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState Continuation.End "$.data" parametersObj null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Result
        let stateS0 = $"{{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState (Continuation.Next "S1") "$.data" parametersObj null|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, Result
        let stateS0 = $"{{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {result}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState Continuation.End "$.data" parametersObj resultObj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, Result
        let stateS0 = $"{{\"Type\": \"Pass\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {result}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let expected = [|createPassState (Continuation.Next "S1") "$.data" parametersObj resultObj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParsePassStateWithErrors() =
        // Without End, Next
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Pass\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // With End = false
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Pass\", \"End\": false}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore

    [<Test>]
    member public this.ParseTaskState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let ioPath = $"{inputPath}, {outputPath}"
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let parametersObj = [|createPayloadTemplate "item.$" "$.value"|]
        let resultSelector = "\"ResultSelector\": {\"data.$\": \"$.result\"}"
        let resultSelectorObj = [|createPayloadTemplate "data.$" "$.result"|]
        let retry = "\"Retry\" : [{\"ErrorEquals\": [\"States.TaskFailed\"], \"IntervalSeconds\": 3, \"MaxAttempts\": 2, \"BackoffRate\": 1.5}]"
        let retryObj = [|createRetrier [|"States.TaskFailed"|] 3 2 1.5|]
        let catch = "\"Catch\": [{\"ErrorEquals\": [\"States.ALL\"], \"Next\": \"S0\", \"ResultPath\": \"$.error-info\"}]"
        let catchObj = [|createCatcher [|"States.ALL"|] "S0" "$.error-info"|]
        let resource = "\"Resource\": \"some-rs\""
        let credentials = "\"Credentials\": {\"key\": 666}"
        let credentialsObj = "{\"key\": 666}" |> JObject.Parse
        let createTaskState = createTaskState "S0" "IDDQD" "$.input" "$.output"
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, with Retry, with Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}, {retry}, {catch}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj retryObj catchObj "some-rs" TimeoutParameters.None HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, with Retry, with Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}, {retry}, {catch}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj retryObj catchObj "some-rs" TimeoutParameters.None HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath, with Credentials
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}, {credentials}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None HeartbeatParameters.None credentialsObj
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath, with Credentials
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}, {credentials}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None HeartbeatParameters.None credentialsObj
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // unsupported properties
        (*// End, without Retry, without Catch, TimeoutSeconds, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}, \"TimeoutSeconds\": 13}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 13) HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, TimeoutSeconds, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}, \"TimeoutSeconds\": 13}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSeconds 13) HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, without Retry, without Catch, TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}, \"TimeoutSecondsPath\": \"$.timeout\"}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSecondsPath "$.timeout") HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, TimeoutSecondsPath, without HeartbeatSeconds/HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}, \"TimeoutSecondsPath\": \"$.timeout\"}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" (TimeoutParameters.TimeoutSecondsPath "$.timeout") HeartbeatParameters.None null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSeconds
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}, \"HeartbeatSeconds\": 19}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None (HeartbeatParameters.HeartbeatSeconds 19) null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSeconds
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}, \"HeartbeatSeconds\": 19}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None (HeartbeatParameters.HeartbeatSeconds 19) null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {resource}, \"HeartbeatSecondsPath\": \"$.heartbeat\"}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None (HeartbeatParameters.HeartbeatSecondsPath "$.heartbeat") null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, without TimeoutSeconds/TimeoutSecondsPath, with HeartbeatSecondsPath
        let stateS0 = $"{{\"Type\": \"Task\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {resource}, \"HeartbeatSecondsPath\": \"$.heartbeat\"}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createTaskState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty "some-rs" TimeoutParameters.None (HeartbeatParameters.HeartbeatSecondsPath "$.heartbeat") null
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)*)

    [<Test>]
    member public this.ParseTaskStateWithErrors() =
        // Without End, Next
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Resource\": \"some-rs\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // With End = false
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": false, \"Resource\": \"some-rs\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, Without Resource
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, Without Resource
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With TimeoutSeconds and TimeoutSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"TimeoutSeconds\": 60, \"TimeoutSecondsPath\": \"$.timeout\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With TimeoutSeconds and TimeoutSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"TimeoutSeconds\": 60, \"TimeoutSecondsPath\": \"$.timeout\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With HeartbeatSeconds and HeartbeatSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"HeartbeatSeconds\": 45, \"HeartbeatSecondsPath\": \"$.heartbeat\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With HeartbeatSeconds and HeartbeatSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"HeartbeatSeconds\": 45, \"HeartbeatSecondsPath\": \"$.heartbeat\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // unsupported properties
        // End, With TimeoutSeconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"TimeoutSeconds\": 60}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With TimeoutSeconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"TimeoutSeconds\": 60}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With TimeoutSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"TimeoutSecondsPath\": \"$.timeout\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With TimeoutSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"TimeoutSecondsPath\": \"$.timeout\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With HeartbeatSeconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"HeartbeatSeconds\": 45}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With HeartbeatSeconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"HeartbeatSeconds\": 45}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, With HeartbeatSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"HeartbeatSecondsPath\": \"$.heartbeat\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With HeartbeatSecondsPath
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"HeartbeatSecondsPath\": \"$.heartbeat\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // TODO (std_string) : think about impl and check
        (*// End, With HeartbeatSeconds LE TimeoutSeconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"some-rs\", \"TimeoutSeconds\": 45, \"HeartbeatSeconds\": 71}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, With HeartbeatSeconds LE TimeoutSeconds
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Task\", \"Next\": \"S1\", \"Resource\": \"some-rs\", \"TimeoutSeconds\": 45, \"HeartbeatSeconds\": 71}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore*)

    [<Test>]
    member public this.ParseParallelState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let ioPath = $"{inputPath}, {outputPath}"
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let parametersObj = [|createPayloadTemplate "item.$" "$.value"|]
        let resultSelector = "\"ResultSelector\": {\"data.$\": \"$.result\"}"
        let resultSelectorObj = [|createPayloadTemplate "data.$" "$.result"|]
        let retry = "\"Retry\" : [{\"ErrorEquals\": [\"States.TaskFailed\"], \"IntervalSeconds\": 3, \"MaxAttempts\": 2, \"BackoffRate\": 1.5}]"
        let retryObj = [|createRetrier [|"States.TaskFailed"|] 3 2 1.5|]
        let catch = "\"Catch\": [{\"ErrorEquals\": [\"States.ALL\"], \"Next\": \"S0\", \"ResultPath\": \"$.error-info\"}]"
        let catchObj = [|createCatcher [|"States.ALL"|] "S0" "$.error-info"|]
        let branch1 = "{\"StartAt\": \"B10\", \"States\": {\"B10\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs1\"}}}"
        let branch1Obj = [|createSimpleTaskState "B10" Continuation.End "rs1"|] |> createSimpleStateMachine "B10"
        let branch2 = "{\"StartAt\": \"B20\", \"States\": {\"B20\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs2\"}}}"
        let branch2Obj = [|createSimpleTaskState "B20" Continuation.End "rs2"|] |> createSimpleStateMachine "B20"
        let branches = "\"Branches\": [" + branch1 + ", " + branch2 + "]"
        let createParallelState = createParallelState "S0" "IDDQD" "$.input" "$.output"
        // End, without Retry, without Catch
        let stateS0 = $"{{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {branches}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch
        let stateS0 = $"{{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {branches}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, with Retry, with Catch
        let stateS0 = $"{{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {retry}, {catch}, {branches}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState Continuation.End "$.data" parametersObj resultSelectorObj retryObj catchObj [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, with Retry, with Catch
        let stateS0 = $"{{\"Type\": \"Parallel\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {retry}, {catch}, {branches}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createParallelState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj retryObj catchObj [|branch1Obj; branch2Obj|]
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseParallelStateWithErrors() =
        let branch1 = "{\"StartAt\": \"B10\", \"States\": {\"B10\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs1\"}}}"
        let branch2 = "{\"StartAt\": \"B20\", \"States\": {\"B20\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs2\"}}}"
        let branches = "\"Branches\": [" + branch1 + ", " + branch2 + "]"
        // Without End, Next
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Parallel\", " + branches + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // With End = false
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Parallel\", \"End\": false, " + branches + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, without Branches
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Parallel\", \"End\": true}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, without Branches
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Parallel\", \"Next\": \"S1\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore

    [<Test>]
    member public this.ParseMapState() =
        let inputPath = "\"InputPath\": \"$.input\""
        let outputPath = "\"OutputPath\": \"$.output\""
        let ioPath = $"{inputPath}, {outputPath}"
        let resultPath = "\"ResultPath\": \"$.data\""
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let parametersObj = [|createPayloadTemplate "item.$" "$.value"|]
        let resultSelector = "\"ResultSelector\": {\"data.$\": \"$.result\"}"
        let resultSelectorObj = [|createPayloadTemplate "data.$" "$.result"|]
        let retry = "\"Retry\" : [{\"ErrorEquals\": [\"States.TaskFailed\"], \"IntervalSeconds\": 3, \"MaxAttempts\": 2, \"BackoffRate\": 1.5}]"
        let retryObj = [|createRetrier [|"States.TaskFailed"|] 3 2 1.5|]
        let catch = "\"Catch\": [{\"ErrorEquals\": [\"States.ALL\"], \"Next\": \"S0\", \"ResultPath\": \"$.error-info\"}]"
        let catchObj = [|createCatcher [|"States.ALL"|] "S0" "$.error-info"|]
        let oldMapDetails = "\"ItemsPath\": \"$.Item\", \"Iterator\": {\"StartAt\": \"It0\", \"States\": {\"It0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs\"}}}"
        let iteratorObj = [|createSimpleTaskState "It0" Continuation.End "rs"|] |> createSimpleStateMachine "It0"
        let itemSelector = "\"ItemSelector\": {\"item.$\": \"$.value\"}"
        let itemSelectorObj = [|createPayloadTemplate "item.$" "$.value"|]
        let mapDetails = "\"ItemsPath\": \"$.Item\", \"ItemProcessor\": {\"StartAt\": \"It0\", \"States\": {\"It0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs\"}}}"
        let itemProcessorObj = [|createSimpleTaskState "It0" Continuation.End "rs"|] |> createSimpleStateMachine "It0"
        let createOldMapState = createOldMapState "S0" "IDDQD" "$.input" "$.output"
        let createMapState = createMapState "S0" "IDDQD" "$.input" "$.output"
        // End, without Retry, without Catch, Iterator, Parameters
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {oldMapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createOldMapState Continuation.End "$.data" parametersObj resultSelectorObj Array.empty Array.empty iteratorObj "$.Item" 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, Iterator, Parameters
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {oldMapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createOldMapState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj Array.empty Array.empty iteratorObj "$.Item" 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, with Retry, with Catch, Iterator, Parameters
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {parameters}, {resultSelector}, {retry}, {catch}, {oldMapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createOldMapState Continuation.End "$.data" parametersObj resultSelectorObj retryObj catchObj iteratorObj "$.Item" 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, with Retry, with Catch, Iterator, Parameters
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {parameters}, {resultSelector}, {retry}, {catch}, {oldMapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createOldMapState (Continuation.Next "S1") "$.data" parametersObj resultSelectorObj retryObj catchObj iteratorObj "$.Item" 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, without Retry, without Catch, ItemProcessor, ItemSelector
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {itemSelector}, {resultSelector}, {mapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState Continuation.End "$.data" Array.empty resultSelectorObj Array.empty Array.empty null itemProcessorObj "$.Item" null itemSelectorObj null null 0 0 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, without Retry, without Catch, ItemProcessor, ItemSelector
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {itemSelector}, {resultSelector}, {mapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState (Continuation.Next "S1") "$.data" Array.empty resultSelectorObj Array.empty Array.empty null itemProcessorObj "$.Item" null itemSelectorObj null null 0 0 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // End, with Retry, with Catch, ItemProcessor, ItemSelector
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"End\": true, {resultPath}, {itemSelector}, {resultSelector}, {retry}, {catch}, {mapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState Continuation.End "$.data" Array.empty resultSelectorObj retryObj catchObj null itemProcessorObj "$.Item" null itemSelectorObj null null 0 0 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)
        // Next, with Retry, with Catch, ItemProcessor, ItemSelector
        let stateS0 = $"{{\"Type\": \"Map\", \"Comment\": \"IDDQD\", {ioPath}, \"Next\": \"S1\", {resultPath}, {itemSelector}, {resultSelector}, {retry}, {catch}, {mapDetails}}}"
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": " + stateS0 + "}}" |> parser.Parse
        let stateS0Obj = createMapState (Continuation.Next "S1") "$.data" Array.empty resultSelectorObj retryObj catchObj null itemProcessorObj "$.Item" null itemSelectorObj null null 0 0 0
        let expected = [|stateS0Obj|] |> createSimpleStateMachine "S0"
        this.CheckStateMachine(expected, actual)

    [<Test>]
    member public this.ParseMapStateWithErrors() =
        let iterator = "\"Iterator\": {\"StartAt\": \"It0\", \"States\": {\"It0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs\"}}}"
        let itemProcessor = "\"ItemProcessor\": {\"StartAt\": \"It0\", \"States\": {\"It0\": {\"Type\": \"Task\", \"End\": true, \"Resource\": \"rs\"}}}"
        let parameters = "\"Parameters\": {\"item.$\": \"$.value\"}"
        let itemSelector = "\"ItemSelector\": {\"item.$\": \"$.value\"}"
        let itemReader = "\"ItemReader\": {\"Resource\": \"some-reader\"}"
        let itemBatcher = "\"ItemBatcher\": {\"MaxItemsPerBatch\": 666}"
        let resultWriter = "\"ResultWriter\": {\"Resource\": \"some-writer\"}"
        let maxConcurrency = "\"MaxConcurrency\": 666"
        let toleratedFailurePercentage = "\"ToleratedFailurePercentage\": 13"
        let toleratedFailureCount = "\"ToleratedFailureCount\": 13"
        // Without End, Next
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", " + iterator + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // With End = false
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": false, " + iterator + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, without Iterator
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, without Iterator
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\"}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, Iterator, ItemProcessor
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + iterator + "," + itemProcessor + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, Iterator, ItemProcessor
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + iterator + "," + itemProcessor + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, Parameters, ItemSelector
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + parameters + "," + itemSelector + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, Parameters, ItemSelector
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + parameters + "," + itemSelector + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // unsupported properties
        // End, ItemReader
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + itemReader + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, ItemReader
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + itemReader + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, ItemBatcher
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + itemBatcher + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, ItemBatcher
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + itemBatcher + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, ResultWriter
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + resultWriter + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, ResultWriter
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + resultWriter + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, MaxConcurrency
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + maxConcurrency + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, MaxConcurrency
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + maxConcurrency + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, ToleratedFailurePercentage
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + toleratedFailurePercentage + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, ToleratedFailurePercentage
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + toleratedFailurePercentage + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // End, ToleratedFailureCount
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"End\": true, " + itemProcessor + "," + toleratedFailureCount + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore
        // Next, ToleratedFailureCount
        let actual = "{\"StartAt\": \"S0\", \"States\": {\"S0\": {\"Type\": \"Map\", \"Next\": \"S1\", " + itemProcessor + "," + toleratedFailureCount + "}}}"
        (fun() -> actual |> parser.Parse |> ignore) |> Assert.Throws<InvalidOperationException> |> ignore