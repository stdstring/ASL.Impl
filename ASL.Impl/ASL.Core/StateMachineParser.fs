namespace ASL.Core

open Newtonsoft.Json.Linq
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Extension>]
type internal JObjectExtensions =

    [<Extension>]
    static member public GetOptionalValue<'TValue> (source: JObject, name: string, defaultValue: 'TValue) =
        match source.[name] with
        | null -> defaultValue
        | property -> property.ToObject<'TValue>()

    [<Extension>]
    static member public GetOptionalValue<'TValue> (source: JObject, name: string, predicate: 'TValue -> bool, defaultValue: 'TValue) =
        match source.[name] with
        | null -> defaultValue
        | property ->
            let value = property.ToObject<'TValue>()
            match value |> predicate with
            | false -> raise (System.InvalidOperationException("Bad Data"))
            | true -> value

    [<Extension>]
    static member public GetMandatoryValue<'TValue> (source: JObject, name: string) =
        match source.[name] with
        | null -> raise (System.InvalidOperationException("Bad Data"))
        | property -> property.ToObject<'TValue>()

type public StateMachineParser() =

    let checkAllowedKeys (source: JObject) (allowedKeys: seq<string>) =
        let allowedKeysSet = new HashSet<string>(allowedKeys)
        source.Children() |> Seq.cast<JProperty> |> Seq.map (fun property -> property.Name) |> Seq.forall allowedKeysSet.Contains

    let parseContinuation (source: JObject) =
        let nextState = source.["Next"]
        let isEnd = source.["End"]
        match nextState, isEnd with
        | _, null when nextState <> null -> nextState.ToObject<string>() |> Continuation.Next
        | null, _ when isEnd <> null ->
            match isEnd.ToObject<bool>() with
            | true -> Continuation.End
            | false -> raise (System.InvalidOperationException("Bad Data"))
        | _ -> Continuation.None

    let parseTimeoutParameters (source: JObject) =
        let timeoutSeconds = source.["TimeoutSeconds"]
        let timeoutSecondsPath = source.["TimeoutSecondsPath"]
        match timeoutSeconds, timeoutSecondsPath with
        | _, null when timeoutSeconds <> null -> timeoutSeconds.ToObject<int>() |> TimeoutParameters.TimeoutSeconds
        | null, _ when timeoutSecondsPath <> null -> timeoutSecondsPath.ToObject<string>() |> TimeoutParameters.TimeoutSecondsPath
        | null, null -> TimeoutParameters.None
        | _ -> raise (System.InvalidOperationException("Bad Data"))

    let parseHeartbeatParameters (source: JObject) =
        let heartbeatSeconds = source.["HeartbeatSeconds"]
        let heartbeatSecondsPath = source.["HeartbeatSecondsPath"]
        match heartbeatSeconds, heartbeatSecondsPath with
        | _, null when heartbeatSeconds <> null -> heartbeatSeconds.ToObject<int>() |> HeartbeatParameters.HeartbeatSeconds
        | null, _ when heartbeatSecondsPath <> null -> heartbeatSecondsPath.ToObject<string>() |> HeartbeatParameters.HeartbeatSecondsPath
        | null, null -> HeartbeatParameters.None
        | _ -> raise (System.InvalidOperationException("Bad Data"))

    let parseWaitParameters (source: JObject) =
        let seconds = source.["Seconds"]
        let secondsPath = source.["SecondsPath"]
        let timestamp = source.["Timestamp"]
        let timestampPath = source.["TimestampPath"]
        match seconds, secondsPath, timestamp, timestampPath with
        | null, null, null, null -> WaitParameters.None
        | _, null, null, null when seconds <> null -> seconds.ToObject<int>() |> WaitParameters.Seconds
        | null, _, null, null when secondsPath <> null -> secondsPath.ToObject<string>() |> WaitParameters.SecondsPath
        | null, null, _, null when timestamp <> null -> timestamp.ToObject<System.DateTime>() |> WaitParameters.Timestamp
        | null, null, null, _ when timestampPath <> null -> timestampPath.ToObject<string>() |> WaitParameters.TimestampPath
        | _ -> raise (System.InvalidOperationException("Bad Data"))

    let createPayloadTemplate (payloadTemplate: JProperty) =
        {PayloadTemplate.Name = payloadTemplate.Name; PayloadTemplate.Value = payloadTemplate.Value.ToObject<string>()}

    let parsePayloadTemplate (name: string) (source: JObject) =
        match source.[name] with
        | null -> Array.empty
        | parameters -> parameters.Children() |> Seq.cast<JProperty> |> Seq.map createPayloadTemplate |> Seq.toArray

    let checkValue (expectedType: JTokenType) (value: JToken) =
        match value.Type = expectedType with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true -> value

    let parseConditionCheck (source: JObject) =
        let condition = source.Children() |> Seq.cast<JProperty> |> Seq.filter (fun property -> property.Name <> "Variable" && property.Name <> "Next") |> Seq.exactlyOne
        match condition.Name with
        | "StringEquals" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringEquals
        | "StringEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringEqualsPath
        | "StringLessThan" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringLessThan
        | "StringLessThanPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringLessThanPath
        | "StringGreaterThan" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringGreaterThan
        | "StringGreaterThanPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringGreaterThanPath
        | "StringLessThanEquals" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringLessThanEquals
        | "StringLessThanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringLessThanEqualsPath
        | "StringGreaterThanEquals" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringGreaterThanEquals
        | "StringGreaterThanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringGreaterThanEqualsPath
        | "StringMatches" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.StringMatches
        | "NumericEquals" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericEqualsI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericEqualsF
            | _ -> raise (System.InvalidOperationException("Bad Data"))
        | "NumericEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.NumericEqualsPath
        | "NumericLessThan" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericLessThanI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericLessThanF
            | _ -> raise (System.InvalidOperationException("Bad Data"))
        | "NumericLessThanPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.NumericLessThanPath
        | "NumericGreaterThan" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericGreaterThanI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericGreaterThanF
            | _ -> raise (System.InvalidOperationException("Bad Data"))
        | "NumericGreaterThanPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.NumericGreaterThanPath
        | "NumericLessThanEquals" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericLessThanEqualsI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericLessThanEqualsF
            | _ -> raise (System.InvalidOperationException("Bad Data"))
        | "NumericLessThanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.NumericLessThanEqualsPath
        | "NumericGreaterThanEquals" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericGreaterThanEqualsI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericGreaterThanEqualsF
            | _ -> raise (System.InvalidOperationException("Bad Data"))
        | "NumericGreaterThanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.NumericGreaterThanEqualsPath
        | "BooleanEquals"->
            (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.BooleanEquals
        | "BooleanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.BooleanEqualsPath
        | "TimestampEquals" ->
            (condition.Value |> checkValue JTokenType.Date).ToObject<System.DateTime>() |> ConditionCheck.TimestampEquals
        | "TimestampEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.TimestampEqualsPath
        | "TimestampLessThan" ->
            (condition.Value |> checkValue JTokenType.Date).ToObject<System.DateTime>() |> ConditionCheck.TimestampLessThan
        | "TimestampLessThanPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.TimestampLessThanPath
        | "TimestampGreaterThan" ->
            (condition.Value |> checkValue JTokenType.Date).ToObject<System.DateTime>() |> ConditionCheck.TimestampGreaterThan
        | "TimestampGreaterThanPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.TimestampGreaterThanPath
        | "TimestampLessThanEquals" ->
            (condition.Value |> checkValue JTokenType.Date).ToObject<System.DateTime>() |> ConditionCheck.TimestampLessThanEquals
        | "TimestampLessThanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.TimestampLessThanEqualsPath
        | "TimestampGreaterThanEquals" ->
            (condition.Value |> checkValue JTokenType.Date).ToObject<System.DateTime>() |> ConditionCheck.TimestampGreaterThanEquals
        | "TimestampGreaterThanEqualsPath" ->
            (condition.Value |> checkValue JTokenType.String).ToObject<string>() |> ConditionCheck.TimestampGreaterThanEqualsPath
        | "IsNull" -> (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.IsNull
        | "IsPresent" -> (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.IsPresent
        | "IsNumeric" -> (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.IsNumeric
        | "IsString" -> (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.IsString
        | "IsBoolean" -> (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.IsBoolean
        | "IsTimestamp" -> (condition.Value |> checkValue JTokenType.Boolean).ToObject<bool>() |> ConditionCheck.IsTimestamp
        | _ -> raise (System.InvalidOperationException("Bad Data"))

    let parseRetrier(source: JObject) =
        let errorEquals = source.GetMandatoryValue<string[]>("ErrorEquals")
        let intervalSeconds = source.GetOptionalValue<int>("IntervalSeconds", (fun value -> value > 0), 1)
        let maxAttempts = source.GetOptionalValue<int>("MaxAttempts", (fun value -> value >= 0), 3)
        let backoffRate = source.GetOptionalValue<double>("BackoffRate", (fun value -> value >= 1.0), 2.0)
        {Retrier.ErrorEquals = errorEquals;
         Retrier.IntervalSeconds = intervalSeconds;
         Retrier.MaxAttempts = maxAttempts;
         Retrier.BackoffRate = backoffRate}

    let parseRetry(source: JObject) =
        match source.["Retry"] with
        | null -> [||]
        | retryValue -> retryValue.Children() |> Seq.cast<JObject> |> Seq.map parseRetrier |> Seq.toArray

    let parseCatcher(source: JObject) =
        let errorEquals = source.GetMandatoryValue<string[]>("ErrorEquals")
        let next = source.GetMandatoryValue<string>("Next")
        let resultPath = source.GetOptionalValue<string>("ResultPath", null)
        {Catcher.ErrorEquals = errorEquals; Catcher.Next = next; Catcher.ResultPath = resultPath}

    let parseCatch(source: JObject) =
        match source.["Catch"] with
        | null -> [||]
        | catchValue -> catchValue.Children() |> Seq.cast<JObject> |> Seq.map (fun item -> item |> parseCatcher) |> Seq.toArray

    let parseCondition(source: JObject) =
        let variable = source.GetMandatoryValue<string>("Variable")
        let conditionCheck = source |> parseConditionCheck
        {Condition.Variable = variable; Condition.Check = conditionCheck}

    let rec parseBoolExpression(source: JObject) =
        match source.["And"] with
        | null ->
            match source.["Or"] with
            | null ->
                match source.["Not"] with
                | null ->
                    source |> parseCondition |> BoolExpr.TrueExpr
                | notProperty ->
                    notProperty :?> JObject |> parseBoolExpression |> BoolExpr.NotExpr
            | orProperty ->
                match orProperty.Children() |> Seq.cast<JObject> |> Seq.map parseBoolExpression |> Seq.toArray with
                | subexprs when subexprs.Length < 2 -> raise (System.InvalidOperationException("Bad Data"))
                | subexprs -> subexprs |> BoolExpr.OrExpr
        | andProperty ->
            match andProperty.Children() |> Seq.cast<JObject> |> Seq.map parseBoolExpression |> Seq.toArray with
            | subexprs when subexprs.Length < 2 -> raise (System.InvalidOperationException("Bad Data"))
            | subexprs -> subexprs |> BoolExpr.AndExpr

    let parseChoiceNext (allowNext: bool) (source: JObject) =
        match source.["Next"] with
        | nextProperty when (nextProperty <> null) <> allowNext -> raise (System.InvalidOperationException("Bad Data"))
        | null -> null
        | nextProperty -> nextProperty.ToObject<string>()

    let parseChoice(source: JObject, allowNext: bool) =
        let next = source |> parseChoiceNext allowNext
        let rule = source |> parseBoolExpression
        {Choice.Next = next; Choice.Rule = rule}

    member private this.ParseState(name: string, source: JObject) : IState =
        let state: IState = match source.GetMandatoryValue<string>("Type") with
                            | "Task" -> this.ParseTaskState(name, source)
                            | "Parallel" -> this.ParseParallelState(name, source)
                            | "Map" ->  this.ParseMapState(name, source)
                            | "Pass" ->  this.ParsePassState(name, source)
                            | "Wait" ->  this.ParseWaitState(name, source)
                            | "Choice" ->  this.ParseChoiceState(name, source)
                            | "Succeed" ->  this.ParseSucceedState(name, source)
                            | "Fail" ->  this.ParseFailState(name, source)
                            | _ -> raise (System.InvalidOperationException("Bad Data"))
        match state.Validate() with
        | true -> state
        | false -> raise (System.InvalidOperationException("Bad Data"))

    member private this.ParseFailState(name: string, source: JObject) =
        match ["Type"; "Comment"; "Error"; "Cause"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let error = source.GetMandatoryValue<string>("Error")
            let cause = source.GetMandatoryValue<string>("Cause")
            new FailState(Name = name, Comment = comment, Error = error, Cause = cause)

    member private this.ParseSucceedState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            new SucceedState(Name = name, Comment = comment, InputPath = inputPath, OutputPath = outputPath)

    member private this.ParseChoiceState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"; "Choices"; "Default"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            let defaultState = source.GetOptionalValue<string>("Default", null)
            match source.["Choices"] with
            | null -> raise (System.InvalidOperationException("Bad Data"))
            | choicesValue ->
                match choicesValue.Children() |> Seq.cast<JObject> |> Seq.map (fun choice -> parseChoice(choice, true)) |> Seq.toArray with
                | [||] -> raise (System.InvalidOperationException("Bad Data"))
                | choices ->
                    new ChoiceState(Name = name,
                                    Comment = comment,
                                    InputPath = inputPath,
                                    OutputPath = outputPath,
                                    Choices = choices,
                                    Default = defaultState)

    member private this.ParseWaitState(name: string, source: JObject) =
        match ["Type";
               "Comment";
               "InputPath";
               "OutputPath";
               "Next";
               "End";
               "Seconds";
               "SecondsPath";
               "Timestamp";
               "TimestampPath"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            let continuation = source |> parseContinuation
            let waitParameters = source |> parseWaitParameters
            new WaitState(Name = name,
                          Comment = comment,
                          InputPath = inputPath,
                          OutputPath = outputPath,
                          Continuation = continuation,
                          WaitParameters = waitParameters)

    member private this.ParsePassState(name: string, source: JObject) =
        match ["Type";
               "Comment";
               "InputPath";
               "OutputPath";
               "Next";
               "End";
               "ResultPath";
               "Parameters";
               "Result"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            let continuation = source |> parseContinuation
            let resultPath = source.GetOptionalValue<string>("ResultPath", null)
            let parameters = source |> parsePayloadTemplate "Parameters"
            let result = (let value = source.["Result"] in if value = null then null else new JObject(value :?> JObject))
            new PassState(Name = name,
                          Comment = comment,
                          InputPath = inputPath,
                          OutputPath = outputPath,
                          Continuation = continuation,
                          ResultPath = resultPath,
                          Parameters = parameters,
                          Result = result)

    member private this.ParseTaskState(name: string, source: JObject) =
        match ["Type";
               "Comment";
               "InputPath";
               "OutputPath";
               "Next";
               "End";
               "ResultPath";
               "Parameters";
               "ResultSelector";
               "Retry";
               "Catch";
               "Resource";
               "TimeoutSeconds";
               "TimeoutSecondsPath";
               "HeartbeatSeconds";
               "HeartbeatSecondsPath";
               "Credentials"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            let continuation = source |> parseContinuation
            let resultPath = source.GetOptionalValue<string>("ResultPath", null)
            let parameters = source |> parsePayloadTemplate "Parameters"
            let resultSelector = source |> parsePayloadTemplate "ResultSelector"
            let retry = source |> parseRetry
            let catch = source |> parseCatch
            let resource = source.GetMandatoryValue<string>("Resource")
            let timeoutParameters = source |> parseTimeoutParameters
            let heartbeatParameters = source |> parseHeartbeatParameters
            let credentials = (let value = source.["Credentials"] in if value = null then null else new JObject(value :?> JObject))
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

    member private this.ParseParallelState(name: string, source: JObject) =
        match ["Type";
               "Comment";
               "InputPath";
               "OutputPath";
               "Next";
               "End";
               "ResultPath";
               "Parameters";
               "ResultSelector";
               "Retry";
               "Catch";
               "Branches"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            let continuation = source |> parseContinuation
            let resultPath = source.GetOptionalValue<string>("ResultPath", null)
            let parameters = source |> parsePayloadTemplate "Parameters"
            let resultSelector = source |> parsePayloadTemplate "ResultSelector"
            let retry = source |> parseRetry
            let catch = source |> parseCatch
            match source.["Branches"] with
            | null -> raise (System.InvalidOperationException("Bad Data"))
            | branchesValue ->
                let branches = branchesValue.Children() |> Seq.cast<JObject> |> Seq.map this.Parse |> Seq.toArray
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

    member private this.ParseMapState(name: string, source: JObject) =
        match ["Type";
               "Comment";
               "InputPath";
               "OutputPath";
               "Next";
               "End";
               "ResultPath";
               "Parameters";
               "ResultSelector";
               "Retry";
               "Catch";
               "Iterator";
               "ItemProcessor";
               "ItemsPath";
               "ItemReader";
               "ItemSelector";
               "ItemBatcher";
               "ResultWriter";
               "MaxConcurrency";
               "ToleratedFailurePercentage";
               "ToleratedFailureCount"] |> checkAllowedKeys source with
        | false -> raise (System.InvalidOperationException("Bad Data"))
        | true ->
            let comment = source.GetOptionalValue<string>("Comment", null)
            let inputPath = source.GetOptionalValue<string>("InputPath", null)
            let outputPath = source.GetOptionalValue<string>("OutputPath", null)
            let continuation = source |> parseContinuation
            let resultPath = source.GetOptionalValue<string>("ResultPath", null)
            let parameters = source |> parsePayloadTemplate "Parameters"
            let resultSelector = source |> parsePayloadTemplate "ResultSelector"
            let retry = source |> parseRetry
            let catch = source |> parseCatch
            let iterator = (let value = source.["Iterator"] in if value = null then null else value :?> JObject |> this.Parse)
            let itemProcessor = (let value = source.["ItemProcessor"] in if value = null then null else value :?> JObject |> this.Parse)
            let itemsPath = source.GetOptionalValue<string>("ItemsPath", null)
            let itemReader = (let value = source.["ItemReader"] in if value = null then null else new JObject(value :?> JObject))
            let itemSelector = source |> parsePayloadTemplate "ItemSelector"
            let itemBatcher = (let value = source.["ItemBatcher"] in if value = null then null else new JObject(value :?> JObject))
            let resultWriter = (let value = source.["ResultWriter"] in if value = null then null else new JObject(value :?> JObject))
            let maxConcurrency = source.GetOptionalValue<int>("MaxConcurrency", 0)
            let toleratedFailurePercentage = source.GetOptionalValue<int>("ToleratedFailurePercentage", 0)
            let toleratedFailureCount = source.GetOptionalValue<int>("ToleratedFailureCount", 0)
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
                         ToleratedFailureCount= toleratedFailureCount)

    member private this.Parse(source: JObject) =
        let startAt = source.GetMandatoryValue<string>("StartAt")
        let comment = source.GetOptionalValue<string>("Comment", null)
        let version = source.GetOptionalValue<string>("Version", null)
        let timeoutSeconds = source.GetOptionalValue<int>("TimeoutSeconds", 0)
        match source.["States"] with
        | null -> raise (System.InvalidOperationException("Bad Data"))
        | values ->
            let states = values.Children() |> Seq.cast<JProperty> |> Seq.map (fun property -> this.ParseState(property.Name, property.Value :?> JObject)) |> Seq.toArray
            new StateMachine(StartAt = startAt,
                             Comment = comment,
                             Version = version,
                             TimeoutSeconds = timeoutSeconds,
                             States = states)

    member public this.Parse(source: string) =
        source |> JObject.Parse |> this.Parse