namespace ASL.Experiments

open Newtonsoft.Json.Linq
open System

type Continuation =
    | Next of string
    | End

type TimeoutParameters =
    | TimeoutSeconds of int
    | TimeoutSecondsPath of string

type HeartbeatParameters =
    | HeartbeatSeconds of int
    | HeartbeatSecondsPath of string
    | NotSpecified

type WaitParameters =
    | Seconds of int
    | SecondsPath of string
    | Timestamp of DateTime
    | TimestampPath of string

type Parameter = {Name: string; Value: string}

type ConditionCheck =
    | StringEquals of string
    | StringEqualsPath of string
    | StringLessThan of string
    | StringLessThanPath of string
    | StringGreaterThan of string
    | StringGreaterThanPath of string
    | StringLessThanEquals of string
    | StringLessThanEqualsPath of string
    | StringGreaterThanEquals of string
    | StringGreaterThanEqualsPath of string
    | StringMatches of string
    | NumericEqualsI of int
    | NumericEqualsF of float
    | NumericEqualsPath of string
    | NumericLessThanI of int
    | NumericLessThanF of float
    | NumericLessThanPath of string
    | NumericGreaterThanI of int
    | NumericGreaterThanF of float
    | NumericGreaterThanPath of string
    | NumericLessThanEqualsI of int
    | NumericLessThanEqualsF of float
    | NumericLessThanEqualsPath of string
    | NumericGreaterThanEqualsI of int
    | NumericGreaterThanEqualsF of float
    | NumericGreaterThanEqualsPath of string
    | BooleanEquals of bool
    | BooleanEqualsPath of string
    | TimestampEquals of DateTime
    | TimestampEqualsPath of string
    | TimestampLessThan of DateTime
    | TimestampLessThanPath of string
    | TimestampGreaterThan of DateTime
    | TimestampGreaterThanPath of string
    | TimestampLessThanEquals of DateTime
    | TimestampLessThanEqualsPath of string
    | TimestampGreaterThanEquals of DateTime
    | TimestampGreaterThanEqualsPath of string
    | IsNull of bool
    | IsPresent of bool
    | IsNumeric of bool
    | IsString of bool
    | IsBoolean of bool
    | IsTimestamp of bool

type Condition = {Variable: string; Check: ConditionCheck}

type BoolExpr =
    | AndExpr of BoolExpr[]
    | OrExpr of BoolExpr[]
    | NotExpr of BoolExpr
    | TrueExpr of Condition

type Choice = {Rule: BoolExpr; Next: string}

type Retrier = {ErrorEquals: string[]; IntervalSeconds: int; MaxAttempts: int; BackoffRate: float}

type Catcher = {ErrorEquals: string[]; Next: string; ResultPath: string}

type StateMachine = {StartAt: string;
                     Comment: string;
                     Version: string;
                     TimeoutSeconds: int;
                     States: State[]}

and TaskState = {Name: string;
                 Comment: string;
                 InputPath: string;
                 OutputPath: string;
                 Continuation: Continuation;
                 ResultPath: string;
                 Parameters: Parameter[];
                 ResultSelector: Parameter[];
                 Retry: Retrier[];
                 Catch: Catcher[];
                 Resource: string;
                 TimeoutParameters: TimeoutParameters;
                 HeartbeatParameters: HeartbeatParameters}

and ParallelState = {Name: string;
                     Comment: string;
                     InputPath: string;
                     OutputPath: string;
                     Continuation: Continuation;
                     ResultPath: string;
                     Parameters: Parameter[];
                     ResultSelector: Parameter[];
                     Retry: Retrier[];
                     Catch: Catcher[];
                     Branches: StateMachine[]}

and MapState = {Name: string;
                Comment: string;
                InputPath: string;
                OutputPath: string;
                Continuation: Continuation;
                ResultPath: string;
                Parameters: Parameter[];
                ResultSelector: Parameter[];
                Retry: Retrier[];
                Catch: Catcher[];
                Iterator: StateMachine;
                ItemsPath: string;
                MaxConcurrency: int}

and PassState = {Name: string;
                 Comment: string;
                 InputPath: string;
                 OutputPath: string;
                 Continuation: Continuation;
                 ResultPath: string;
                 Parameters: Parameter[];
                 // TODO (std_string) : think about type
                 Result: JObject}

and WaitState = {Name: string;
                 Comment: string;
                 InputPath: string;
                 OutputPath: string;
                 Continuation: Continuation;
                 WaitParameters: WaitParameters}

and ChoiceState = {Name: string;
                   Comment: string;
                   InputPath: string;
                   OutputPath: string;
                   Choices: Choice[];
                   Default: string}

and SucceedState = {Name: string;
                    Comment: string;
                    InputPath: string;
                    OutputPath: string}

and FailState = {Name: string;
                 Comment: string;
                 Error: string;
                 Cause: string}

and State =
    | Task of TaskState
    | Parallel of ParallelState
    | Map of MapState
    | Pass of PassState
    | Wait of WaitState
    | Choice of ChoiceState
    | Succeed of SucceedState
    | Fail of FailState

type StateMachineParser() =

    let checkAllowedKeys (source: JObject) (allowedKeys: seq<string>) =
        let allowedKeysSet = allowedKeys |> Set<string>
        source.Children() |> Seq.cast<JProperty> |> Seq.map (fun property -> property.Name) |> Seq.forall (fun key -> key |> allowedKeysSet.Contains)

    let parseContinuation (source: JObject) =
        let nextState = source.["Next"]
        let isEnd = source.["End"]
        match nextState, isEnd with
        | _, null when nextState <> null -> nextState.ToObject<string>() |> Continuation.Next
        | null, _ when isEnd <> null ->
            match isEnd.ToObject<bool>() with
            | true -> Continuation.End
            | false -> raise (InvalidOperationException("Bad Data"))
        | _ -> raise (InvalidOperationException("Bad Data"))

    let parseTimeoutParameters (source: JObject) =
        let timeoutSeconds = source.["TimeoutSeconds"]
        let timeoutSecondsPath = source.["TimeoutSecondsPath"]
        match timeoutSeconds, timeoutSecondsPath with
        | _, null when timeoutSeconds <> null -> timeoutSeconds.ToObject<int>() |> TimeoutParameters.TimeoutSeconds
        | null, _ when timeoutSecondsPath <> null -> timeoutSecondsPath.ToObject<string>() |> TimeoutParameters.TimeoutSecondsPath
        | null, null -> TimeoutParameters.TimeoutSeconds 60
        | _ -> raise (InvalidOperationException("Bad Data"))

    let parseHeartbeatParameters (source: JObject) =
        let heartbeatSeconds = source.["HeartbeatSeconds"]
        let heartbeatSecondsPath = source.["HeartbeatSecondsPath"]
        match heartbeatSeconds, heartbeatSecondsPath with
        | _, null when heartbeatSeconds <> null -> heartbeatSeconds.ToObject<int>() |> HeartbeatParameters.HeartbeatSeconds
        | null, _ when heartbeatSecondsPath <> null -> heartbeatSecondsPath.ToObject<string>() |> HeartbeatParameters.HeartbeatSecondsPath
        | null, null -> HeartbeatParameters.NotSpecified
        | _ -> raise (InvalidOperationException("Bad Data"))

    let parseWaitParameters (source: JObject) =
        let seconds = source.["Seconds"]
        let secondsPath = source.["SecondsPath"]
        let timestamp = source.["Timestamp"]
        let timestampPath = source.["TimestampPath"]
        match seconds, secondsPath, timestamp, timestampPath with
        | _, null, null, null when seconds <> null -> seconds.ToObject<int>() |> WaitParameters.Seconds
        | null, _, null, null when secondsPath <> null -> secondsPath.ToObject<string>() |> WaitParameters.SecondsPath
        | null, null, _, null when timestamp <> null -> timestamp.ToObject<DateTime>() |> WaitParameters.Timestamp
        | null, null, null, _ when timestampPath <> null -> timestampPath.ToObject<string>() |> WaitParameters.TimestampPath
        | _ -> raise (InvalidOperationException("Bad Data"))

    let createParameter (property: JProperty) =
        {Parameter.Name = property.Name; Parameter.Value = property.Value.ToObject<string>()}

    let parseParameters (name: string) (source: JObject) =
        match source.[name] with
        | null -> null
        | parameters -> parameters.Children() |> Seq.cast<JProperty> |> Seq.map createParameter |> Seq.toArray

    let parseConditionCheck (source: JObject) =
        let condition = source.Children() |> Seq.cast<JProperty> |> Seq.filter (fun property -> property.Name <> "Variable" && property.Name <> "Next") |> Seq.exactlyOne
        match condition.Name with
        | "StringEquals" -> condition.Value.ToObject<string>() |> ConditionCheck.StringEquals
        | "StringEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.StringEqualsPath
        | "StringLessThan" -> condition.Value.ToObject<string>() |> ConditionCheck.StringLessThan
        | "StringLessThanPath" -> condition.Value.ToObject<string>() |> ConditionCheck.StringLessThanPath
        | "StringGreaterThan" -> condition.Value.ToObject<string>() |> ConditionCheck.StringGreaterThan
        | "StringGreaterThanPath" -> condition.Value.ToObject<string>() |> ConditionCheck.StringGreaterThanPath
        | "StringLessThanEquals" -> condition.Value.ToObject<string>() |> ConditionCheck.StringLessThanEquals
        | "StringLessThanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.StringLessThanEqualsPath
        | "StringGreaterThanEquals" -> condition.Value.ToObject<string>() |> ConditionCheck.StringGreaterThanEquals
        | "StringGreaterThanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.StringGreaterThanEqualsPath
        | "StringMatches" -> condition.Value.ToObject<string>() |> ConditionCheck.StringMatches
        | "NumericEquals" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericEqualsI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericEqualsF
            | _ -> raise (InvalidOperationException("Bad Data"))
        | "NumericEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.NumericEqualsPath
        | "NumericLessThan" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericLessThanI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericLessThanF
            | _ -> raise (InvalidOperationException("Bad Data"))
        | "NumericLessThanPath" -> condition.Value.ToObject<string>() |> ConditionCheck.NumericLessThanPath
        | "NumericGreaterThan" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericGreaterThanI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericGreaterThanF
            | _ -> raise (InvalidOperationException("Bad Data"))
        | "NumericGreaterThanPath" -> condition.Value.ToObject<string>() |> ConditionCheck.NumericGreaterThanPath
        | "NumericLessThanEquals" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericLessThanEqualsI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericLessThanEqualsF
            | _ -> raise (InvalidOperationException("Bad Data"))
        | "NumericLessThanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.NumericLessThanEqualsPath
        | "NumericGreaterThanEquals" ->
            match condition.Value.Type with
            | JTokenType.Integer -> condition.Value.ToObject<int>() |> ConditionCheck.NumericGreaterThanEqualsI
            | JTokenType.Float -> condition.Value.ToObject<float>() |> ConditionCheck.NumericGreaterThanEqualsF
            | _ -> raise (InvalidOperationException("Bad Data"))
        | "NumericGreaterThanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.NumericGreaterThanEqualsPath
        | "BooleanEquals"-> condition.Value.ToObject<bool>() |> ConditionCheck.BooleanEquals
        | "BooleanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.BooleanEqualsPath
        | "TimestampEquals" -> condition.Value.ToObject<DateTime>() |> ConditionCheck.TimestampEquals
        | "TimestampEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.TimestampEqualsPath
        | "TimestampLessThan" -> condition.Value.ToObject<DateTime>() |> ConditionCheck.TimestampLessThan
        | "TimestampLessThanPath" -> condition.Value.ToObject<string>() |> ConditionCheck.TimestampLessThanPath
        | "TimestampGreaterThan" -> condition.Value.ToObject<DateTime>() |> ConditionCheck.TimestampGreaterThan
        | "TimestampGreaterThanPath" -> condition.Value.ToObject<string>() |> ConditionCheck.TimestampGreaterThanPath
        | "TimestampLessThanEquals" -> condition.Value.ToObject<DateTime>() |> ConditionCheck.TimestampLessThanEquals
        | "TimestampLessThanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.TimestampLessThanEqualsPath
        | "TimestampGreaterThanEquals" -> condition.Value.ToObject<DateTime>() |> ConditionCheck.TimestampGreaterThanEquals
        | "TimestampGreaterThanEqualsPath" -> condition.Value.ToObject<string>() |> ConditionCheck.TimestampGreaterThanEqualsPath
        | "IsNull" -> condition.Value.ToObject<bool>() |> ConditionCheck.IsNull
        | "IsPresent" -> condition.Value.ToObject<bool>() |> ConditionCheck.IsPresent
        | "IsNumeric" -> condition.Value.ToObject<bool>() |> ConditionCheck.IsNumeric
        | "IsString" -> condition.Value.ToObject<bool>() |> ConditionCheck.IsString
        | "IsBoolean" -> condition.Value.ToObject<bool>() |> ConditionCheck.IsBoolean
        | "IsTimestamp" -> condition.Value.ToObject<bool>() |> ConditionCheck.IsTimestamp
        | _ -> raise (InvalidOperationException("Bad Data"))

    let parseChoiceNext (allowNext: bool) (source: JObject) =
        match source.["Next"] with
        | nextProperty when (nextProperty <> null) <> allowNext -> raise (InvalidOperationException("Bad Data"))
        | null -> null
        | nextProperty -> nextProperty.ToObject<string>()

    member private this.GetOptionalValue<'TValue> (name: string, defaultValue: 'TValue, source: JObject) =
        match source.[name] with
        | null -> defaultValue
        | property -> property.ToObject<'TValue>()

    member private this.GetOptionalValue<'TValue> (name: string, predicate: 'TValue -> bool, defaultValue: 'TValue, source: JObject) =
        match source.[name] with
        | null -> defaultValue
        | property ->
            let value = property.ToObject<'TValue>()
            match value |> predicate with
            | false -> raise (InvalidOperationException("Bad Data"))
            | true -> value

    member private this.GetMandatoryValue<'TValue> (name: string, source: JObject) =
        match source.[name] with
        | null -> raise (InvalidOperationException("Bad Data"))
        | property -> property.ToObject<'TValue>()

    member private this.ParseRetrier(source: JObject) =
        let errorEquals = this.GetMandatoryValue<string[]>("ErrorEquals", source)
        let intervalSeconds = this.GetOptionalValue<int>("IntervalSeconds", (fun value -> value > 0), 1, source)
        let maxAttempts = this.GetOptionalValue<int>("MaxAttempts", (fun value -> value >= 0), 3, source)
        let backoffRate = this.GetOptionalValue<double>("BackoffRate", (fun value -> value >= 1.0), 2.0, source)
        {Retrier.ErrorEquals = errorEquals;
         Retrier.IntervalSeconds = intervalSeconds;
         Retrier.MaxAttempts = maxAttempts;
         Retrier.BackoffRate = backoffRate}

    member private this.ParseRetry(source: JObject) =
        match source.["Retry"] with
        | null -> [||]
        | retryValue -> retryValue.Children() |> Seq.cast<JObject> |> Seq.map (fun item -> item |> this.ParseRetrier) |> Seq.toArray

    member private this.ParseCatcher(source: JObject) =
        let errorEquals = this.GetMandatoryValue<string[]>("ErrorEquals", source)
        let next = this.GetMandatoryValue<string>("Next", source)
        let resultPath = this.GetOptionalValue<string>("ResultPath", null, source)
        {Catcher.ErrorEquals = errorEquals; Catcher.Next = next; Catcher.ResultPath = resultPath}

    member private this.ParseCatch(source: JObject) =
        match source.["Catch"] with
        | null -> [||]
        | catchValue -> catchValue.Children() |> Seq.cast<JObject> |> Seq.map (fun item -> item |> this.ParseCatcher) |> Seq.toArray

    member private this.ParseCondition(source: JObject) =
        let variable = this.GetMandatoryValue<string>("Variable", source)
        let conditionCheck = source |> parseConditionCheck
        {Condition.Variable = variable; Condition.Check = conditionCheck}

    member private this.ParseBoolExpression(source: JObject) =
        match source.["And"] with
        | null ->
            match source.["Or"] with
            | null ->
                match source.["Not"] with
                | null ->
                    source |> this.ParseCondition |> BoolExpr.TrueExpr
                | notProperty ->
                    notProperty :?> JObject |> this.ParseBoolExpression |> BoolExpr.NotExpr
            | orProperty ->
                orProperty.Children() |> Seq.cast<JObject> |> Seq.map this.ParseBoolExpression |> Seq.toArray |> BoolExpr.OrExpr
        | andProperty ->
            andProperty.Children() |> Seq.cast<JObject> |> Seq.map this.ParseBoolExpression |> Seq.toArray |> BoolExpr.AndExpr

    member private this.ParseChoice(source: JObject, allowNext: bool) =
        let next = source |> parseChoiceNext allowNext
        let rule = source |> this.ParseBoolExpression
        {Choice.Next = next; Choice.Rule = rule}

    member private this.ParseState(name: string, source: JObject) =
        match this.GetMandatoryValue<string>("Type", source) with
        | "Task" -> this.ParseTaskState(name, source)
        | "Parallel" -> this.ParseParallelState(name, source)
        | "Map" ->  this.ParseMapState(name, source)
        | "Pass" ->  this.ParsePassState(name, source)
        | "Wait" ->  this.ParseWaitState(name, source)
        | "Choice" ->  this.ParseChoiceState(name, source)
        | "Succeed" ->  this.ParseSucceedState(name, source)
        | "Fail" ->  this.ParseFailState(name, source)
        | _ -> raise (InvalidOperationException("Bad Data"))

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
               "HeartbeatSecondsPath"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            let continuation = source |> parseContinuation
            let resultPath = this.GetOptionalValue<string>("ResultPath", null, source)
            let parameters = source |> parseParameters "Parameters"
            let resultSelector = source |> parseParameters "ResultSelector"
            let retry = source |> this.ParseRetry
            let catch = source |> this.ParseCatch
            let resource = this.GetMandatoryValue<string>("Resource", source)
            let timeoutParameters = source |> parseTimeoutParameters
            let heartbeatParameters = source |> parseHeartbeatParameters
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

    member private this.ParseParallelState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"; "Next"; "End"; "ResultPath"; "Parameters"; "ResultSelector"; "Retry"; "Catch"; "Branches"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            let continuation = source |> parseContinuation
            let resultPath = this.GetOptionalValue<string>("ResultPath", null, source)
            let parameters = source |> parseParameters "Parameters"
            let resultSelector = source |> parseParameters "ResultSelector"
            let retry = source |> this.ParseRetry
            let catch = source |> this.ParseCatch
            match source.["Branches"] with
            | null -> raise (InvalidOperationException("Bad Data"))
            | branchesValue ->
                let branches = branchesValue.Children() |> Seq.cast<JObject> |> Seq.map this.Parse |> Seq.toArray
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
               "ItemsPath";
               "MaxConcurrency"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            let continuation = source |> parseContinuation
            let resultPath = this.GetOptionalValue<string>("ResultPath", null, source)
            let parameters = source |> parseParameters "Parameters"
            let resultSelector = source |> parseParameters "ResultSelector"
            let retry = source |> this.ParseRetry
            let catch = source |> this.ParseCatch
            let itemsPath = this.GetOptionalValue<string>("ItemsPath", null, source)
            let maxConcurrency = this.GetOptionalValue<int>("MaxConcurrency", 0, source)
            match source.["Iterator"] with
            | null -> raise (InvalidOperationException("Bad Data"))
            | iteratorValue ->
                let iterator = iteratorValue :?> JObject |> this.Parse
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

    member private this.ParsePassState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"; "Next"; "End"; "ResultPath"; "Parameters"; "Result"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            let continuation = source |> parseContinuation
            let resultPath = this.GetOptionalValue<string>("ResultPath", null, source)
            let parameters = source |> parseParameters "Parameters"
            let result = if source.["Result"] = null then null else new JObject(source.["Result"] :?> JObject)
            {PassState.Name = name;
             PassState.Comment = comment;
             PassState.InputPath = inputPath;
             PassState.OutputPath = outputPath;
             PassState.Continuation = continuation;
             PassState.ResultPath = resultPath;
             PassState.Parameters = parameters;
             PassState.Result = result} |> State.Pass

    member private this.ParseWaitState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"; "Next"; "End"; "Seconds"; "SecondsPath"; "Timestamp"; "TimestampPath"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            let continuation = source |> parseContinuation
            let waitParameters = source |> parseWaitParameters
            {WaitState.Name = name;
             WaitState.Comment = comment;
             WaitState.InputPath = inputPath;
             WaitState.OutputPath = outputPath;
             WaitState.Continuation = continuation;
             WaitState.WaitParameters = waitParameters} |> State.Wait

    member private this.ParseChoiceState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"; "Choices"; "Default"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            let defaultState = this.GetOptionalValue<string>("Default", null, source)
            match source.["Choices"] with
            | null -> raise (InvalidOperationException("Bad Data"))
            | choicesValue ->
                match choicesValue.Children() |> Seq.cast<JObject> |> Seq.map (fun choice -> this.ParseChoice(choice, true)) |> Seq.toArray with
                | [||] -> raise (InvalidOperationException("Bad Data"))
                | choices ->
                    {ChoiceState.Name = name;
                     ChoiceState.Comment = comment;
                     ChoiceState.InputPath = inputPath;
                     ChoiceState.OutputPath = outputPath;
                     ChoiceState.Choices = choices;
                     ChoiceState.Default = defaultState} |> State.Choice

    member private this.ParseSucceedState(name: string, source: JObject) =
        match ["Type"; "Comment"; "InputPath"; "OutputPath"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let inputPath = this.GetOptionalValue<string>("InputPath", null, source)
            let outputPath = this.GetOptionalValue<string>("OutputPath", null, source)
            {SucceedState.Name = name;
             SucceedState.Comment = comment;
             SucceedState.InputPath = inputPath;
             SucceedState.OutputPath = outputPath} |> State.Succeed

    member private this.ParseFailState(name: string, source: JObject) =
        match ["Type"; "Comment"; "Error"; "Cause"] |> checkAllowedKeys source with
        | false -> raise (InvalidOperationException("Bad Data"))
        | true ->
            let comment = this.GetOptionalValue<string>("Comment", null, source)
            let error = this.GetMandatoryValue<string>("Error", source)
            let cause = this.GetMandatoryValue<string>("Cause", source)
            {FailState.Name = name;
             FailState.Comment = comment;
             FailState.Error = error;
             FailState.Cause = cause} |> State.Fail

    member private this.Parse(source: JObject) =
        let startAt = this.GetMandatoryValue<string>("StartAt", source)
        let comment = this.GetOptionalValue<string>("Comment", null, source)
        let version = this.GetOptionalValue<string>("Version", null, source)
        let timeoutSeconds = this.GetOptionalValue<int>("TimeoutSeconds", -1, source)
        match source.["States"] with
        | null -> raise (InvalidOperationException("Bad Data"))
        | values ->
            let states = values.Children() |> Seq.cast<JProperty> |> Seq.map (fun property -> this.ParseState(property.Name, property.Value :?> JObject)) |> Seq.toArray
            {StateMachine.StartAt = startAt;
             StateMachine.Comment = comment;
             StateMachine.Version = version;
             StateMachine.TimeoutSeconds = timeoutSeconds;
             StateMachine.States = states}

    member public this.Parse(source: string) =
        source |> JObject.Parse |> this.Parse