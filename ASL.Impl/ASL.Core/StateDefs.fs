namespace ASL.Core

open Newtonsoft.Json.Linq

type public StateType =
    | Fail = 0
    | Succeed = 1
    | Choice = 2
    | Wait = 3
    | Pass = 4
    | Task = 5
    | Parallel = 6
    | Map = 7

[<AllowNullLiteralAttribute>]
type public IState =
    interface
        abstract member StateType: StateType
        abstract member Validate: unit -> bool
    end

[<AllowNullLiteralAttribute>]
type public StateMachine() =

    member val public StartAt: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public Version: string = "" with get, set
    member val public TimeoutSeconds: int = 0 with get, set
    member val public States: IState[] = Array.zeroCreate 0 with get, set

    member public this.Validate() =
        // TODO (std_string) : think about deep validation of SateMachine
        // TODO (std_string) : think about validation of Version property (if specified)
        (this.StartAt |> System.String.IsNullOrEmpty |> not) &&
        (this.States.Length > 0)

type public Continuation =
    | None
    | Next of string
    | End

type public PayloadTemplate = {Name: string; Value: string}

type public FailState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public Error: string = "" with get, set
    member val public Cause: string = "" with get, set

    interface IState with
        member this.StateType = StateType.Fail
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Error |> System.String.IsNullOrEmpty |> not) &&
            (this.Cause |> System.String.IsNullOrEmpty |> not)

type public SucceedState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set

    interface IState with
        member this.StateType = StateType.Succeed
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not)

type public ConditionCheck =
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
    | TimestampEquals of System.DateTime
    | TimestampEqualsPath of string
    | TimestampLessThan of System.DateTime
    | TimestampLessThanPath of string
    | TimestampGreaterThan of System.DateTime
    | TimestampGreaterThanPath of string
    | TimestampLessThanEquals of System.DateTime
    | TimestampLessThanEqualsPath of string
    | TimestampGreaterThanEquals of System.DateTime
    | TimestampGreaterThanEqualsPath of string
    | IsNull of bool
    | IsPresent of bool
    | IsNumeric of bool
    | IsString of bool
    | IsBoolean of bool
    | IsTimestamp of bool

type public Condition = {Variable: string; Check: ConditionCheck}

type public BoolExpr =
    | AndExpr of BoolExpr[]
    | OrExpr of BoolExpr[]
    | NotExpr of BoolExpr
    | TrueExpr of Condition

type public Choice = {Rule: BoolExpr; Next: string}

type public ChoiceState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set
    member val public Choices: Choice[] = Array.zeroCreate 0 with get, set
    member val public Default: string = "" with get, set

    interface IState with
        member this.StateType = StateType.Choice
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Choices.Length > 0)

type public WaitParameters =
    | None
    | Seconds of int
    | SecondsPath of string
    | Timestamp of System.DateTime
    | TimestampPath of string

type public WaitState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set
    member val public Continuation: Continuation = Continuation.None with get, set
    member val public WaitParameters: WaitParameters = WaitParameters.None with get, set

    interface IState with
        member this.StateType = StateType.Wait
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Continuation <> Continuation.None) &&
            (this.WaitParameters <> WaitParameters.None)

type public PassState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set
    member val public Continuation: Continuation = Continuation.None with get, set
    member val public ResultPath: string = "" with get, set
    member val public Parameters: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    // TODO (std_string) : think about type
    member val public Result: JObject = null with get, set

    interface IState with
        member this.StateType = StateType.Pass
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Continuation <> Continuation.None)

type public Retrier = {ErrorEquals: string[]; IntervalSeconds: int; MaxAttempts: int; BackoffRate: float}

type public Catcher = {ErrorEquals: string[]; Next: string; ResultPath: string}

type TimeoutParameters =
    | None
    | TimeoutSeconds of int
    | TimeoutSecondsPath of string

type HeartbeatParameters =
    | None
    | HeartbeatSeconds of int
    | HeartbeatSecondsPath of string

type public TaskState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set
    member val public Continuation: Continuation = Continuation.None with get, set
    member val public ResultPath: string = "" with get, set
    member val public Parameters: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    member val public ResultSelector: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    member val public Retry: Retrier[] = Array.zeroCreate 0 with get, set
    member val public Catch: Catcher[] = Array.zeroCreate 0 with get, set
    member val public Resource: string = "" with get, set
    member val public TimeoutParameters: TimeoutParameters = TimeoutParameters.None with get, set
    member val public HeartbeatParameters: HeartbeatParameters = HeartbeatParameters.None with get, set
    // TODO (std_string) : think about type
    member val public Credentials: JObject = null with get, set

    interface IState with
        member this.StateType = StateType.Task
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Continuation <> Continuation.None) &&
            (this.Resource |> System.String.IsNullOrEmpty |> not) &&
            // unsupported properties
            (this.TimeoutParameters = TimeoutParameters.None) &&
            (this.HeartbeatParameters = HeartbeatParameters.None)

type public ParallelState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set
    member val public Continuation: Continuation = Continuation.None with get, set
    member val public ResultPath: string = "" with get, set
    member val public Parameters: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    member val public ResultSelector: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    member val public Retry: Retrier[] = Array.zeroCreate 0 with get, set
    member val public Catch: Catcher[] = Array.zeroCreate 0 with get, set
    member val public Branches: StateMachine[] = Array.zeroCreate 0 with get, set

    interface IState with
        member this.StateType = StateType.Parallel
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Continuation <> Continuation.None) &&
            (this.Branches.Length > 0)

type public MapState() =

    member public this.StateType = (this :> IState).StateType
    member public this.Validate() = (this :> IState).Validate()
    member val public Name: string = "" with get, set
    member val public Comment: string = "" with get, set
    member val public InputPath: string = "" with get, set
    member val public OutputPath: string = "" with get, set
    member val public Continuation: Continuation = Continuation.None with get, set
    member val public ResultPath: string = "" with get, set
    // deprecated
    member val public Parameters: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    member val public ResultSelector: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    member val public Retry: Retrier[] = Array.zeroCreate 0 with get, set
    member val public Catch: Catcher[] = Array.zeroCreate 0 with get, set
    // deprecated
    member val public Iterator: StateMachine = null with get, set
    member val public ItemProcessor: StateMachine = null with get, set
    member val public ItemsPath: string = "" with get, set
    // TODO (std_string) : think about type
    member val public ItemReader: JObject = null with get, set
    member val public ItemSelector: PayloadTemplate[] = Array.zeroCreate 0 with get, set
    // TODO (std_string) : think about type
    member val public ItemBatcher: JObject = null with get, set
    // TODO (std_string) : think about type
    member val public ResultWriter: JObject = null with get, set
    member val public MaxConcurrency: int = 0 with get, set
    member val public ToleratedFailurePercentage: int = 0 with get, set
    member val public ToleratedFailureCount: int = 0 with get, set

    interface IState with
        member this.StateType = StateType.Map
        member this.Validate() =
            (this.Name |> System.String.IsNullOrEmpty |> not) &&
            (this.Continuation <> Continuation.None) &&
            ((this.Iterator = null) <> (this.ItemProcessor = null)) &&
            (((this.Parameters.Length > 0) && (this.ItemSelector.Length > 0)) |> not) &&
            // unsupported properties
            (this.ItemReader = null) &&
            (this.ItemBatcher = null) &&
            (this.ResultWriter = null) &&
            (this.MaxConcurrency = 0) &&
            (this.ToleratedFailurePercentage = 0) &&
            (this.ToleratedFailureCount = 0)