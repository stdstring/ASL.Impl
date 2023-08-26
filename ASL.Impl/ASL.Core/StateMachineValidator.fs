namespace ASL.Core

open System.Collections.Generic

type Path = string list

type WarningReason =
    | PossibleCycle
    | EndViaCatchLinksOnly

type Warning = {Path: Path; Reason: WarningReason}

type ErrorReason =
    | UnknownState
    | EmptyEndStates
    | InaccessibleStates

type Error = {Path: Path; Reason: ErrorReason}

type ValidateResult = {Errors: IList<Error>; Warnings: IList<Warning>}

module StateMachineValidatorImpl =

    type GraphNode = {Name: string; LinksTo: string[]; CatchLinksTo: string[]; End: bool}

    type StateMachineGraph = {StartNode: string;
                              Nodes: IDictionary<string, GraphNode>;
                              EndNodes: ISet<string>;
                              ChildrenGraphs: IList<string * StateMachineGraph>}

    type VisitType =
        | Empty
        | Enter
        | ViaUsualLink
        | ViaCatchLink
        | Both

open StateMachineValidatorImpl

type StateMachineValidator() =

    let createGraphNode (name: string) (linksTo: string[]) (catchLinksTo: string[]) (isEnd: bool) =
        {GraphNode.Name = name; GraphNode.LinksTo = linksTo; GraphNode.CatchLinksTo = catchLinksTo; GraphNode.End = isEnd}

    let createGraphNodeFromContinuation (name: string) (continuation: Continuation) (catchLinksTo: string[]) =
        match continuation with
        | Continuation.Next nextState -> createGraphNode name [|nextState|] catchLinksTo false
        | Continuation.End -> createGraphNode name [||] catchLinksTo true
        | Continuation.None -> failwith "Bad continuation value"

    let getChoiceStateLinks (choiceData: ChoiceState) =
        let defaultLink = match choiceData.Default with
                          | null -> []
                          | value -> [value]
        choiceData.Choices |> Seq.map (fun choice -> choice.Next) |> Seq.append defaultLink |> Seq.distinct |> Seq.toArray

    let getCatchLinks (catch: Catcher[]) =
        catch |> Seq.map (fun catcher -> catcher.Next) |> Seq.distinct |> Seq.toArray

    let generateGraphNode (state: IState) =
        match state.StateType with
        | StateType.Fail ->
            let failState = state :?> FailState
            createGraphNode failState.Name Array.empty Array.empty true
        | StateType.Succeed ->
            let succeedState = state :?> SucceedState
            createGraphNode succeedState.Name Array.empty Array.empty true
        | StateType.Choice ->
            let choiceState = state :?> ChoiceState
            createGraphNode choiceState.Name (choiceState |> getChoiceStateLinks) Array.empty false
        | StateType.Wait ->
            let waitState = state :?> WaitState
            createGraphNodeFromContinuation waitState.Name waitState.Continuation Array.empty
        | StateType.Pass ->
            let passState = state :?> PassState
            createGraphNodeFromContinuation passState.Name passState.Continuation Array.empty
        | StateType.Task ->
            let taskState = state :?> TaskState
            taskState.Catch |> getCatchLinks |> createGraphNodeFromContinuation taskState.Name taskState.Continuation
        | StateType.Map ->
            let mapState = state :?> MapState
            mapState.Catch |> getCatchLinks |> createGraphNodeFromContinuation mapState.Name mapState.Continuation
        | StateType.Parallel ->
            let parallelState = state :?> ParallelState
            parallelState.Catch |> getCatchLinks |> createGraphNodeFromContinuation parallelState.Name parallelState.Continuation
        | _ -> failwith "Unexpected StateType value"

    let rec createGraphForStateMachine (stateMachine: StateMachine) =
        let graphMap = new Dictionary<string, GraphNode>() :> IDictionary<string, GraphNode>
        let endNodes = new HashSet<string>() :> ISet<string>
        let сhildrenGraphs = new ResizeArray<string * StateMachineGraph>() :> IList<string * StateMachineGraph>
        for state in stateMachine.States do
            let graphNode = state |> generateGraphNode
            graphMap.Add(graphNode.Name, graphNode)
            if graphNode.End then
                graphNode.Name |> endNodes.Add |> ignore
            match state.StateType with
            | StateType.Map ->
                let mapState = state :?> MapState
                match mapState with
                | _ when (mapState.Iterator <> null) && (mapState.ItemProcessor = null) ->
                    (mapState.Name, mapState.Iterator |> createGraphForStateMachine) |> сhildrenGraphs.Add
                | _ when (mapState.Iterator = null) && (mapState.ItemProcessor <> null) ->
                    (mapState.Name, mapState.ItemProcessor |> createGraphForStateMachine) |> сhildrenGraphs.Add
                | _ -> failwith "Bad map state"
            | StateType.Parallel ->
                let parallelState = state :?> ParallelState
                parallelState.Branches |> Seq.iteri (fun index branch -> ($"{parallelState.Name}.{index}", branch |> createGraphForStateMachine) |> сhildrenGraphs.Add)
            | _ -> ()
        {StateMachineGraph.StartNode = stateMachine.StartAt;
         StateMachineGraph.Nodes = graphMap;
         StateMachineGraph.EndNodes = endNodes;
         StateMachineGraph.ChildrenGraphs = сhildrenGraphs}

    let hasUnknownStates (graph: StateMachineGraph) =
        match graph.StartNode |> graph.Nodes.ContainsKey with
        | false -> true
        | true ->
            let links = graph.Nodes |> Seq.map (fun pair -> Seq.append pair.Value.LinksTo pair.Value.CatchLinksTo) |> Seq.concat
            links |> Seq.filter (fun link -> link |> graph.Nodes.ContainsKey |> not) |> Seq.isEmpty |> not

    let visitNode (visitMap: IDictionary<string, VisitType>) (graph: StateMachineGraph) (node: string) (link: VisitType) (queue: Queue<string * VisitType>) =
        match visitMap[node] with
        | VisitType.Empty
        | VisitType.Enter ->
            visitMap[node] <- link
            graph.Nodes[node].LinksTo |> Seq.iter (fun nodeTo -> (nodeTo, VisitType.ViaUsualLink) |> queue.Enqueue)
            graph.Nodes[node].CatchLinksTo |> Seq.iter (fun nodeTo -> (nodeTo, VisitType.ViaCatchLink) |> queue.Enqueue)
            false
        | VisitType.ViaUsualLink when link = ViaCatchLink ->
            visitMap[node] <- VisitType.Both
            graph.Nodes[node].LinksTo |> Seq.iter (fun nodeTo -> (nodeTo, VisitType.ViaUsualLink) |> queue.Enqueue)
            graph.Nodes[node].CatchLinksTo |> Seq.iter (fun nodeTo -> (nodeTo, VisitType.ViaCatchLink) |> queue.Enqueue)
            false
        | VisitType.ViaCatchLink when link = ViaUsualLink ->
            visitMap[node] <- VisitType.Both
            graph.Nodes[node].LinksTo |> Seq.iter (fun nodeTo -> (nodeTo, VisitType.ViaUsualLink) |> queue.Enqueue)
            graph.Nodes[node].CatchLinksTo |> Seq.iter (fun nodeTo -> (nodeTo, VisitType.ViaCatchLink) |> queue.Enqueue)
            false
        | _ -> true

    let rec validateGraph (graph: StateMachineGraph) (path: Path) (result: ValidateResult) =
        if graph.EndNodes.Count = 0 then
            {Error.Path = path; Error.Reason = ErrorReason.EmptyEndStates} |> result.Errors.Add
        match graph |> hasUnknownStates with
        | true ->
            {Error.Path = path; Error.Reason = ErrorReason.UnknownState} |> result.Errors.Add
        | false ->
            let visitMap = new Dictionary<string, VisitType>(graph.Nodes |> Seq.map (fun node -> new KeyValuePair<string, VisitType>(node.Key, VisitType.Empty)))
            let queue = new Queue<string * VisitType>()
            (graph.StartNode, VisitType.Enter) |> queue.Enqueue
            let mutable hasPossibleCycle = false
            while queue.Count > 0 do
                let node, link = queue.Dequeue()
                hasPossibleCycle <- hasPossibleCycle || visitNode visitMap graph node link queue
            if hasPossibleCycle then
                {Warning.Path = path; Warning.Reason = WarningReason.PossibleCycle} |> result.Warnings.Add
            if visitMap |> Seq.filter (fun pair -> pair.Value = VisitType.Empty) |> Seq.isEmpty |> not then
                {Error.Path = path; Error.Reason = ErrorReason.InaccessibleStates} |> result.Errors.Add
            if (graph.EndNodes |> Seq.filter (fun node -> visitMap.[node] = VisitType.ViaCatchLink) |> Seq.isEmpty |> not) &&
               (graph.EndNodes |> Seq.filter (fun node -> visitMap.[node] = VisitType.ViaUsualLink) |> Seq.isEmpty) then
                {Warning.Path = path; Warning.Reason = WarningReason.EndViaCatchLinksOnly} |> result.Warnings.Add
        for childPath, childGraph in graph.ChildrenGraphs do
            result |> validateGraph childGraph ([childPath] |> List.append path) |> ignore
        result

    member public this.Validate(stateMachine: StateMachine) =
        // prepare data
        // check children state machines
        // check existence of state with End = true
        // check existence of all states in transitions
        // check absence of orphan (unreachable) states
        // check cycles (cycles allowed if they contains Choice states with several choices)
        let graph = stateMachine |> createGraphForStateMachine
        let result = {ValidateResult.Errors = new ResizeArray<Error>() :> IList<Error>;
                      ValidateResult.Warnings = new ResizeArray<Warning>() :> IList<Warning>}
        result |> validateGraph graph []