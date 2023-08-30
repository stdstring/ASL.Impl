namespace ASL.Core.Tests

open ASL.Core
open NUnit.Framework

[<TestFixture>]
type StateMachineValidatorTests() =
    static member private GetSuccessValidateData() =
        seq {
             // fail
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Fail", "Error": "DNMD", "Cause": "IDKFA"}}}"""
             // succeed
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Succeed"}}}"""
             // choice
             yield """{"StartAt": "S0", "States": {""" +
                   """"S0": {"Type": "Choice", "Choices": [{"Variable": "$.var", "IsNull": false, "Next": "S1"}]}, """ +
                   """"S1": {"Type": "Succeed"}}}"""
             // wait
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Wait", "End": true, "Seconds": 13}}}"""
             // pass
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Pass", "End": true}}}"""
             // task
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Task", "Resource": "some-rs", "End": true}}}"""
             yield """{"StartAt": "S0", "States": {""" +
                   """"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S1"}, """ +
                   """"S1": {"Type": "Task", "Resource": "some-rs", "End": true}}}"""
             yield """{"StartAt": "S0", "States": """ +
                   """{"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S1", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S1"}]}, """ +
                   """"S1": {"Type": "Succeed"}}}"""
             yield """{"StartAt": "S0", "States": """ +
                   """{"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S1", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S2"}]}, """ +
                   """"S1": {"Type": "Succeed"}, "S2": {"Type": "Succeed"}}}"""
             // map
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, "Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Succeed"}}}}}}"""
             yield """{"StartAt": "S0", "States": """ +
                   """{"S0": {"Type": "Map", "Next": "S1", "Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Succeed"}}}}, """ +
                   """"S1": {"Type": "Succeed"}}}"""
             yield """{"StartAt": "S0", "States": """ +
                   """{"S0": {"Type": "Map", "Next": "S1", "Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Succeed"}}}, "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S1"}]}, """ +
                   """"S1": {"Type": "Succeed"}}}"""
             yield """{"StartAt": "S0", "States": """ +
                   """{"S0": {"Type": "Map", "Next": "S1", "Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Succeed"}}}, "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S2"}]}, """ +
                   """"S1": {"Type": "Succeed"}, "S2": {"Type": "Succeed"}}}"""
             // parallel
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Parallel", "End": true, """ +
                   """"Branches": [{"StartAt": "SB1_0", "States": {"SB1_0": {"Type": "Succeed"}}}, {"StartAt": "SB2_0", "States": {"SB2_0": {"Type": "Succeed"}}}]}}}"""
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Parallel", "Next": "S1", """ +
                   """"Branches": [{"StartAt": "SB1_0", "States": {"SB1_0": {"Type": "Succeed"}}}, {"StartAt": "SB2_0", "States": {"SB2_0": {"Type": "Succeed"}}}]}, """+
                   """"S1": {"Type": "Succeed"}}}"""
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Parallel", "Next": "S1", """ +
                   """"Branches": [{"StartAt": "SB1_0", "States": {"SB1_0": {"Type": "Succeed"}}}, {"StartAt": "SB2_0", "States": {"SB2_0": {"Type": "Succeed"}}}], """ +
                   """"Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S1"}]}, """+
                   """"S1": {"Type": "Succeed"}}}"""
             yield """{"StartAt": "S0", "States": {"S0": {"Type": "Parallel", "Next": "S1", """ +
                   """"Branches": [{"StartAt": "SB1_0", "States": {"SB1_0": {"Type": "Succeed"}}}, {"StartAt": "SB2_0", "States": {"SB2_0": {"Type": "Succeed"}}}], """ +
                   """"Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S2"}]}, """ +
                   """"S1": {"Type": "Succeed"}, "S2": {"Type": "Succeed"}}}"""
        }

    static member private GetWarningValidateData() =
        seq {
            // choice
            yield [|"""{"StartAt": "S0", "States": {""" +
                    """"S0": {"Type": "Choice", "Choices": [{"Variable": "$.var", "IsNull": false, "Next": "S0"}], "Default": "S1"}, """ +
                    """"S1": {"Type": "Succeed"}}}""" :> obj;
                    [{Warning.Path = []; Warning.Reason = WarningReason.PossibleCycle}]|]
            yield [|"""{"StartAt": "S0", "States": {""" +
                    """"S0": {"Type": "Choice", "Choices": [{"Variable": "$.var", "IsNull": false, "Next": "S1"}], "Default": "S2"}, """ +
                    """"S1": {"Type": "Task", "Resource": "some-rs", "Next": "S0"}, "S2": {"Type": "Succeed"}}}""" :> obj;
                    [{Warning.Path = []; Warning.Reason = WarningReason.PossibleCycle}]|]
            // task
            yield [|"""{"StartAt": "S0", "States": """ +
                    """{"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S0", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S1"}]}, """ +
                    """"S1": {"Type": "Succeed"}}}""" :> obj;
                    [{Warning.Path = []; Warning.Reason = WarningReason.PossibleCycle}; {Warning.Path = []; Warning.Reason = WarningReason.EndViaCatchLinksOnly}]|]
            // map + choice
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Choice", "Choices": [{"Variable": "$.var", "IsNull": false, "Next": "SI0"}], "Default": "SI1"}, """ +
                    """"SI1": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Warning.Path = ["S0"]; Warning.Reason = WarningReason.PossibleCycle}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Choice", "Choices": [{"Variable": "$.var", "IsNull": false, "Next": "SI1"}], "Default": "SI2"}, """ +
                    """"SI1": {"Type": "Task", "Resource": "some-rs", "Next": "SI0"}, "SI2": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Warning.Path = ["S0"]; Warning.Reason = WarningReason.PossibleCycle}]|]
            // map + task
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Task", "Resource": "some-rs", "Next": "SI0", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "SI1"}]}, """ +
                    """"SI1": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Warning.Path = ["S0"]; Warning.Reason = WarningReason.PossibleCycle}; {Warning.Path = ["S0"]; Warning.Reason = WarningReason.EndViaCatchLinksOnly}]|]
        }

    static member private GetErrorValidateData() =
        seq {
            yield [|"""{"StartAt": "S00", "States": {"S0": {"Type": "Succeed"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.UnknownState}]|]
            yield [|"""{"StartAt": "S0", "States": {"S00": {"Type": "Succeed"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.UnknownState}]|]
            yield [|"""{"StartAt": "S0", "States": """ +
                    """{"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S1", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S2"}]}, """ +
                    """"S1": {"Type": "Succeed"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.UnknownState}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Succeed"}, "S1": {"Type": "Succeed"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.InaccessibleStates}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Succeed"}, "S1": {"Type": "Task", "Resource": "some-rs", "Next": "S0"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.InaccessibleStates}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S1"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.EmptyEndStates}; {Error.Path = []; Error.Reason = ErrorReason.UnknownState}]|]
            // map + ...
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, "Iterator": {"StartAt": "SI00", "States": {"SI0": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.UnknownState}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, "Iterator": {"StartAt": "SI0", "States": {"SI00": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.UnknownState}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, "Iterator": """ +
                    """{"StartAt": "SI0", "States": {"SI0": {"Type": "Task", "Resource": "some-rs", "Next": "S0"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.EmptyEndStates}; {Error.Path = ["S0"]; Error.Reason = ErrorReason.UnknownState}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Succeed"}, "SI1": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.InaccessibleStates}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Succeed"}, "SI1": {"Type": "Task", "Resource": "some-rs", "Next": "SI0"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.InaccessibleStates}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Task", "Resource": "some-rs", "Next": "SI1"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.EmptyEndStates}; {Error.Path = ["S0"]; Error.Reason = ErrorReason.UnknownState}]|]
            // without warnings when UnknownState error exists
            yield [|"""{"StartAt": "S0", "States": """ +
                    """{"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S0", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S1"}]}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.EmptyEndStates}; {Error.Path = []; Error.Reason = ErrorReason.UnknownState}]|]
        }

    static member private GetErrorWarningValidateData() =
        seq {
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S0"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.EmptyEndStates}];
                    [{Warning.Path = []; Warning.Reason = WarningReason.PossibleCycle}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Task", "Resource": "some-rs", "Next": "S0", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "S1"}]}, """ +
                    """"S1": {"Type": "Succeed"}, "S2": {"Type": "Succeed"}}}""" :> obj;
                    [{Error.Path = []; Error.Reason = ErrorReason.InaccessibleStates}];
                    [{Warning.Path = []; Warning.Reason = WarningReason.PossibleCycle}; {Warning.Path = []; Warning.Reason = WarningReason.EndViaCatchLinksOnly}]|]
            // map + ...
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Task", "Resource": "some-rs", "Next": "SI0"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.EmptyEndStates}];
                    [{Warning.Path = ["S0"]; Warning.Reason = WarningReason.PossibleCycle}]|]
            yield [|"""{"StartAt": "S0", "States": {"S0": {"Type": "Map", "End": true, """ +
                    """"Iterator": {"StartAt": "SI0", "States": {"SI0": {"Type": "Task", "Resource": "some-rs", "Next": "SI0", "Catch": [{"ErrorEquals": ["States.ALL"], "Next": "SI1"}]}, """ +
                    """"SI1": {"Type": "Succeed"}, "SI2": {"Type": "Succeed"}}}}}}""" :> obj;
                    [{Error.Path = ["S0"]; Error.Reason = ErrorReason.InaccessibleStates}];
                    [{Warning.Path = ["S0"]; Warning.Reason = WarningReason.PossibleCycle}; {Warning.Path = ["S0"]; Warning.Reason = WarningReason.EndViaCatchLinksOnly}]|]
        }

    [<TestCaseSource(nameof StateMachineValidatorTests.GetSuccessValidateData)>]
    member public this.SuccessValidate(source: string) =
        let parser = new StateMachineParser()
        let validator = new StateMachineValidator()
        let validateResult = source |> parser.Parse |> validator.Validate
        Assert.IsEmpty(validateResult.Errors)
        Assert.IsEmpty(validateResult.Warnings)

    [<TestCaseSource(nameof StateMachineValidatorTests.GetWarningValidateData)>]
    member public this.WarningValidate(source: string, warnings: Warning list) =
        let parser = new StateMachineParser()
        let validator = new StateMachineValidator()
        let validateResult = source |> parser.Parse |> validator.Validate
        Assert.IsEmpty(validateResult.Errors)
        Assert.AreEqual(new ResizeArray<Warning>(warnings), validateResult.Warnings)

    [<TestCaseSource(nameof StateMachineValidatorTests.GetErrorValidateData)>]
    member public this.ErrorValidate(source: string, errors: Error list) =
        let parser = new StateMachineParser()
        let validator = new StateMachineValidator()
        let validateResult = source |> parser.Parse |> validator.Validate
        Assert.IsEmpty(validateResult.Warnings)
        Assert.AreEqual(new ResizeArray<Error>(errors), validateResult.Errors)

    [<TestCaseSource(nameof StateMachineValidatorTests.GetErrorWarningValidateData)>]
    member public this.ErrorWarningValidate(source: string, errors: Error list, warnings: Warning list) =
        let parser = new StateMachineParser()
        let validator = new StateMachineValidator()
        let validateResult = source |> parser.Parse |> validator.Validate
        Assert.AreEqual(new ResizeArray<Warning>(warnings), validateResult.Warnings)
        Assert.AreEqual(new ResizeArray<Error>(errors), validateResult.Errors)
