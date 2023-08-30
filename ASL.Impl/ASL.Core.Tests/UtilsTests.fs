namespace ASL.Core.Tests

open ASL.Core
open NUnit.Framework

[<TestFixture>]
type NameValidatorTests() =

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
    member public this.IsNameValid(name: string, expectedResult: bool) =
        Assert.AreEqual(expectedResult, name |> NameValidator.IsValid)