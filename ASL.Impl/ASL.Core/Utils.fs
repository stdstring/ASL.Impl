namespace ASL.Core

open System.Text.RegularExpressions

type internal NameValidator private() =

    static member public IsValid(name: string) =
        // A name must not contain:
        // 1. white space
        // 2. brackets < > { } [ ]
        // 3. wildcard characters ? *
        // 4. special characters " # % \ ^ | ~ ` $ & , ; : /
        // 5. control characters (U+0000-001F, U+007F-009F)
        // strict constraint: the name should only contain 0-9, A-Z, a-z, - and _.
        (name |> System.String.IsNullOrEmpty |> not) && Regex.IsMatch(name, "^[0-9A-Za-z_\-]+$", RegexOptions.None)