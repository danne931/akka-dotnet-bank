namespace Bank.Transfer.Domain

[<RequireQualifiedAccess>]
type TransferCategory =
   | InternalWithinOrg
   | InternalAutomatedWithinOrg
   | InternalBetweenOrgs
   | Domestic

module TransferCategory =
   let fromString (str: string) : TransferCategory option =
      match str.ToLower() with
      | "internalwithingorg" -> Some TransferCategory.InternalWithinOrg
      | "internalautomatedwithinorg" ->
         Some TransferCategory.InternalAutomatedWithinOrg
      | "internalbetweenorgs" -> Some TransferCategory.InternalBetweenOrgs
      | "domestic" -> Some TransferCategory.Domestic
      | _ -> None

   let fromStringUnsafe str : TransferCategory =
      match fromString str with
      | Some s -> s
      | None -> failwith "Error attempting to cast string to TransferCategory"

[<RequireQualifiedAccess>]
type RecipientAccountEnvironment =
   | InternalWithinOrg
   | InternalBetweenOrgs
   | Domestic

module RecipientAccountEnvironment =
   let fromString (str: string) : RecipientAccountEnvironment option =
      match str.ToLower() with
      | "internalwithinorg" ->
         Some RecipientAccountEnvironment.InternalWithinOrg
      | "internalbetweenorgs" ->
         Some RecipientAccountEnvironment.InternalBetweenOrgs
      | "domestic" -> Some RecipientAccountEnvironment.Domestic
      | _ -> None

   let fromStringUnsafe str : RecipientAccountEnvironment =
      match fromString str with
      | Some s -> s
      | None ->
         failwith
            "Error attempting to cast string to RecipientAccountEnvironment"
