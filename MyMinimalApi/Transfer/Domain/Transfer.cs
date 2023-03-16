namespace Bank.Transfer.Domain;

public enum InternationalRecipientAccountIdentificationStrategy {
   SwiftBIC,
   IBAN,
   NationalID
}

public enum RecipientAccountEnvironment {
   Internal,
   Domestic,
   International
}