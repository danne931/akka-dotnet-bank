namespace Lib.Types;

public abstract record Command(Guid EntityId) {
   public DateTime Timestamp { get; init; } = DateTime.UtcNow;
};

public abstract record Event(
   Guid EntityId,
   DateTime Timestamp,
   string Name,
   float Version = 1.0F
);