using EventStore.Client;
using System.Text.Json;
using LanguageExt;
using static LanguageExt.Prelude;

using System.Collections.Immutable;

using Account.Domain.Events;

namespace Lib.Persistence;

public static class EventStoreManager {
   public async static Task<Unit> SaveAndPublish(
      EventStoreClient es,
      ImmutableDictionary<string, Type> mapping,
      string streamName,
      Event evt,
      StreamState? expectedStreamState = null
   ) {
      var typeFromName = mapping[evt.Name];

      var eventData = new EventData(
         eventId: Uuid.NewUuid(),
         type: evt.Name,
         data: JsonSerializer.SerializeToUtf8Bytes(evt, typeFromName)
      );

      await es.AppendToStreamAsync(
         streamName,
         expectedStreamState ?? StreamState.StreamExists,
         new[] { eventData }
      );

      return unit;
   }

   public static async Task<Option<Lst<object>>> ReadStream(
   //public static async TryOption<Lst<object>> ReadStream(
      EventStoreClient es,
      string streamName,
      ImmutableDictionary<string, Type> mapping
   ) {
      var stream = es.ReadStreamAsync(
         Direction.Forwards,
         streamName,
         StreamPosition.Start
      );
      if (await stream.ReadState == ReadState.StreamNotFound)
         return None;

      return await stream.AggregateAsync(
         List<object>(),
         (acc, e) =>
            acc.Add(JsonSerializer.Deserialize(
               e.Event.Data.ToArray(),
               mapping[e.Event.EventType]
            ))
         );
   }

   public static EventStoreClient Connect() => new EventStoreClient(
      EventStoreClientSettings.Create("esdb://127.0.0.1:2113?tls=false&keepAliveTimeout=10000&keepAliveInterval=10000"));
}
