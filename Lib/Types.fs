module Lib.Types

open System

type StreamThrottleEnvConfig = {
   Count: int
   Burst: int
   Duration: TimeSpan
}

type StreamChunkingEnvConfig = { Size: int; Duration: TimeSpan }

type BackoffSupervisorEnvConfig = {
   MinBackoff: TimeSpan
   MaxBackoff: TimeSpan
   RandomFactor: float
   MaxNrOfRetries: int
   ResetCounterAfter: TimeSpan
}

type PersistenceSupervisorEnvConfig = {
   MinBackoff: TimeSpan
   MaxBackoff: TimeSpan
   RandomFactor: float
   MaxNrOfRetries: int
}

type QueueConnectionEnvConfig = {
   Host: string
   Port: int
   VirtualHost: string
   Username: string
   Password: string
}

type QueueEnvConfig = { Name: string; MaxParallelism: int }
