module Lib.Types

open System

type StreamThrottle = {
   Count: int
   Burst: int
   Duration: TimeSpan
}

type StreamChunking = { Size: int; Duration: TimeSpan }

type BackoffSupervisorOptions = {
   MinBackoff: TimeSpan
   MaxBackoff: TimeSpan
   RandomFactor: float
   MaxNrOfRetries: int
   ResetCounterAfter: TimeSpan
}

type PersistenceSupervisorOptions = {
   MinBackoff: TimeSpan
   MaxBackoff: TimeSpan
   RandomFactor: float
   MaxNrOfRetries: int
}

type QueueConnectionSettings = {
   Host: string
   Port: int
   VirtualHost: string
   Username: string
   Password: string
}

type QueueSettings = { Name: string; MaxParallelism: int }
