import * as pulumi from '@pulumi/pulumi'
import * as k8s from '@pulumi/kubernetes'

import { config as pgConfig } from './postgres'
import { config as rmqConfig } from './rabbitmq'

// Grab some values from the Pulumi stack configuration (or use defaults)
const config = new pulumi.Config()

export const isDev = config.require('environment') === 'Development'
export const isStaging = config.require('environment') === 'Staging'
export const isProduction = config.require('environment') === 'Production'

export const ports = {
  akkaRemoting: config.getNumber('akkaRemotingPort') ?? 8082,
  akkaManagement: config.getNumber('akkaManagementPort') ?? 8558,
  akkaHealthCheckReadiness:
    config.getNumber('akkaHealthCheckReadinessPort') ?? 9998,
  akkaHealthCheckLiveness:
    config.getNumber('akkaHealthCheckLivenessPort') ?? 9999,
  petabridgeCmd: config.getNumber('petabridgeCmdPort') ?? 9110,
  mockPartnerBank: config.getNumber('mockPartnerBankTcpBindPort') ?? 5007,
  postgres: config.getNumber('postgresPort') ?? 5432,
  webHttp: config.getNumber('webHttp') ?? 80
}

const defaultNamespace = config.require('defaultK8Namespace')

export const initContainers = {
  postgresReady: {
    command: [
      'sh',
      '-c',
      `until nc -z ${pgConfig.k8ResourceName}-postgres.${defaultNamespace}.svc.cluster.local ${ports.postgres};` +
      'do echo "Waiting for postgres..."; sleep 3; done;\n'
    ],
    image: 'busybox:1.28',
    name: 'init-postgres'
  },
  rabbitmqReady: {
    command: [
      'sh',
      '-c',
      `until nc -z ${rmqConfig.k8ResourceName}.${defaultNamespace}.svc.cluster.local 5672;` +
      'do echo "Waiting for rabbitmq..."; sleep 3; done;\n'
    ],
    image: 'busybox:1.28',
    name: 'init-rabbitmq'
  },
  accountServiceReady: {
    command: [
      'sh',
      '-c',
      `until nc -zv account-service.${defaultNamespace}.svc.cluster.local ${ports.akkaHealthCheckReadiness};` +
      'do echo waiting for account cluster formation; sleep 3; done'
    ],
    image: 'busybox:1.28',
    name: 'wait-for-seed-nodes-in-cluster-formation'
  },
  readModelsReady: {
    command: [
      'sh',
      '-c',
      `until psql --host ${pgConfig.k8ResourceName}-postgres.${defaultNamespace}.svc.cluster.local` +
      ` -d ${pgConfig.database} -U ${pgConfig.user} -p ${ports.postgres} -c "select 1 from balance_history";` +
      ' do echo waiting for data to be seeded; sleep 3; done\n'
    ],
    env: [
      {
        name: 'PGPASSWORD',
        value: pgConfig.password
      }
    ],
    image: 'postgres:16.0',
    name: 'wait-for-seed-data-initialized'
  }

}

// ConfigMap for environment config relevant across all resources
export const initBankEnvConfigMap = (
  provider: k8s.Provider
): k8s.core.v1.ConfigMap =>
  new k8s.core.v1.ConfigMap(
    'bank-env',
    {
      metadata: {
        name: 'bank-env'
      },
      data: {
        ASPNETCORE_ENVIRONMENT: config.require('environment'),
        AkkaRemoting__Port: ports.akkaRemoting.toString(),

        ClusterStartupMethod: 'DiscoveryKubernetes',
        ClusterDiscoveryKubernetesStartup__PodLabelSelector: 'cluster=akkabank',
        ClusterDiscoveryKubernetesStartup__PortName: 'akka-management',
        ClusterDiscoveryKubernetesStartup__RequiredContactPointsNr: (
          config.getNumber('akkaClusterDiscoveryRequiredContactPoints') ?? 2
        ).toString(),

        AkkaHealthCheck__ReadinessPort:
          ports.akkaHealthCheckReadiness.toString(),
        AkkaHealthCheck__LivenessPort: ports.akkaHealthCheckLiveness.toString(),
        PetabridgeCmdRemoting__Port: ports.petabridgeCmd.toString(),

        ConnectionStrings__Postgres: pgConfig.password.apply(
          (pwd) =>
            `postgres://${pgConfig.user}:${pwd}@${pgConfig.k8ResourceName}-postgres.${defaultNamespace}.svc.cluster.local/${pgConfig.database}`
        ),
        ConnectionStrings__PostgresAdoFormat: pgConfig.password.apply(
          (pwd) =>
            `Server=${pgConfig.k8ResourceName}-postgres.${defaultNamespace}.svc.cluster.local;Database=${pgConfig.database};Uid=${pgConfig.user};Pwd=${pwd}`
        ),

        QueueConnection__Host: `${rmqConfig.k8ResourceName}.${defaultNamespace}.svc.cluster.local`,
        QueueConnection__Port: '5672',
        QueueConnection__Username: rmqConfig.user,
        QueueConnection__Password: rmqConfig.password,
        QueueConnection__VirtualHost: '/',
        MockSendingEmail: isProduction ? 'false' : config.get('mockSendingEmail') ?? 'true'
      }
    },
    { provider }
  )
