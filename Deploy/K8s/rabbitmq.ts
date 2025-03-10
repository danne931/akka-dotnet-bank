import * as pulumi from '@pulumi/pulumi'
import * as k8s from '@pulumi/kubernetes'

// Pull config from Pulumi ESC "staging" environment.
const configStore = new pulumi.Config()
export const config = {
  user: configStore.require('rabbitmqUser'),
  password: configStore.requireSecret('rabbitmqPassword'),
  k8ResourceName: configStore.get('rabbitmqK8ResourceName') ?? 'rabbitmq'
}

// RabbitMQ Helm Chart
export default function initRabbitMq(
  provider: k8s.Provider,
  namespace: k8s.core.v1.Namespace
): k8s.helm.v3.Chart {
  return new k8s.helm.v3.Chart(
    config.k8ResourceName,
    {
      fetchOpts: {
        repo: 'https://charts.bitnami.com/bitnami'
      },

      chart: 'rabbitmq',
      version: '14.7.0',

      namespace: namespace.metadata.name,

      values: {
        auth: {
          username: config.user,
          password: config.password
        },

        // Management Web UI
        plugins: 'rabbitmq_management',

        persistence: {
          enabled: true
        },

        service: {
          type: 'ClusterIP',
          ports: {
            amqp: 5672,
            management: 15672
          }
        }
      }
    },
    { provider }
  )
}
