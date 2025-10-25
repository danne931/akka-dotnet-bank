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
): k8s.helm.v4.Chart {
  return new k8s.helm.v4.Chart(
    config.k8ResourceName,
    {
      chart: 'oci://registry-1.docker.io/cloudpirates/rabbitmq',
      version: '0.5.5',

      namespace: namespace.metadata.name,

      values: {
        auth: {
          username: config.user,
          password: config.password,
          erlangCookie: "secretz"
        },

        // Management Web UI
        managementPlugin: {
          enabled: true
        },

        persistence: {
          enabled: true
        },

        service: {
          type: 'ClusterIP',
          amqpPort: 5672,
          managementPort: 15672
        }
      }
    },
    { provider }
  )
}
