import * as k8s from '@pulumi/kubernetes'

import { ports, isDev } from './environment'

export const initMockPartnerBankService = (
  provider: k8s.Provider
): k8s.core.v1.Service =>
  new k8s.core.v1.Service(
    'mockPartnerBank',
    {
      metadata: {
        name: 'mock-partner-bank'
      },
      spec: {
        ports: [
          {
            port: ports.mockPartnerBank,
            protocol: 'TCP',
            targetPort: ports.mockPartnerBank
          }
        ],
        selector: {
          app: 'mock-partner-bank'
        }
      }
    },
    { provider }
  )

export const initMockPartnerBankDeployment = (
  provider: k8s.Provider
): k8s.apps.v1.Deployment =>
  new k8s.apps.v1.Deployment(
    'mockPartnerBankDeployment',
    {
      metadata: {
        labels: {
          app: 'mock-partner-bank'
        },
        name: 'mock-partner-bank-deployment'
      },
      spec: {
        replicas: 1,
        selector: {
          matchLabels: {
            app: 'mock-partner-bank'
          }
        },
        template: {
          metadata: {
            labels: {
              app: 'mock-partner-bank'
            }
          },
          spec: {
            containers: [
              {
                env: [
                  {
                    name: 'TCP_BIND_PORT',
                    value: ports.mockPartnerBank.toString()
                  }
                ],
                image: isDev ? 'mock-partner-bank:latest' : 'danne931/akka-dotnet-bank-mock-partner-bank:latest',
                imagePullPolicy: isDev ? 'Never' : 'Always',
                livenessProbe: {
                  initialDelaySeconds: 10,
                  periodSeconds: 60,
                  tcpSocket: {
                    port: 'tcp-bind-port'
                  }
                },
                name: 'mock-partner-bank',
                ports: [
                  {
                    containerPort: ports.mockPartnerBank,
                    name: 'tcp-bind-port',
                    protocol: 'TCP'
                  }
                ]
              }
            ]
          }
        }
      }
    },
    { provider }
  )
