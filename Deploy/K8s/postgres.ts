import * as pulumi from '@pulumi/pulumi'
import * as k8s from '@pulumi/kubernetes'
import * as fs from 'fs'
import * as path from 'path'

// Pull config from Pulumi ESC "staging" environment.
const configStore = new pulumi.Config()
export const config = {
  database: configStore.require('postgresDatabase'),
  user: configStore.require('postgresUser'),
  password: configStore.requireSecret('postgresPassword'),
  k8ResourceName: configStore.get('postgresK8ResourceName') ?? 'pg'
}

// Postgres Helm Chart
export default function initPostgres (
  provider: k8s.Provider,
  namespace: k8s.core.v1.Namespace
): k8s.helm.v4.Chart {
  const schemaConfigMap = initSchemaConfigMap(provider)

  return new k8s.helm.v4.Chart(
    config.k8ResourceName,
    {
      // https://hub.docker.com/r/bitnamicharts/postgresql
      chart: 'oci://registry-1.docker.io/bitnamicharts/postgresql',
      version: '18.1.13',

      namespace: namespace.metadata.name,

      values: {
        global: {
          postgresql: {
            auth: {
              username: config.user,
              password: config.password
            }
          }
        },

        auth: {
          database: config.database
        },

        primary: {
          initdb: {
            // postgres-schemas configmap containing SQL files
            // from Database/Init directory.
            scriptsConfigMap: schemaConfigMap.metadata.name
          }
        }
      }
    },
    { provider }
  )
}

function initSchemaConfigMap (provider: k8s.Provider): k8s.core.v1.ConfigMap {
  const sqlDir = '../../Database/Init'
  const sqlFileData: { [key: string]: string } = {}

  fs.readdirSync(sqlDir).forEach((file) => {
    if (path.extname(file) === '.sql') {
      const filePath = path.join(sqlDir, file)
      const fileContent = fs.readFileSync(filePath, 'utf8')
      sqlFileData[file] = fileContent
    }
  })

  return new k8s.core.v1.ConfigMap(
    'postgres-schemas',
    {
      metadata: {
        name: 'postgres-schemas'
      },
      data: sqlFileData
    },
    { provider }
  )
}
