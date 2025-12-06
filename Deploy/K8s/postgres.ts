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
      // https://artifacthub.io/packages/helm/cloudpirates-postgres/postgres
      chart: 'oci://registry-1.docker.io/cloudpirates/postgres',
      version: '0.12.4',

      namespace: namespace.metadata.name,

      values: {
        auth: {
          database: config.database,
          username: config.user,
          password: config.password
        },

        initdb: {
          // postgres-schemas configmap containing SQL files
          // from Database/Init directory.
          scriptsConfigMap: schemaConfigMap.metadata.name
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
