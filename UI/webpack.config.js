const HtmlWebpackPlugin = require('html-webpack-plugin')
const Dotenv = require('dotenv-webpack')
const path = require('path')

const staticPath = path.resolve(__dirname, './dist')

module.exports = (env, argv) => {
  const mode = argv.mode
  console.log(`Webpack building in ${mode} mode`)
  const proxyUrl = 'http://localhost:3000'

  return {
    mode: mode,
    entry: './dist/App.js',
    output: {
      path: staticPath,
      filename: '[name].js',
      chunkFilename: '[name].[contenthash].js'
    },

    devServer: {
      proxy: {
        '/api': proxyUrl,
        '/login': proxyUrl,
        '/session': proxyUrl,
        '/bankHub': {
          target: 'ws://localhost:3000',
          ws: true
        }
      },
      static: staticPath
    },

    plugins: [
      new Dotenv(),
      new HtmlWebpackPlugin({
        title: 'Phoenix Banking',
        template: './src/index.html',
        meta: {
          viewport: 'width=device-width, initial-scale=1'
        }
      })
    ],

    externalsType: 'script',
    externals: {
      LeaderLine: ['https://cdnjs.cloudflare.com/ajax/libs/leader-line/1.0.7/leader-line.min.js', 'LeaderLine']
    },

    module: {
      rules: [
        {
          test: /\.sass$/i,
          use: ['style-loader', 'css-loader', 'sass-loader']
        },
        // NOTE: CSS needs to be parsed for some 3rd party libs
        {
          test: /\.css$/i,
          use: ['style-loader', 'css-loader']
        },
        {
          test: /\.svg$/i,
          type: 'asset/resource'
        }
      ]
    },

    optimization: {
      splitChunks: {
        cacheGroups: {
          vendor: {
            test: /[\\/]node_modules|dist[\\/]fable_modules[\\/]/,
            chunks: 'all'
          }
        }
      }
    }
  }
}
