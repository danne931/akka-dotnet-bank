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
    },

    // TODO:
    // I am receiving 'Compiled with problems: Deprecation The legacy JS API is deprecated and will be removed in Dart Sass 2.0.0.' 
    // warnings while running app with webpack dev server via 'npm start'.  I converted the deprecated 'import' statements
    // to 'use' but still have 1 remaining warning to track down.
    // Will use 'ignoreWarnings' here to ignore it for now.
    ignoreWarnings: [
      {
        module: /sass-loader/,
        message: /The legacy JS API is deprecated/
      }
    ]
  }
}
