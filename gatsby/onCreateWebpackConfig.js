const { resolve } = require("path")
const TsconfigPathsPlugin = require("tsconfig-paths-webpack-plugin")

const onCreateWebpackConfig = ({ loaders, actions }) => {
  const jsLoader = loaders.js()
  actions.setWebpackConfig({
    module: {
      rules: [
        {
          enforce: "pre",
          test: /\.js$|\.jsx$|\.ts$|\.tsx$/,
          loader: "eslint-loader",
          exclude: /(node_modules|.cache|public)/,
          options: {
            emitError: true,
            emitWarning: false,
            failOnError: true,
            configFile: ".eslintrc",
          },
        },
        {
          test: /\.tsx?$/,
          exclude: /node_modules/,
          use: [jsLoader, "ts-loader"],
        },
      ],
    },
    resolve: {
      plugins: [new TsconfigPathsPlugin()],
      modules: [
        resolve(process.cwd(), "src"),
        resolve(process.cwd(), "node_modules"),
      ],
    },
  })
}

module.exports = onCreateWebpackConfig
