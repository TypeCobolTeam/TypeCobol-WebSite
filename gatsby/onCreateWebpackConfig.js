const { resolve } = require("path")
const TsconfigPathsPlugin = require("tsconfig-paths-webpack-plugin")
const CleanCSSPlugin = require("less-plugin-clean-css")

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
        {
          test: /\.less$/,
          use: [
            {
              loader: "style-loader",
            },
            {
              loader: "css-loader", // translates CSS into CommonJS
            },
            {
              loader: "less-loader", // compiles Less to CSS
              options: {
                plugins: [new CleanCSSPlugin({ advanced: true })],
                modifyVars: {
                  hack: `true; @import "${resolve(
                    process.cwd(),
                    "src/style/theme.less"
                  )}";`,
                },
                javascriptEnabled: true,
              },
            },
          ],
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
