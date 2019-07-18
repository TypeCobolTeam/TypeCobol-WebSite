const { resolve } = require("path")
const eslint = require("eslint")
const TsconfigPathsPlugin = require("tsconfig-paths-webpack-plugin")
const tsImportPluginFactory = require("ts-import-plugin")

const onCreateWebpackConfig = ({ loaders, stage, actions }) => {
  const jsLoader = loaders.js()

  /**
   * Module Rules
   */
  const lessLoader = {
    loader: "less-loader",
    options: {
      modifyVars: {
        hack: `true; @import "${resolve(
          process.cwd(),
          "src/style/theme.less"
        )}";`,
      },
      javascriptEnabled: true,
    },
  }
  const lessRule = {
    test: /\.less$/,
    use: stage.includes(`html`)
      ? [loaders.null()]
      : [
          loaders.miniCssExtract(),
          loaders.css({ importLoaders: 1 }),
          loaders.postcss(), // use defaults for autoprefixer for configured browsers
          lessLoader, // Compiles Less to CSS and Javascript
        ],
  }

  const eslintLoader = {
    loader: "eslint-loader",
    options: {
      emitError: true,
      emitWarning: false,
      failOnError: true,
      configFile: ".eslintrc",
      formatter: eslint.CLIEngine.getFormatter("stylish"),
    },
  }
  const eslintRule = {
    enforce: "pre",
    test: /\.js$|\.jsx$|\.ts$|\.tsx$/,
    exclude: /(node_modules|.cache|public)/,
    use: [eslintLoader],
  }

  const tsLoader = {
    loader: "ts-loader",
    options: {
      transpileOnly: true,
      getCustomTransformers: () => ({
        before: [
          tsImportPluginFactory({
            libraryName: "antd",
            libraryDirectory: "es",
            style: true,
          }),
        ],
      }),
    },
  }
  const tsRule = {
    test: /\.tsx?$/,
    exclude: /node_modules/,
    use: [jsLoader, tsLoader],
  }

  /**
   * Set Config Object
   */
  const config = {
    module: {
      rules: [lessRule, eslintRule, tsRule],
    },
    resolve: {
      plugins: [new TsconfigPathsPlugin()],
      modules: [
        resolve(process.cwd(), "src"),
        resolve(process.cwd(), "node_modules"),
      ],
    },
  }

  /**
   * setWebpackConfig
   */
  actions.setWebpackConfig(config)
}

module.exports = onCreateWebpackConfig
