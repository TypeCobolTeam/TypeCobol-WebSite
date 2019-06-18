const { resolve } = require("path")

const onCreateWebpackConfig = ({ actions }) => {
  actions.setWebpackConfig({
    resolve: {
      alias: {
        "@components": resolve(process.cwd(), "src/components"),
        "@utils": resolve(process.cwd(), "src/utils"),
        "@content": resolve(process.cwd(), "content"),
      },
      modules: [
        resolve(process.cwd(), "src"),
        resolve(process.cwd(), "node_modules"),
      ],
    },
  })
}

module.exports = onCreateWebpackConfig
