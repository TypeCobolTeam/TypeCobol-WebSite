const { resolve } = require("path");

const onCreateWebpackConfig = ({ actions }) => {
  actions.setWebpackConfig({
    resolve: {
      alias: {
        "@components": resolve(__dirname, "../src/components"),
        "@utils": resolve(__dirname, "../src/utils"),
        "@content": resolve(__dirname, "../content")
      },
      modules: [
        resolve(__dirname, "../src"),
        resolve(__dirname, "../node_modules")
      ]
    }
  });
};

module.exports = onCreateWebpackConfig;
