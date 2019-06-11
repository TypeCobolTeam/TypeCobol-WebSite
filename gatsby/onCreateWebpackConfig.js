const { resolve } = require("path");

const onCreateWebpackConfig = ({ actions }) => {
  actions.setWebpackConfig({
    resolve: {
      modules: [
        resolve(__dirname, "../src"),
        resolve(__dirname, "../node_modules")
      ],
      alias: {
        "_variables.sass": resolve(__dirname, "../src/scss/_variables.sass")
      }
    }
  });
};

module.exports = onCreateWebpackConfig;
