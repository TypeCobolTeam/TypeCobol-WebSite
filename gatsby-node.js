const path = require("path");

// Creating Markdown pages automatically
exports.createPages = ctx => {
  const { actions, graphql } = ctx;
  const { createPage } = actions;
  const pageTemplate = path.resolve("src/templates/pageTemplate.js");
  return graphql(`
    {
      allMarkdownRemark(filter: { frontmatter: { type: { eq: "page" } } }) {
        edges {
          node {
            fileAbsolutePath
            id
          }
        }
      }
    }
  `).then(result => {
    if (result.errors) return Promise.reject(result.errors);
    result.data.allMarkdownRemark.edges.forEach(data => {
      let path = data.node.fileAbsolutePath
        .split("/pages/")[1]
        .replace(/\.[^/.]+$/, "");
      createPage({
        path: path,
        component: pageTemplate,
        context: {
          pageID: data.node.id
        }
      });
    });
  });
};
exports.onCreateWebpackConfig = ({
  stage,
  getConfig,
  rules,
  loaders,
  actions
}) => {
  actions.setWebpackConfig({
    resolve: {
      alias: {
        "_variables.sass": path.resolve(__dirname, "src/style/_variables.sass")
      }
    }
  });
};
