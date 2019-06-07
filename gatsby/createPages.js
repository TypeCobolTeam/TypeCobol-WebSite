"use strict";

const { resolve } = require("path");

// Creating Markdown pages automatically
module.exports = ({ graphql, actions }) => {
  const { createPage, createRedirect } = actions;
  const pageTemplate = resolve("src/templates/pageTemplate.js");
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
