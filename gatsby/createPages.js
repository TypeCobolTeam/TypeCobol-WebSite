const { resolve } = require("path");

const createPage = async ({ graphql, actions }) => {
  const { createPage, createRedirect } = actions;

  // query all content from /content/
  const allMarkdownRemark = await graphql(
    `
      {
        allMarkdownRemark(limit: 1000) {
          edges {
            node {
              id
              fields {
                slug
              }
              frontmatter {
                disablePage
              }
            }
          }
        }
      }
    `
  );

  // select templates according to slug (=path) generated @ /gatsby/onCreateNode.ts
  const selectTemplate = slug => {
    if (slug.includes("community/")) {
      return resolve(`src/templates/community/index.tsx`);
    } else if (slug.includes("tutorial/")) {
      return resolve(`src/templates/tutorial/index.tsx`);
    }
    return resolve(`src/templates/single/index.tsx`);
  };

  allMarkdownRemark.data.allMarkdownRemark.edges.forEach(edge => {
    const { slug } = edge.node.fields;
    const { id } = edge.node;
    const { disablePage } = edge.node.frontmatter;

    if (!slug || disablePage) return;

    const template = selectTemplate(slug);

    createPage({
      path: slug,
      component: template,
      context: {
        slug: slug,
        id: id
      }
    });

    // Redirect from /category/page to /category/page.html
    let redirect_from = slug.replace(".html", "");
    createRedirect({
      fromPath: redirect_from,
      isPermanent: true,
      redirectInBrowser: true,
      toPath: slug
    });

    // add redirections for index pages
    if (slug.includes("index")) {
      let redirect_from = slug.replace("index.html", "");
      createRedirect({
        fromPath: redirect_from,
        isPermanent: true,
        redirectInBrowser: true,
        toPath: slug
      });

      redirect_from = redirect_from.replace(/\/$/, "");
      createRedirect({
        fromPath: redirect_from,
        isPermanent: true,
        redirectInBrowser: true,
        toPath: slug
      });
    }
  });

  return;
};

module.exports = createPage;
