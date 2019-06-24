const { resolve } = require("path")

const createPage = async ({ graphql, actions }) => {
  // eslint-disable-next-line no-shadow
  const { createPage, createRedirect } = actions

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
                redirectFrom
              }
            }
          }
        }
      }
    `
  )

  // select templates according to slug (=path) generated @ /gatsby/onCreateNode.ts
  const selectTemplate = slug => {
    if (slug.includes("community/")) {
      return resolve(`src/templates/community/index.tsx`)
    }
    if (slug.includes("docs/")) {
      return resolve(`src/templates/docs/index.tsx`)
    }
    return resolve(`src/templates/single/index.tsx`)
  }

  allMarkdownRemark.data.allMarkdownRemark.edges.forEach(edge => {
    const { slug } = edge.node.fields
    const { id, frontmatter } = edge.node
    const { disablePage } = frontmatter

    // avoir having two vars with same name
    const redirectHereFrom = frontmatter.redirectFrom

    if (!slug || disablePage) return

    const template = selectTemplate(slug)

    createPage({
      path: slug,
      component: template,
      context: {
        slug,
        id,
      },
    })

    // Redirect from /category/page to /category/page.html
    let redirectFrom = slug.replace(".html", "")
    createRedirect({
      fromPath: redirectFrom,
      isPermanent: true,
      redirectInBrowser: true,
      toPath: slug,
    })

    // add redirections for index pages
    if (slug.includes("index")) {
      redirectFrom = slug.replace("index.html", "")
      createRedirect({
        fromPath: redirectFrom,
        isPermanent: true,
        redirectInBrowser: true,
        toPath: slug,
      })

      redirectFrom = redirectFrom.replace(/\/$/, "")
      createRedirect({
        fromPath: redirectFrom,
        isPermanent: true,
        redirectInBrowser: true,
        toPath: slug,
      })
    }

    // allow a redirect_form meta tag in pages
    redirectFrom = redirectHereFrom
    if (!redirectFrom) return
    if (typeof redirectFrom === "string") {
      createRedirect({
        fromPath: redirectFrom,
        toPath: slug,
        isPermanent: true,
        redirectInBrowser: true,
      })
    } else if (
      typeof redirectFrom === "object" &&
      redirectFrom !== null &&
      redirectFrom !== ""
    ) {
      redirectFrom.forEach(from => {
        createRedirect({
          fromPath: from,
          toPath: slug,
          isPermanent: true,
          redirectInBrowser: true,
        })
      })
    }
  })
}

module.exports = createPage
