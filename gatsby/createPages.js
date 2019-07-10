const { resolve } = require("path")

const defaultlangKey = "en"

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
                isTranslation
                translationCode
                template
                isFourOFour
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

  allMarkdownRemark.data.allMarkdownRemark.edges.forEach(edge => {
    const {
      slug,
      isTranslation,
      translationCode,
      template,
      isFourOFour,
    } = edge.node.fields
    const { id, frontmatter } = edge.node
    const { disablePage } = frontmatter

    const redirectHereFrom = frontmatter.redirectFrom

    if (!slug || disablePage || template === "") return

    let FourOFourOptions = {}
    if (isFourOFour && isTranslation) {
      FourOFourOptions = {
        matchPath: `/${defaultlangKey}/*`,
      }
    }

    createPage({
      path: slug,
      component: resolve(template),
      context: {
        slug,
        id,
        translation: isTranslation ? translationCode : defaultlangKey,
      },
      ...FourOFourOptions,
    })

    if (translationCode === defaultlangKey) {
      const fromPath = slug.replace(`/${translationCode}`, "")
      createRedirect({
        fromPath,
        toPath: slug,
        isPermanent: true,
        redirectInBrowser: true,
      })
    }

    let fromPath = slug.replace(".html", "")
    if (fromPath !== slug) {
      createRedirect({
        fromPath,
        toPath: slug,
        isPermanent: true,
        redirectInBrowser: true,
      })
    }

    // allow a redirect_form meta tag in pages
    fromPath = redirectHereFrom
    if (!fromPath) return
    if (typeof fromPath === "object" && fromPath !== null && fromPath !== "") {
      fromPath.forEach(from => {
        createRedirect({
          fromPath: from,
          toPath: slug,
          isPermanent: true,
          redirectInBrowser: true,
        })
      })
    }
    fromPath = "/"
    createRedirect({
      fromPath,
      toPath: `/${defaultlangKey}/`,
      isPermanent: true,
      redirectInBrowser: true,
    })
  })
}

module.exports = createPage
