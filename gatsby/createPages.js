const { resolve } = require("path")
const fs = require("fs")
const { safeLoad } = require("js-yaml")
const langs = safeLoad(fs.readFileSync("content/i18n/languages.yml", "utf8"))
const defaultlangKey = langs[0].tag

const createPage = async ({ graphql, actions }) => {
  const { createPage, createRedirect } = actions // eslint-disable-line no-shadow

  const allMarkdownRemark = await graphql(`
    {
      allMarkdownRemark(limit: 1000) {
        edges {
          node {
            id
            fields {
              slug
              translationCode
              template
            }
            frontmatter {
              disable
              redirectFrom
            }
          }
        }
      }
    }
  `)

  allMarkdownRemark.data.allMarkdownRemark.edges.forEach(edge => {
    const { slug, translationCode, template } = edge.node.fields
    const { id, frontmatter } = edge.node
    const { disable } = frontmatter

    if (!slug || disable || template === "") return

    const redirectHereFrom = frontmatter.redirectFrom

    createPage({
      path: slug,
      component: resolve(template),
      context: {
        id,
        translation: translationCode !== "" ? translationCode : defaultlangKey,
      },
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
  })
}

module.exports = createPage
