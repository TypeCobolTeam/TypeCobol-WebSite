const fs = require("fs")
const { safeLoad } = require("js-yaml")

const onCreatePage = ({ page, actions }) => {
  const { createPage, deletePage, createRedirect } = actions
  if (!["/", "/blog/"].includes(page.path)) return

  try {
    const langs = safeLoad(
      fs.readFileSync("content/i18n/languages.yml", "utf8")
    )
    langs.map(elem => {
      const newPage = Object.assign({}, page)
      newPage.path = `/${elem.tag}${page.path}`
      newPage.context = {
        translation: elem.tag,
      }
      createPage(newPage)
      return newPage
    })
    const toPath = `/${langs[0].tag}${page.path}`
    const fromPath = page.path
    createRedirect({
      fromPath,
      toPath,
      isPermanent: true,
      redirectInBrowser: true,
    })
    deletePage(page)
  } catch (e) {
    throw e
  }
}

module.exports = onCreatePage
