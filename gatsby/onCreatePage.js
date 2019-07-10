const fs = require("fs")
const yaml = require("js-yaml")

const onCreatePage = ({ page, actions }) => {
  if (page.path !== "/") return
  try {
    const langs = yaml.safeLoad(
      fs.readFileSync("content/i18n/languages.yml", "utf8")
    )
    const { createPage, deletePage } = actions
    // eslint-disable-next-line array-callback-return
    langs.map(elem => {
      const newPage = Object.assign({}, page)
      newPage.path = `/${elem.tag}/`
      newPage.context = {
        translation: elem.tag,
      }
      createPage(newPage)
    })
    deletePage(page)
  } catch (e) {
    throw e
  }
}

module.exports = onCreatePage
