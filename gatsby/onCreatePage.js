const fs = require("fs")
const yaml = require("js-yaml")

const onCreatePage = ({ page, actions }) => {
  const { createPage, deletePage, createRedirect } = actions
  if (page.path !== "/") return
  try {
    const langs = yaml.safeLoad(
      fs.readFileSync("content/i18n/languages.yml", "utf8")
    )
    // eslint-disable-next-line array-callback-return
    langs.map(elem => {
      const newPage = Object.assign({}, page)
      newPage.path = `/${elem.tag}/`
      newPage.context = {
        translation: elem.tag,
      }
      createPage(newPage)
    })
    const toPath = `/${langs[0].tag}/`
    createRedirect({
      fromPath: "/",
      toPath,
      isPermanent: true,
      redirectInBrowser: true,
    })
    // eslint-disable-next-line no-console
    console.log(`${"/"} --> ${toPath}`)
    deletePage(page)
  } catch (e) {
    throw e
  }
}

module.exports = onCreatePage
