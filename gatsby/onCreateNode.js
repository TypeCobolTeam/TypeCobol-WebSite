const { repository } = require("../package.json")

const langFileRegex = /i18n[/\\]([a-z]{2})[/\\].*/g

const selectTemplate = slug => {
  if (slug.includes("community/")) {
    return "src/templates/community/index.tsx"
  }
  if (slug.includes("docs/")) {
    return "src/templates/docs/index.tsx"
  }
  if (slug.includes("blog/")) {
    return "src/templates/blog/index.tsx"
  }
  return "src/templates/single/index.tsx"
}

const onCreateNode = ({ node, getNode, actions }) => {
  const { createNodeField } = actions
  if (node.internal.type === "MarkdownRemark") {
    const { relativePath } = getNode(node.parent) // "i18n/en/community/contribute.md"

    const template = selectTemplate(relativePath)

    const isTranslation = langFileRegex.test(relativePath)
    const translationCode = isTranslation
      ? relativePath.replace(langFileRegex, "$1").replace("/", "")
      : ""

    let slug = `/${relativePath.replace(".md", ".html")}`
    slug = slug.replace("/index.html", "")
    slug = slug.replace("/i18n", "")

    const gitLink = repository.replace(
      ".git",
      `/tree/develop/content/${relativePath}`
    )

    createNodeField({
      node,
      name: "slug",
      value: slug,
    })

    createNodeField({
      node,
      name: "translationCode",
      value: translationCode,
    })

    createNodeField({
      node,
      name: "template",
      value: template,
    })

    createNodeField({
      node,
      name: "gitLink",
      value: gitLink,
    })
  }
}

module.exports = onCreateNode
