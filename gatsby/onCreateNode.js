// Adds slug field for URL in page creation
// Set URL with relativePath in /content folder

const langFileRegex = /i18n[/\\]([a-z]{2})[/\\].*/g

// select templates according to slug (=path) generated.
const selectTemplate = slug => {
  if (slug.includes("community")) {
    return "src/templates/community/index.tsx"
  }
  if (slug.includes("docs")) {
    return "src/templates/docs/index.tsx"
  }
  return "src/templates/single/index.tsx"
}

const onCreateNode = ({ node, getNode, actions }) => {
  const { createNodeField } = actions
  if (node.internal.type === "MarkdownRemark") {
    const { relativePath } = getNode(node.parent)

    const template = selectTemplate(relativePath)

    const isTranslation = langFileRegex.test(relativePath)
    const translationCode = isTranslation
      ? relativePath.replace(langFileRegex, "$1").replace("/", "")
      : ""

    let slug = `/${relativePath.replace(".md", ".html")}`
    slug = slug.replace("/index.html", "")
    slug = slug.replace("/i18n", "")

    const isFourOFour = slug.includes("404.html")

    createNodeField({
      node,
      name: "slug",
      value: slug,
    })

    createNodeField({
      node,
      name: "isFourOFour",
      value: isFourOFour,
    })

    createNodeField({
      node,
      name: "isTranslation",
      value: isTranslation,
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
  }
}

module.exports = onCreateNode
