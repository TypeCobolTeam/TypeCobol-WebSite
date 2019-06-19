// Adds slug field for URL in page creation
// Set URL with relativePath in /content folder

const onCreateNode = ({ node, getNode, actions }) => {
  const { createNodeField } = actions
  if (node.internal.type === "MarkdownRemark") {
    const { relativePath } = getNode(node.parent)
    const slug = `/${relativePath.replace(".md", ".html")}`

    createNodeField({
      node,
      name: "slug",
      value: slug,
    })
  }
}

module.exports = onCreateNode
