// Adds slug field for URL in page creation
// Set URL with relativePath in /content folder

const onCreateNode = ({ node, getNode, actions }) => {
  const { createNodeField } = actions;
  if (node.internal.type == "MarkdownRemark") {
    const { relativePath } = getNode(node.parent);
    let slug = `/${relativePath.replace(".md", ".html")}`;

    createNodeField({
      node,
      name: "slug",
      value: slug
    });
  }

  return;
};

module.exports = onCreateNode;
