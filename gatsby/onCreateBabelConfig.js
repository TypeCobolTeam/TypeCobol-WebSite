const onCreateBabelConfig = ({ actions }) => {
  const { setBabelPlugin } = actions
  setBabelPlugin({
    name: `babel-plugin-import`,
    options: {
      libraryName: "antd",
      style: true,
    },
  })
}

module.exports = onCreateBabelConfig
