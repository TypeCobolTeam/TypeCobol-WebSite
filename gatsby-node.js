exports.onCreateWebpackConfig = require("./gatsby/onCreateWebpackConfig")
exports.createPages = require("./gatsby/createPages")
exports.onCreateNode = require("./gatsby/onCreateNode")
exports.resolvableExtensions = () => [
  ".ts",
  ".tsx",
  ".js",
  ".jsx",
  ".yml",
  ".yaml",
  ".png",
  ".jpg",
  ".svg",
]
