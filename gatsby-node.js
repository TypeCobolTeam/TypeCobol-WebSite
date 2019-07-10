exports.onCreateWebpackConfig = require("./gatsby/onCreateWebpackConfig")
exports.createPages = require("./gatsby/createPages")
exports.onCreateNode = require("./gatsby/onCreateNode")
exports.onPostBuild = require("./gatsby/onPostBuild")
exports.onCreatePage = require("./gatsby/onCreatePage")
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
