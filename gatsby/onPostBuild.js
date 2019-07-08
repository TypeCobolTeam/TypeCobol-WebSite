const { resolve } = require("path")
const createZip = require("./create-zip")

const onPostBuild = () => {
  if (process.env.ZIP) {
    createZip()
    console.log("Successfully created the archive")
  }
}

module.exports = onPostBuild
