const createZip = require("./create-zip")

const onPostBuild = () => {
  if (process.env.ZIP) {
    createZip()
    // eslint-disable-next-line no-console
    console.log("Successfully created the archive")
  }
}

module.exports = onPostBuild
