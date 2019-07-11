const fs = require("fs")
const path = require("path")
const archiver = require("archiver")
const archive = archiver("zip", {
  zlib: { level: 0 },
})

const copyAndSanitizeIfHTML = (src, dest, sanitizeReg) => {
  const exists = fs.existsSync(src)
  const stats = exists && fs.statSync(src)
  const isDirectory = exists && stats.isDirectory()
  if (exists && isDirectory) {
    fs.mkdirSync(dest, {
      recursive: true,
    })
    fs.readdirSync(src).forEach(childItem => {
      copyAndSanitizeIfHTML(
        path.join(src, childItem),
        path.join(dest, childItem),
        sanitizeReg
      )
    })
  } else if (path.extname(src) === ".html") {
    let content = fs.readFileSync(src, "utf8")
    content = content.replace(sanitizeReg, "")
    fs.writeFileSync(dest, content)
  } else {
    fs.linkSync(src, dest)
  }
}

const createZip = settings => {
  return new Promise((resolve, reject) => {
    const { sanitizeReg, zipFilename, tempFolder, fileDirectives } = settings

    fileDirectives.forEach(elem => {
      const output = path.join(tempFolder, elem.output)
      copyAndSanitizeIfHTML(elem.input, output, sanitizeReg)
      return output
    })

    // Create Archive
    fs.mkdirSync(path.join(process.cwd(), "static"), { recursive: true })
    const archiveOutput = fs.createWriteStream(
      path.join(process.cwd(), "static", zipFilename)
    )

    archive
      .directory(tempFolder, false)
      .on("error", err => reject(err))
      .pipe(archiveOutput)

    archiveOutput.on("close", () => resolve(archiveOutput.path))
    archive.finalize()
  })
}

module.exports = createZip
