const fs = require("fs")
const path = require("path")
const os = require("os")
const archiver = require("archiver")
const archive = archiver("zip", {
  zlib: { level: 0 },
})

const sanitizeReg = /(((style|srcset|role|sizes|id|class)="[^"]*")|(<style(.|\n)*?<\/style>)|(<aside(.|\n)*?<\/aside>)|(<header(.|\n)*?<\/header>)|(tc-[^(\s|")]*)|((class|id)="(.|\n)*?")|(\s+?(?=(\s|>)))|<\/?div>|<div(.|\n)*?>|<\/?section>|<section(.|\n)*?>|<\/?main>|<main(.|\n)*?>)/gm

const zipFile = path.join(process.cwd(), "static", "HTMLDoc.zip")
const inputDocFolder = path.join(process.cwd(), "public", "docs")
const inputStaticFolder = path.join(process.cwd(), "public", "static")
const date = new Date()
const tempZipFolder = path.join(
  os.tmpdir(),
  `typecobol-web-tmp-zip---${date.getTime().toString()}`
)

const copyAndSanitizeIfHTML = (src, dest) => {
  const exists = fs.existsSync(src)
  const stats = exists && fs.statSync(src)
  const isDirectory = exists && stats.isDirectory()
  if (exists && isDirectory) {
    fs.mkdirSync(dest, { recursive: true })
    fs.readdirSync(src).forEach(childItemName => {
      copyAndSanitizeIfHTML(
        path.join(src, childItemName),
        path.join(dest, childItemName)
      )
    })
  } else if (path.extname(src) === ".html") {
    let content = fs.readFileSync(src, "utf8")
    content = content.replace(sanitizeReg, "")
    fs.writeFile(dest, content, err => {
      if (err) throw err
    })
  } else {
    fs.linkSync(src, dest)
  }
}

const createZip = () => {
  // Run Copy to tmp folder
  copyAndSanitizeIfHTML(inputDocFolder, tempZipFolder)
  copyAndSanitizeIfHTML(inputStaticFolder, path.join(tempZipFolder, "static"))

  // Create Archive
  fs.mkdirSync(path.join(process.cwd(), "static"), { recursive: true })
  const output = fs.createWriteStream(zipFile)
  archive.pipe(output)
  archive.directory(tempZipFolder, false)
  archive.finalize()
}

module.exports = createZip
