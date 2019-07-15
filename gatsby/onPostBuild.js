/* eslint-disable consistent-return */
const os = require("os")

const path = require("path")
const createZip = require("./create-zip")

const onPostBuild = () => {
  if (!process.env.ZIP) return
  const date = new Date()

  const sanitizeReg = /(((style|srcset|role|sizes|id|class)="[^"]*")|(<style(.|\n)*?<\/style>)|(<noscript(.|\n)*?<\/noscript>)|(<footer(.|\n)*?<\/footer>)|(<aside(.|\n)*?<\/aside>)|(<header(.|\n)*?<\/header>)|(tc-[^(\s|")]*)|((class|id)="(.|\n)*?")|(\s+?(?=(\s|>)))|<\/?div>|<div(.|\n)*?>|<\/?section>|<section(.|\n)*?>|<\/?main>|<main(.|\n)*?>)/gm

  const tempFolder = path.join(
    os.tmpdir(),
    `typecobol-web-tmp-zip---${date.getTime().toString()}`
  )

  const settings = {
    sanitizeReg,
    zipFilename: "HTMLDoc.zip",
    tempFolder,
    fileDirectives: [
      {
        input: path.join(process.cwd(), "public", "en", "docs"),
        output: path.join("en"),
      },
      {
        input: path.join(process.cwd(), "public", "fr", "docs"),
        output: path.join("fr"),
      },
      {
        input: path.join(process.cwd(), "public", "static"),
        output: path.join("static"),
      },
    ],
  }
  return createZip(settings)
}

module.exports = onPostBuild
