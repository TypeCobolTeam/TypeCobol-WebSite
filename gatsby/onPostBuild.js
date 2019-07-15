/* eslint-disable no-extend-native */
/* eslint-disable consistent-return */
const os = require("os")

const path = require("path")
const createZip = require("./createZip")

RegExp.prototype.a = function a(re) {
  return new RegExp(`(${this.source})|(${re.source})`, this.flags)
}

const onPostBuild = () => {
  if (!process.env.ZIP) return
  const date = new Date()

  const sanitizeReg = /\s+?(?=(\s|>))/gm // remove useless spaces
    .a(/(style|srcset|role|sizes|id|class|data-react-helmet)="[^"]*"/) // remove attribute from html tags
    .a(/<style(.|\n)*?<\/style>/) // remove style tag
    .a(/<noscript(.|\n)*?<\/noscript>/) // remove noscript tag
    .a(/<footer(.|\n)*?<\/footer>/) // remove footer tag
    .a(/<aside(.|\n)*?<\/aside>/) // remove aside tag
    .a(/<header(.|\n)*?<\/header>/) // remove header tag
    .a(/<div(.|\n)*?>|<\/?div>/) // remove div tag
    .a(/<section(.|\n)*?>|<\/?section>/) // remove section tag
    .a(/<main(.|\n)*?>|<\/?main>/) // remove main tag
    .a(/<meta name="generator" (.|\n)*?>/) // remove generator tag
  // .a(/(?<=")\/(?=static\/)/) // we're using static located in the current folder (to enable only if asked)

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
        output: path.join("en-en"),
      },
      {
        input: path.join(process.cwd(), "public", "static"),
        output: path.join("en-en", "static"),
      },
      {
        input: path.join(process.cwd(), "public", "fr", "docs"),
        output: path.join("fr-fr"),
      },
      {
        input: path.join(process.cwd(), "public", "static"),
        output: path.join("fr-fr", "static"),
      },
    ],
  }
  return createZip(settings)
}

module.exports = onPostBuild
