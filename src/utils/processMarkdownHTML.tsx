const Replacements: ReplacementProp[] = require("@content/HTMLReplacements.yml")

interface ReplacementProp {
  toReplace: string
  with: string
}

const processHTML = (html: string) => {
  let newHTML: string = html
  Replacements.forEach((element: ReplacementProp) => {
    newHTML = newHTML.replace(element.toReplace, element.with)
  })
  return newHTML
}

export default processHTML
