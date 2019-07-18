const yaml = require("js-yaml")

module.exports = {
  process(src) {
    const result = yaml.safeLoad(src)
    const json = JSON.stringify(result, undefined, "\t")
    return `module.exports = ${json}`
  },
}
