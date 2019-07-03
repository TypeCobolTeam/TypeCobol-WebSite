// eslint-disable-next-line import/no-extraneous-dependencies
const yaml = require("js-yaml")

module.exports = {
  process(src) {
    const json = yaml.load(src)
    return `module.exports = ${json};`
  },
}
