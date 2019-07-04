const versionsYML: VersionsYML = require("@content/versions.yml")

interface Version {
  url: string
  changelog: string
}

interface Versions {
  [key: string]: Version
}

interface VersionsYML {
  current: string
  versions: Versions
}
const { versions } = versionsYML
const { current } = versionsYML

const getInfoByVersion = (version: string) => {
  const selected = versions[version]
  return selected
}

const getInfoByUrl = (url: string) => {
  const selected = Object.keys(versions).find(key => versions[key].url === url)
  return selected
}

export { getInfoByUrl, getInfoByVersion, versions, current }
