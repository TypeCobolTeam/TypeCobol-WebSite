/* eslint-disable dot-notation */

import "jest-extended"
import { readdirSync, readFileSync, existsSync, statSync } from "fs"
import { shallow } from "enzyme"
import { createElement } from "react"
import matter = require("gray-matter")

const { join, extname } = require("path")
const { safeLoad } = require("js-yaml")

const languagesInDir = (path: string) => {
  const dir = readdirSync(path, { withFileTypes: true }).filter(e =>
    e.isDirectory()
  )
  return Array.from(dir, x => x.name)
}

const getKeys = obj => {
  return Object.keys(obj).reduce((r, k) => {
    r.push(k)
    if (Object(obj[k]) === obj[k]) r.push(...getKeys(obj[k]))
    return r
  }, [])
}

const isValidPersonString = (person: string) => {
  const reg = /((?:\w+ ?)+)<(?:(?:(?:(?:https?:\/\/)?twitter\.com\/)|@)([a-zA-Z0-9_]+))?([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]{2,}\.[a-z]{2,4})?>/
  const executed = reg.exec(person)
  if (!executed || !(executed[2] || executed[3])) return false
  return true
}

const getAllHrefsFromCatNav = catNav => {
  return Object.values(catNav)
    .map(elem => {
      if (elem["item"]) {
        return elem["href"]
      }
      if (elem["group"] && elem["items"]) {
        return [
          Object.keys(elem["items"]).map(subi =>
            elem["items"][subi]["item"] ? elem["items"][subi]["href"] : null
          ),
        ]
      }
      return null
    })
    .toString()
    .split(",")
    .filter(el => el !== "")
}

const readAllFilesInDirAndSubDir = directory => {
  let results = []
  const firstLevel = readdirSync(directory)
  firstLevel.forEach(file => {
    const filePath = join(directory, file)
    const fileStat = statSync(filePath)
    if (fileStat && fileStat.isDirectory()) {
      results = results.concat(readAllFilesInDirAndSubDir(filePath))
    } else {
      results.push(filePath)
    }
  })
  return results
}

const languages = safeLoad(readFileSync("content/i18n/languages.yml", "utf8"))

describe("i18n", () => {
  describe("common", () => {
    describe("languages.yml", () => {
      test("file recognition as array", () => {
        expect(languages).toBeArray()
      })
      languages.forEach(langObj => {
        test("general", () => {
          expect(langObj).toBeObject()
          expect(langObj).toContainAllKeys(["tag", "name", "completed"])
          expect(langObj.tag).toBeString()
          expect(langObj.tag.length).toStrictEqual(2)
          expect(langObj.name).toBeString()
          expect(langObj.tag.length).not.toBeEmpty()
          expect(langObj.completed).toBeWithin(1, 4)
        })
      })
      test("language folders all present in file", () => {
        const inDir = languagesInDir("content/i18n")
        const langsInCfg = Array.from(languages, (x: { tag: string }) => x.tag)
        expect(langsInCfg).toStrictEqual(inDir)
      })
    })
  })
  languages.forEach(lang => {
    describe(`${lang.tag}`, () => {
      describe("navbar.yml", () => {
        const generalNavigation: { label: string; href: string }[] = safeLoad(
          readFileSync(`content/i18n/${lang.tag}/navbar.yml`, "utf8")
        )
        const generalNavigationHrefs = Object.keys(generalNavigation).map(
          value => generalNavigation[value].href
        )
        describe("per-element tests", () => {
          generalNavigation.forEach(el => {
            test(el.label, () => {
              expect(el).toContainAllKeys(["label", "href"])
              expect(el.label).toBeString()
              expect(el.href).toBeString()
              expect(el.href).toStartWith("/")
            })
          })
        })
        test("has to include common links", () => {
          expect(generalNavigationHrefs).toIncludeSameMembers([
            "/",
            "/community",
            "/docs",
            "/blog",
          ])
        })
      })
      describe("blog.yml", () => {
        const blogTranslationsFile = safeLoad(
          readFileSync(`content/i18n/${lang.tag}/blog.yml`, "utf8")
        )
        test("file recognition as object", () => {
          expect(blogTranslationsFile).toBeObject()
        })
        test("contain translations", () => {
          expect(blogTranslationsFile).toContainAllKeys([
            "latest",
            "posted_by",
            "translated_by",
            "on",
          ])
          expect(blogTranslationsFile["latest"]).not.toBeEmpty()
          expect(blogTranslationsFile["posted_by"]).not.toBeEmpty()
          expect(blogTranslationsFile["translated_by"]).not.toBeEmpty()
          expect(blogTranslationsFile["on"]).not.toBeEmpty()
        })
      })
      describe("homepage.tsx", () => {
        test("rendered is not empty", () => {
          return import(`content/i18n/${lang.tag}/homepage`).then(home => {
            const wrapper = shallow(createElement(home.default))
            expect(wrapper.html()).toBeTruthy()
          })
        })
      })
      describe("footer.tsx", () => {
        test("file contains [euro-information & typecobol & (france || alsace)]", () => {
          const cb = home => {
            const wrapper = shallow(createElement(home.default))
              .html()
              .toLowerCase()
            expect(wrapper).toInclude("euro-information")
            expect(wrapper).toInclude("typecobol")
            expect(/france|alsace/.test(wrapper)).toBeTrue()
          }
          return import(`content/i18n/${lang.tag}/footer`).then(cb)
        })
      })
      ;["community", "docs"].forEach(type => {
        describe(`${type}/`, () => {
          describe("navigation.yml", () => {
            const Navigation = safeLoad(
              readFileSync(
                `content/i18n/${lang.tag}/${type}/navigation.yml`,
                "utf8"
              )
            )
            const NavigationHrefs = getAllHrefsFromCatNav(Navigation)
            test("using correctly defined keys", () => {
              const keys = getKeys(Navigation).filter(
                (e, p, s) => e.toUpperCase() !== e && s.indexOf(e) === p
              )
              expect([
                "color",
                "textColor",
                "group",
                "items",
                "item",
                "href",
                "icon",
                "iconColor",
                "divider",
              ]).toIncludeAllMembers(keys)
            })
            describe("check if all links exists in directory", () => {
              NavigationHrefs.forEach((el: string) => {
                let target = el.endsWith("/")
                  ? `${el}index.md`
                  : el.replace(".html", ".md")
                // prettier-ignore
                target = join(process.cwd(),"content","i18n",lang.tag,type,target)
                test(el, () => {
                  expect(existsSync(target)).toBeTrue()
                })
              })
            })
          })
          describe("check markdown files required fields", () => {
            const markdownFiles = readAllFilesInDirAndSubDir(
              `content/i18n/${lang.tag}/${type}/`
            ).filter(e => extname(e) === ".md")
            markdownFiles.forEach(file => {
              test(`${file}`, () => {
                const content = readFileSync(file)
                const { data } = matter(content)
                expect(data).toBeObject()
                expect(data.title).toBeString()
                if (data.redirectFrom) expect(data).toBeArray()
                if (data.disable !== undefined) expect(data).toBeBoolean()
              })
            })
          })
        })
      })
      describe("blog/", () => {
        const mdBlogFiles = readAllFilesInDirAndSubDir(
          `content/i18n/${lang.tag}/blog/`
        ).filter(e => extname(e) === ".md")
        mdBlogFiles.forEach(file => {
          test(`${file}`, () => {
            const content = readFileSync(file)
            const { data } = matter(content)

            expect(data).toBeObject()

            expect(data.title).toBeString()

            expect(data.date).toBeString()
            const date = new Date(data.date)
            expect(data.date).toStrictEqual(date.toISOString())

            expect(data.author).toBeString()
            expect(isValidPersonString(data.author)).toBeTrue()

            if (data.translator) {
              expect(data.translator).toBeString()
              expect(isValidPersonString(data.translator)).toBeTrue()
            }

            expect(data.short).toBeString()
            expect(data.short.length).toBeGreaterThan(20)
          })
        })
      })
    })
  })
})
