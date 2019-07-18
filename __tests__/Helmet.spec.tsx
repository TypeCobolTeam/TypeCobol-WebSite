import * as React from "react"

// @ts-ignore
import Helmet from "@components/Helmet"
import { mount } from "enzyme"
import { Helmet as Helm } from "react-helmet"

// eslint-disable-next-line
let wrapper
let helmet

beforeAll(() => {
  wrapper = mount(<Helmet title="TypeCobol" lang="fr" />)
  helmet = Helm.peek()
})

describe("Helmet", () => {
  it("sets typecobol as page title", () => {
    expect(helmet.title).toMatch(/typecobol/i)
  })
  it("has necessary keywords for SEO", () => {
    expect(helmet.metaTags).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          name: "description",
        }),
      ])
    )
    expect(helmet.metaTags).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          name: "keywords",
        }),
      ])
    )
    expect(helmet.metaTags).toEqual(
      expect.arrayContaining([
        expect.objectContaining({
          name: "og:type",
        }),
      ])
    )
  })
})
