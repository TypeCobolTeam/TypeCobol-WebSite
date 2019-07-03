import * as React from "react"

// @ts-ignore
import Footer from "@components/Footer"
import { render } from "enzyme"

describe("Footer", () => {
  it("renders correctly", () => {
    const wrapper = render(<Footer />)
    expect(wrapper).toMatchSnapshot()
  })
  it(`should contain site name`, () => {
    const wrapper = render(<Footer />)
    expect(wrapper.text()).toMatch(/typecobol/i)
  })
  it(`should contain a copyright notice`, () => {
    const wrapper = render(<Footer />)
    expect(wrapper.text()).toMatch(/copyright/i)
  })
  it(`should have a link to homepage (/)`, () => {
    const wrapper = render(<Footer />)
    expect(wrapper.find("a[href='/']")).toBeTruthy()
  })
})
