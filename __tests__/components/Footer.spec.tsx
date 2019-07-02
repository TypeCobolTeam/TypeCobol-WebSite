import * as React from "react"

// @ts-ignore
import Footer from "@components/Footer"
import { shallow } from "enzyme"

describe("Footer", () => {
  it("renders correctly", () => {
    const wrapper = shallow(<Footer />)
    expect(wrapper).toMatchSnapshot()
  })
  it(`should contain site name`, () => {
    const wrapper = shallow(<Footer />)
    expect(wrapper.find("Footer").text()).toMatch(/copyright/i)
  })
  it(`should contain a copyright notice`, () => {
    const wrapper = shallow(<Footer />)
    expect(wrapper.find("Footer").text()).toMatch(/copyright/i)
  })
  it(`should have a link to homepage (/)`, () => {
    const wrapper = shallow(<Footer />)
    expect(wrapper.find("a[href='/']")).toBeTruthy()
  })
})
