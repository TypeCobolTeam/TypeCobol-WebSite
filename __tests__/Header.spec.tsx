import * as React from "react"

// @ts-ignore
import Header from "@components/Header"
import { shallow } from "enzyme"

describe("Header", () => {
  it("renders correctly", () => {
    const wrapper = shallow(<Header />)
    expect(wrapper).toMatchSnapshot()
  })
})
