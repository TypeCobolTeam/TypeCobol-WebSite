import * as React from "react"

// @ts-ignore
import CodeWindow from "@components/CodeWindow"
import { shallow } from "enzyme"

// cases generated with snappify
describe("CodeWindow", () => {
  it("CodeWindow Case #1", () => {
    const wrapper = shallow(
      <CodeWindow style={{}} showButtons={false}>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #2", () => {
    const wrapper = shallow(
      <CodeWindow style={{}} showButtons>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #3", () => {
    const wrapper = shallow(
      <CodeWindow style={{}}>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #4", () => {
    const wrapper = shallow(
      <CodeWindow style={{ padding: 0 }}>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #5", () => {
    const wrapper = shallow(
      <CodeWindow style={{ padding: 0 }} showButtons>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #6", () => {
    const wrapper = shallow(
      <CodeWindow style={{ padding: 0 }}>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #7", () => {
    const wrapper = shallow(
      <CodeWindow showButtons={false}>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #8", () => {
    const wrapper = shallow(
      <CodeWindow showButtons>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
  it("CodeWindow Case #9", () => {
    const wrapper = shallow(
      <CodeWindow>
        <div />
      </CodeWindow>
    )
    expect(wrapper).toMatchSnapshot()
  })
})
