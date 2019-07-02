import * as React from "react"

import { Layout } from "antd"

import "./index.less"

const { Footer } = Layout

const CFooter: React.StatelessComponent = () => {
  return (
    <Footer
      className="tc-footer"
      style={{
        textAlign: "center",
      }}
    >
      <a href="/">Copyright TypeCobol 2019</a>
    </Footer>
  )
}

export default CFooter
