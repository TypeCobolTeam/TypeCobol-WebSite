import * as React from "react"

import { Layout } from "antd"

import "./index.less"

const { Footer } = Layout

class CFooter extends React.Component<{}, {}> {
  public render() {
    return (
      <Footer
        className="tc-footer"
        style={{
          textAlign: "center",
        }}
      >
        TypeCobol 2019
      </Footer>
    )
  }
}

export default CFooter
