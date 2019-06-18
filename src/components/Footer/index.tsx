import * as React from "react"

// @ts-ignore
// import logoEI from "@content/images/EI.png";

import { Layout } from "antd"
const { Footer } = Layout

class CFooter extends React.Component<{}, {}> {
  public render() {
    return <Footer style={{ textAlign: "center" }}>TypeCobol 2019</Footer>
  }
}

export default CFooter
