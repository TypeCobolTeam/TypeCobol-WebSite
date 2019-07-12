import * as React from "react"

import { Layout } from "antd"

import "./index.less"
import FooterLoader from "./loader"

const { Footer } = Layout

interface FooterProps {
  lang: string
}

const CFooter: React.FunctionComponent<FooterProps> = (props: FooterProps) => {
  const { lang } = props
  return (
    <Footer
      className="tc-footer"
      style={{
        textAlign: "center",
      }}
    >
      <FooterLoader lang={lang} />
    </Footer>
  )
}

export default CFooter
