import * as React from "react"
import { Icon } from "antd"

const Footer: React.FunctionComponent = () => {
  return (
    <>
      <p>
        <span style={{ color: "white" }}>A project by </span>
        <a href="//www.e-i.com/" target="_blank" rel="noopener noreferrer">
          Euro-Information, the Fintech of the Crédit Mutuel Group
        </a>
      </p>
      <p
        style={{
          color: "#cf1322",
          textTransform: "uppercase",
          fontWeight: "bold",
        }}
      >
        Made with
        <Icon
          type="heart"
          theme="twoTone"
          twoToneColor="#cf1322"
          style={{ margin: "0 3px" }}
        />
        in France
      </p>
      <a href="mailto:typecobol@e-i.com">Contact Us</a>
    </>
  )
}

export default Footer
