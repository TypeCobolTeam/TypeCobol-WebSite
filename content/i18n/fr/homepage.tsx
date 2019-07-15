import * as React from "react"
import CodeWindow from "@components/CodeWindow"

import { Typography } from "antd"
import { WindowLocation } from "@reach/router"

const { Title } = Typography

const logo = require("@assets/logo.svg")

interface HomepageProps {
  title: string
  desc: string
  translation: string
  location: WindowLocation
}

const Homepage: React.StatelessComponent<HomepageProps> = (
  props: HomepageProps
) => {
  const { desc } = props
  return (
    <div
      style={{
        top: "50vh",
        transform: "translateY(-50%)",
        position: "absolute",
        width: "100%",
      }}
    >
      <p>FRANCAIS</p>
      <div style={{ textAlign: "center", margin: "0 0 70px 0" }}>
        <img
          style={{
            filter:
              "invert(99%) sepia(3%) saturate(0%) hue-rotate(46deg) brightness(117%) contrast(100%)",
          }}
          src={logo}
          alt="TypeCobol"
          className="tc-logo-home"
        />
        <Title level={3} style={{ marginTop: 0 }}>
          {desc}
        </Title>
      </div>
      <CodeWindow
        showButtons
        style={{
          width: "70%",
          margin: "auto",
          marginTop: 50,
        }}
      >
        <span style={{ color: "rgba(255, 255, 255, 0.65)" }}>// Easy !</span>
        <br />
        <span style={{ color: "#1899ff" }}>&gt; </span>
        TypeCobol.CLI.exe -1 -i PGM1.tcbl -o PGM1.cbl -e rdz -d error.txt -s
        config\skeletons.xml -c .\copyFolder
      </CodeWindow>
    </div>
  )
}

export default Homepage
