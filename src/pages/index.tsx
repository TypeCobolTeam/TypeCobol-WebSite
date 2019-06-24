import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import CodeWindow from "@components/CodeWindow"

import { Layout as antdLayout, Typography } from "antd"
import { WindowLocation } from "@reach/router"
const { Title } = Typography
const { Content } = antdLayout

interface IndexPageProps {
  data: {
    site: {
      siteMetadata: {
        title: string
        desc: string
      }
    }
  }
  location: WindowLocation
}

export default class extends React.Component<IndexPageProps, {}> {
  public render() {
    return (
      <>
        <Layout
          showFooter
          showHeader
          customContentLayout
          Breadcrumb={{
            label: this.props.data.site.siteMetadata.title,
            location: this.props.location,
          }}
        >
          <Content
            style={{
              height: "calc(100vh - (69px + 64px))",
            }}
          >
            <div
              style={{
                top: "50vh",
                transform: "translateY(-50%)",
                position: "absolute",
                width: "100%",
              }}
            >
              <div style={{ textAlign: "center", margin: "0 0 70px 0" }}>
                <Title>{this.props.data.site.siteMetadata.title}</Title>
                <Title level={3} style={{ marginTop: 0 }}>
                  {this.props.data.site.siteMetadata.desc}
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
                <span style={{ color: "rgba(255, 255, 255, 0.65)" }}>
                  // Easy !
                </span>
                <br />
                <span style={{ color: "#1899ff" }}>&gt; </span>
                TypeCobol.CLI.exe -1 -i PGM1.tcbl -o PGM1.cbl -e rdz -d
                error.txt -s config\skeletons.xml -c .\copyFolder
              </CodeWindow>
            </div>
          </Content>
        </Layout>
      </>
    )
  }
}

export const pageQuery = graphql`
  query IndexQuery {
    site {
      siteMetadata {
        title
        desc
      }
    }
  }
`
