import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"

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
          <Content>
            <>
              <Title>{this.props.data.site.siteMetadata.title}</Title>
            </>
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
