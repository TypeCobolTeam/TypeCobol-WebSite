import * as React from "react"
import { graphql } from "gatsby"

import { Parallax } from "rc-scroll-anim"

import Layout from "@components/Layout"

import "./index.less"

import { Layout as antdLayout, Typography } from "antd"
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
}

export default class extends React.Component<IndexPageProps, {}> {
  public render() {
    return (
      <>
        <Layout showFooter showHeader customContentLayout>
          <Content>
            <>
              <Title>TypeCobol</Title>
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
