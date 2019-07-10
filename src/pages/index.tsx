import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"

import { Layout as antdLayout } from "antd"
import { WindowLocation } from "@reach/router"
import HomepageLoader from "@components/HomepageLoader"

const { Content } = antdLayout

interface IndexPageProps {
  pageContext: {
    translation: string
  }
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

const IndexPage: React.StatelessComponent<IndexPageProps> = (
  props: IndexPageProps
) => {
  const {
    data: {
      site: {
        siteMetadata: { title, desc },
      },
    },
    pageContext: { translation },
    location,
  } = props
  return (
    <>
      <Layout
        showFooter
        showHeader
        customContentLayout
        Breadcrumb={{
          label: title,
          location,
        }}
        translationCode={translation}
      >
        <Content
          style={{
            height: "calc(100vh - (69px + 64px))",
          }}
        >
          <HomepageLoader
            params={{ title, desc, translation, location }}
            lang={translation}
          />
        </Content>
      </Layout>
    </>
  )
}

export default IndexPage
