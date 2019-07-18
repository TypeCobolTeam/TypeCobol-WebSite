import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"

import { Layout as antdLayout } from "antd"
import { WindowLocation } from "@reach/router"
import HomepageLoader from "@components/CustomPagesLoader/Home"

import "./index.less"

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

export interface SectionProps {
  children: React.ReactNode
  style?: React.CSSProperties
  blue?: boolean
}

export const Section: React.FunctionComponent<SectionProps> = (
  props: SectionProps
) => {
  const { children, style, blue } = props
  return (
    <section
      style={{
        height: "fit-content",
        position: "relative",
        zIndex: 3,
        textAlign: "center",
        padding: 50,
        background: blue ? "#001529" : "#f0f2f5",
        color: blue ? "white" : "inherit",
        ...style,
      }}
    >
      {children}
    </section>
  )
}

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
          className="homepage"
          style={{
            minHeight: "calc(100vh - 177px)",
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
