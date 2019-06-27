import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import processMarkdownHTML from "@utils/processMarkdownHTML"
import { WindowLocation } from "@reach/router"
import Title from "antd/lib/typography/Title"

const NavigationYml = require("@docs/navigation.yml")

interface PageProps {
  data: {
    markdownRemark: {
      frontmatter: {
        title: string
      }
      html: string
    }
  }
  location: WindowLocation
}

export const pageQuery = graphql`
  query DocsTplMarkdown($id: String!) {
    markdownRemark(id: { eq: $id }) {
      frontmatter {
        title
      }
      html
    }
  }
`

class DocsTpl extends React.Component<PageProps, {}> {
  public render() {
    const { html, frontmatter } = this.props.data.markdownRemark
    const { title } = frontmatter
    return (
      <Layout
        showFooter
        showHeader
        sideNavigation={NavigationYml}
        Breadcrumb={{ location: this.props.location, label: title }}
      >
        <Title style={{ fontSize: "2.5em" }}>{title}</Title>
        <div
          // eslint-disable-next-line
          dangerouslySetInnerHTML={{
            __html: processMarkdownHTML(html),
          }}
        />
      </Layout>
    )
  }
}

export default DocsTpl
