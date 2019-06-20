import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import processMarkdownHTML from "@utils/processMarkdownHTML"
import { WindowLocation } from "@reach/router"

const NavigationYml = require("@tutorial/navigation.yml")

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
  query TutorialTplMarkdown($id: String!) {
    markdownRemark(id: { eq: $id }) {
      frontmatter {
        title
      }
      html
    }
  }
`

class TutorialTpl extends React.Component<PageProps, {}> {
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
        <h1>{title}</h1>
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

export default TutorialTpl
