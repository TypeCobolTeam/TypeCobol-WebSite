import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import processMarkdownHTML from "@utils/processMarkdownHTML"
import { WindowLocation } from "@reach/router"
import Title from "antd/lib/typography/Title"

const NavigationYml = require("@docs/navigation.yml")

interface DocsTemplateProps {
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

const DocsTemplate: React.StatelessComponent<DocsTemplateProps> = (
  props: DocsTemplateProps
) => {
  const { data, location } = props
  const { html, frontmatter } = data.markdownRemark
  const { title } = frontmatter
  return (
    <Layout
      showFooter
      showHeader
      sideNavigation={NavigationYml}
      Breadcrumb={{ location, label: title }}
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

export default DocsTemplate
