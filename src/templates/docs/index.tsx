import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import processMarkdownHTML from "@utils/processMarkdownHTML"
import { WindowLocation } from "@reach/router"
import Title from "antd/lib/typography/Title"

interface DocsTemplateProps {
  data: {
    markdownRemark: {
      frontmatter: {
        title: string
      }
      fields: {
        translationCode: string
        gitLink: string
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
      fields {
        translationCode
        gitLink
      }
      html
    }
  }
`

const DocsTemplate: React.StatelessComponent<DocsTemplateProps> = (
  props: DocsTemplateProps
) => {
  const {
    data: {
      markdownRemark: {
        html,
        frontmatter: { title },
        fields: { translationCode, gitLink },
      },
    },
    location,
  } = props
  // eslint-disable-next-line
  const NavigationYml = require(`../../../content/i18n/${translationCode}/docs/navigation.yml`)
  return (
    <Layout
      showFooter
      showHeader
      sideNavigation={NavigationYml}
      navPrefix="docs"
      Breadcrumb={{ location, label: title }}
      translationCode={translationCode}
      gitLink={gitLink}
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
