import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import processMarkdownHTML from "@utils/processMarkdownHTML"
import { WindowLocation } from "@reach/router"
import Title from "antd/lib/typography/Title"

interface CommunityTemplateProps {
  data: {
    markdownRemark: {
      frontmatter: {
        title: string
      }
      fields: {
        translationCode: string
      }
      html: string
    }
  }
  location: WindowLocation
}

export const pageQuery = graphql`
  query CommunityTplMarkdown($id: String!) {
    markdownRemark(id: { eq: $id }) {
      frontmatter {
        title
      }
      fields {
        translationCode
      }
      html
    }
  }
`

const CommunityTemplate: React.StatelessComponent<CommunityTemplateProps> = (
  props: CommunityTemplateProps
) => {
  const {
    data: {
      markdownRemark: {
        html,
        frontmatter: { title },
        fields: { translationCode },
      },
    },
    location,
  } = props
  // eslint-disable-next-line
  const NavigationYml = require(`../../../content/i18n/${translationCode}/community/navigation.yml`)
  return (
    <Layout
      showFooter
      showHeader
      sideNavigation={NavigationYml}
      navPrefix="community"
      Breadcrumb={{ location, label: title }}
      translationCode={translationCode}
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

export default CommunityTemplate
