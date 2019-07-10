import processMarkdownHTML from "@utils/processMarkdownHTML"
import { graphql } from "gatsby"
import * as React from "react"
import Layout from "@components/Layout"
import { WindowLocation } from "@reach/router"

interface SingleTemplateProps {
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
  query SingleTplMarkdown($id: String!) {
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

const SingleTemplate: React.StatelessComponent<SingleTemplateProps> = (
  props: SingleTemplateProps
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
  return (
    <Layout
      customContentLayout
      Breadcrumb={{ location, label: title }}
      translationCode={translationCode}
    >
      <div
        // eslint-disable-next-line
        dangerouslySetInnerHTML={{
          __html: processMarkdownHTML(html),
        }}
      />
    </Layout>
  )
}

export default SingleTemplate
