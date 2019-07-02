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
      html
    }
  }
`

const SingleTemplate: React.StatelessComponent<SingleTemplateProps> = (
  props: SingleTemplateProps
) => {
  const { data, location } = props
  const { html, frontmatter } = data.markdownRemark
  const { title } = frontmatter
  return (
    <Layout customContentLayout Breadcrumb={{ location, label: title }}>
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
