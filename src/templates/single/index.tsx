import processMarkdownHTML from "@utils/processMarkdownHTML"
import { graphql } from "gatsby"
import * as React from "react"
import Layout from "@components/Layout"
import { WindowLocation } from "@reach/router"

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
  query SingleTplMarkdown($id: String!) {
    markdownRemark(id: { eq: $id }) {
      frontmatter {
        title
      }
      html
    }
  }
`

class SingleTemplate extends React.Component<PageProps, {}> {
  public render() {
    const { html, frontmatter } = this.props.data.markdownRemark
    const { title } = frontmatter
    return (
      <Layout Breadcrumb={{ location: this.props.location, label: title }}>
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

export default SingleTemplate
