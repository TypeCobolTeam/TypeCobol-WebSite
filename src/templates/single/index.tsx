import { processMarkdownHTML } from "@utils/processMarkdownHTML"
import { graphql } from "gatsby"
import * as React from "react"

interface PageProps {
  data: {
    markdownRemark: {
      frontmatter: {
        title: string
      }
      html: string
    }
  }
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
  readonly hello = `Hello`
  public render() {
    const { html, frontmatter } = this.props.data.markdownRemark
    const { title } = frontmatter
    return (
      <div
        dangerouslySetInnerHTML={{
          __html: processMarkdownHTML(html),
        }}
      />
    )
  }
}

export default SingleTemplate
