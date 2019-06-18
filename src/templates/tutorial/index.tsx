import * as React from "react"
import { graphql } from "gatsby"

// @ts-ignore disable tslint warning about yml module not found
import NavigationYml from "@content/Tutorial/navigation.yml"
import Layout from "@components/Layout"
import { processMarkdownHTML } from "@utils/processMarkdownHTML"

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
      <Layout showFooter showHeader sideNavigation={NavigationYml}>
        <div
          dangerouslySetInnerHTML={{
            __html: processMarkdownHTML(html),
          }}
        />
      </Layout>
    )
  }
}

export default TutorialTpl
