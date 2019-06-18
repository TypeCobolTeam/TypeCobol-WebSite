import * as React from "react";
import { graphql } from "gatsby";

interface PageProps {
  data: {
    markdownRemark: {
      frontmatter: {
        title: string;
      };
      html: string;
    };
  };
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
`;

class SingleTemplate extends React.Component<PageProps, {}> {
  readonly hello = `Hello`;
  public render() {
    const { html, frontmatter } = this.props.data.markdownRemark;
    const { title } = frontmatter;
    return (
      <Layout>
        <div
          dangerouslySetInnerHTML={{
            __html: processMarkdownHTML(html)
          }}
        />
      </Layout>
    );
  }
}

export default SingleTemplate;
