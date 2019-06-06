import * as React from "react";
import Layout from "../layout";
import { graphql } from "gatsby";
import Helmet from "react-helmet";
import { Container, Heading } from "react-bulma-components";

export default function Template({ data }) {
  const { frontmatter } = data.markdownRemark;
  const { html } = data.markdownRemark;
  return (
    <Layout>
      <Container>
        <Helmet title={frontmatter.title} />
        <Heading>{frontmatter.title}</Heading>
        <div dangerouslySetInnerHTML={{ __html: html }} />
      </Container>
    </Layout>
  );
}
export const pageQuery = graphql`
  query($pageID: String!) {
    markdownRemark(id: { eq: $pageID }) {
      html
      frontmatter {
        title
      }
    }
  }
`;
