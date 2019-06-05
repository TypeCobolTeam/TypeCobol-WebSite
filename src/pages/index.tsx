import * as React from "react";
import Layout from "../layout";
import { graphql } from "gatsby";
import { Title } from "bloomer/lib/elements/Title";
import { Subtitle } from "bloomer/lib/elements/Subtitle";
import { Container } from "bloomer/lib/layout/Container";

interface IndexPageProps {
  data: {
    site: {
      siteMetadata: {
        title: string;
      };
    };
  };
}

export default class extends React.Component<IndexPageProps, {}> {
  constructor(props: IndexPageProps, context: any) {
    super(props, context);
  }
  public render() {
    return (
      <Layout>
        <Container>
          <Title>Welcome on {this.props.data.site.siteMetadata.title}</Title>
          <Subtitle>This is a skeleton. Changes gonna come.</Subtitle>
        </Container>
      </Layout>
    );
  }
}

export const pageQuery = graphql`
  query IndexQuery {
    site {
      siteMetadata {
        title
      }
    }
  }
`;
