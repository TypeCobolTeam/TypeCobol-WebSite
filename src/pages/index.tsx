import * as React from "react";
import { graphql } from "gatsby";

import Layout from "../components/layout";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";

import { Hero, Heading } from "react-bulma-components";
import { Container } from "react-bulma-components";

interface IndexPageProps {
  data: {
    site: {
      siteMetadata: {
        title: string;
        desc: string;
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
      <Layout isHomePage>
        <Hero color="info" size="fullheight" gradient>
          <Hero.Body>
            <Container>
              <Heading size={1}>
                {this.props.data.site.siteMetadata.title}
              </Heading>
              <Heading
                subtitle
                size={3}
                dangerouslySetInnerHTML={{
                  __html: this.props.data.site.siteMetadata.desc
                }}
              />
            </Container>
          </Hero.Body>
        </Hero>
      </Layout>
    );
  }
}

export const pageQuery = graphql`
  query IndexQuery {
    site {
      siteMetadata {
        title
        desc
      }
    }
  }
`;
