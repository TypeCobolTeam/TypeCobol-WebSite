import * as React from "react";
import { graphql } from "gatsby";

interface pageProps {
  data: {
    site: {
      siteMetadata: {
        title: string;
      };
    };
  };
}

export const pageQuery = graphql`
  query SingleMarkdown {
    site {
      siteMetadata {
        title
      }
    }
  }
`;

class TutorialTemplate extends React.Component<pageProps, {}> {
  readonly hello = `Hello`;
  public render() {
    const { title } = this.props.data.site.siteMetadata;
    return (
      <div>
        <h1>{this.hello} TypeScript world!</h1>
        <p>
          This site is named <strong>{title}</strong>
        </p>
      </div>
    );
  }
}

export default TutorialTemplate;
