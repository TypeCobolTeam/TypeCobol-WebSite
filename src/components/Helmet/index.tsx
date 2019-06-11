import * as React from "react";
import Helmet from "react-helmet";

class HelmetInit extends React.Component<any, any> {
  public render() {
    return (
      <Helmet
        encodeSpecialCharacters={true}
        defaultTitle="TypeCobol"
        titleTemplate="TypeCobolTeam - %s"
      >
        <html lang="en" />
        <body className="root" />
        <meta name="description" content="TypeCobol's official website" />
        <meta property="og:type" content="website" />
      </Helmet>
    );
  }
}

export default HelmetInit;
