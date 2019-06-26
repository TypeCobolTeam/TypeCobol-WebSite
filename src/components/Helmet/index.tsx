import * as React from "react"
import Helmet from "react-helmet"

class CHelmet extends React.Component<{}, {}> {
  public render() {
    return (
      <Helmet
        encodeSpecialCharacters
        defaultTitle="TypeCobol"
        titleTemplate="TypeCobolTeam - %s"
        meta={[
          {
            name: "description",
            content: "Official Website for TypeCobol",
          },
          {
            name: "keywords",
            content: "cobol, language, parser, ibm",
          },
          {
            name: "og:type",
            content: "website",
          },
        ]}
      />
    )
  }
}

export default CHelmet
