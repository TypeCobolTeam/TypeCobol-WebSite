import * as React from "react"
import Helmet from "react-helmet"

class CHelmet extends React.Component<{}, {}> {
  public render() {
    return (
      <Helmet
        encodeSpecialCharacters
        defaultTitle="TypeCobol"
        titleTemplate="TypeCobolTeam - %s"
      >
        <html lang="en" />
        <body className="root" />
        <meta name="description" content="TypeCobol's official website" />
        <meta property="og:type" content="website" />
      </Helmet>
    )
  }
}

export default CHelmet
