import * as React from "react"
import Helmet from "react-helmet"

const CHelmet: React.StatelessComponent = () => {
  return (
    <Helmet
      defaultTitle="TypeCobol"
      titleTemplate="TypeCobol - %s"
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

export default CHelmet
