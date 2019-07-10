import * as React from "react"
import Helmet from "react-helmet"

interface CHelmetProps {
  title: string
  lang: string
}

const CHelmet: React.StatelessComponent<CHelmetProps> = (
  props: CHelmetProps
) => {
  const { title, lang } = props
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
    >
      {title !== "TypeCobol" && <title>{title}</title>}
      <html lang={lang} />
    </Helmet>
  )
}

export default CHelmet
