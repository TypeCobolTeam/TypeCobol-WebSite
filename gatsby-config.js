module.exports = {
  siteMetadata: {
    siteUrl: `https://typecobol.netlify.com`,
    title: `TypeCobol`,
    desc: `An Incremental Cobol parser for IBM Enterprise Cobol 5.1 zOS syntax. TypeCobol is also an extension of Cobol 85 language which can then be converted to Cobol85.`,
  },
  plugins: [
    `gatsby-plugin-react-helmet`,
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/`,
        name: "content",
      },
    },
    `gatsby-transformer-remark`,
    {
      resolve: `gatsby-plugin-breadcrumb`,
      options: {
        defaultCrumb: {
          location: {
            state: { crumbClicked: false },
            pathname: "/",
          },
          crumbLabel: "",
          crumbSeparator: " / ",
        },
      },
    },
    `gatsby-plugin-meta-redirect`,
  ],
}
