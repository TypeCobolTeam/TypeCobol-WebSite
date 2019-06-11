module.exports = {
  siteMetadata: {
    title: `TypeCobol`,
    desc: `An Incremental Cobol parser for IBM Enterprise Cobol 5.1 for zOS syntax. <br> TypeCobol is also an extension of Cobol 85 language which can then be converted to Cobol85.`
  },
  plugins: [
    `gatsby-plugin-react-helmet`,
    `gatsby-plugin-typescript`,
    `gatsby-plugin-sass`,
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/`,
        name: "content"
      }
    },
    `gatsby-transformer-remark`,
    `gatsby-redirect-from`,
    `gatsby-plugin-meta-redirect`
  ]
};
