module.exports = {
  siteMetadata: {
    siteUrl: "https://xcmci02d.formation.cm-cic.fr",
    title: "TypeCobol",
    desc:
      "An Incremental Cobol parser for IBM Enterprise Cobol 5.1 zOS syntax. TypeCobol is also an extension of Cobol 85 language which can then be converted to Cobol85.",
  },
  plugins: [
    "gatsby-plugin-react-helmet",
    "gatsby-plugin-sharp",
    {
      resolve: "gatsby-source-filesystem",
      options: {
        path: `${__dirname}/content/`,
        name: "content",
      },
    },
    {
      resolve: "gatsby-transformer-remark",
      options: {
        plugins: [
          {
            resolve: "gatsby-remark-images",
          },
          {
            resolve: "gatsby-remark-codemirror",
            options: {
              theme: "typecobol",
            },
          },
        ],
      },
    },
    {
      resolve: "gatsby-plugin-breadcrumb",
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
    "gatsby-plugin-sitemap",
    "gatsby-plugin-catch-links",
    {
      resolve: "gatsby-plugin-manifest",
      options: {
        name: "TypeCobol",
        short_name: "TypeCobol",
        start_url: "/",
        background_color: "#f0f2f5",
        theme_color: "#ffffff",
        display: "standalone",
        icon: "content/images/favicon.png",
      },
    },
    {
      resolve: "gatsby-plugin-offline",
      options: {
        cacheId: "typecobol-cache",
      },
    },
    "gatsby-plugin-meta-redirect",
  ],
}
