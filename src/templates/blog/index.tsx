import * as React from "react"
import { graphql } from "gatsby"

import Layout from "@components/Layout"
import processMarkdownHTML from "@utils/processMarkdownHTML"
import { WindowLocation } from "@reach/router"
import Title from "antd/lib/typography/Title"
import { Icon } from "antd"

interface BlogTemplateProps {
  data: {
    markdownRemark: {
      frontmatter: {
        title: string
        date: string
        author: string
        translator: string
      }
      fields: {
        translationCode: string
        gitLink: string
      }
      html: string
    }
  }
  location: WindowLocation
}

export const pageQuery = graphql`
  query BlogTplMarkdown($id: String!) {
    markdownRemark(id: { eq: $id }) {
      frontmatter {
        title
        date
        author
        translator
      }
      fields {
        translationCode
        gitLink
      }
      html
    }
  }
`

export const Info = (person: string) => {
  const reg = /((?:\w+ ?)+)<(?:(?:(?:(?:https?:\/\/)?twitter\.com\/)|@)([a-zA-Z0-9_]+))?([a-zA-Z0-9._-]+@[a-zA-Z0-9._-]{2,}\.[a-z]{2,4})?>/
  const executed = reg.exec(person)
  if (!executed || !(executed[2] || executed[3])) return null
  const name = executed[1]
  const url = executed[2]
    ? `https://twitter.com/${executed[2]}`
    : `mailto:${executed[3]}`
  return { name, url }
}

const BlogTemplate: React.StatelessComponent<BlogTemplateProps> = (
  props: BlogTemplateProps
) => {
  const {
    data: {
      markdownRemark: {
        html,
        frontmatter: { title, date, author, translator },
        fields: { translationCode, gitLink },
      },
    },
    location,
  } = props

  const authorInfo = Info(author)
  const translatorInfo = Info(translator)

  return (
    <Layout
      showFooter
      showHeader
      navPrefix="blog"
      Breadcrumb={{ location, label: title }}
      translationCode={translationCode}
      gitLink={gitLink}
    >
      <div style={{ marginBottom: 20 }}>
        <Title level={4} style={{ display: "inline" }}>
          <Icon style={{ marginRight: 5 }} type="calendar" />
          {date.substring(0, 10)}
        </Title>
        <Title
          level={4}
          style={{
            marginLeft: 20,
            float: "right",
            display: translator ? "inline" : "none",
          }}
        >
          {translator &&
            (translatorInfo ? (
              <a href={translatorInfo.url}>
                <Icon style={{ marginRight: 5 }} type="flag" />
                {translatorInfo.name}
              </a>
            ) : (
              <>
                <Icon style={{ marginRight: 5 }} type="flag" />
                <span>{translator}</span>
              </>
            ))}
        </Title>
        <Title level={4} style={{ float: "right", display: "inline" }}>
          {authorInfo ? (
            <a href={authorInfo.url}>
              <Icon style={{ marginRight: 5 }} type="user" />
              {authorInfo.name}
            </a>
          ) : (
            <>
              <Icon style={{ marginRight: 5 }} type="user" />
              <span>{author}</span>
            </>
          )}
        </Title>
      </div>
      <Title style={{ fontSize: "2.5em" }}>{title}</Title>
      <div
        // eslint-disable-next-line
        dangerouslySetInnerHTML={{
          __html: processMarkdownHTML(html),
        }}
      />
    </Layout>
  )
}

export default BlogTemplate
