import * as React from "react"
import Layout from "@components/Layout"

import { WindowLocation } from "@reach/router"
import { Layout as antdLayout, Typography, PageHeader, Icon } from "antd"
import { Info } from "src/templates/blog"
const { Title, Paragraph } = Typography
const { Content } = antdLayout

const { graphql, Link } = require("gatsby")

interface BlogNode {
  node: {
    fields: {
      slug: string
      translationCode: string
    }
    frontmatter: {
      title: string
      date: string
      short: string
      author: string
      translator: string
    }
  }
}

interface BlogPageProps {
  data: {
    allMarkdownRemark: {
      edges: BlogNode[]
    }
  }
  pageContext: {
    translation: string
  }
  location: WindowLocation
}

export const BlogQuery = graphql`
  {
    allMarkdownRemark(
      limit: 1000
      filter: { frontmatter: { date: { ne: null } }, fields: {} }
    ) {
      edges {
        node {
          fields {
            slug
            translationCode
          }
          frontmatter {
            title
            date
            short
            author
            translator
          }
        }
      }
    }
  }
`

const BlogPage: React.StatelessComponent<BlogPageProps> = (
  props: BlogPageProps
) => {
  const {
    pageContext: { translation },
    data: {
      allMarkdownRemark: { edges },
    },
    location,
  } = props

  const i18n = require(`../../content/i18n/${translation}/blog.yml`) // eslint-disable-line
  return (
    <>
      <Layout
        showHeader
        showFooter
        Breadcrumb={{
          label: "Blog",
          location,
        }}
        translationCode={translation}
      >
        <Content>
          <Title level={1} style={{ padding: "20px 0", textAlign: "center" }}>
            {i18n.latest}
          </Title>
          {edges.map((elem, index) => {
            if (elem.node.fields.translationCode !== translation) return null
            const key = `${index}-${elem.node.frontmatter.title}`
            const authorInfo = Info(elem.node.frontmatter.author)
            const translatorInfo = Info(elem.node.frontmatter.translator)
            return (
              <PageHeader
                style={{
                  border: "1px solid rgb(235, 237, 240)",
                  margin: "20px 0",
                }}
                key={key}
                title={
                  <Link
                    to={elem.node.fields.slug}
                    style={{ fontSize: 25, color: "black" }}
                  >
                    {elem.node.frontmatter.title}
                  </Link>
                }
                subTitle={
                  <span>
                    <Icon style={{ marginRight: 5 }} type="calendar" />
                    {elem.node.frontmatter.date
                      .substring(0, 10)
                      .replace(/-/g, "/")}
                  </span>
                }
              >
                <div className="wrap">
                  <div className="content">
                    <Paragraph>{elem.node.frontmatter.short}</Paragraph>
                    <div
                      style={{
                        display: "flex",
                        flexFlow: "row-reverse",
                      }}
                    >
                      <div style={{ order: 1 }}>
                        {elem.node.frontmatter.author &&
                          (authorInfo ? (
                            <a href={authorInfo.url}>
                              <Icon style={{ marginRight: 5 }} type="user" />
                              {authorInfo.name}
                            </a>
                          ) : (
                            <span>
                              <Icon style={{ marginRight: 5 }} type="user" />
                              {elem.node.frontmatter.author}
                            </span>
                          ))}
                      </div>
                      <div
                        style={{
                          order: 0,
                          marginLeft: 15,
                          display: elem.node.frontmatter.translator
                            ? "initial"
                            : "none",
                        }}
                      >
                        {elem.node.frontmatter.translator &&
                          (translatorInfo ? (
                            <a href={translatorInfo.url}>
                              <Icon style={{ marginRight: 5 }} type="flag" />
                              {translatorInfo.name}
                            </a>
                          ) : (
                            <span>
                              <Icon style={{ marginRight: 5 }} type="flag" />
                              {elem.node.frontmatter.translator}
                            </span>
                          ))}
                      </div>
                    </div>
                  </div>
                </div>
              </PageHeader>
            )
          })}
        </Content>
      </Layout>
    </>
  )
}

export default BlogPage
