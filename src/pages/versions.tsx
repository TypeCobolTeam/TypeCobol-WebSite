import * as React from "react"

import Layout from "@components/Layout"
import { Layout as antdLayout, Typography } from "antd"
import { WindowLocation } from "@reach/router"
import { versions } from "@utils/versions"

import "./index.less"

const { Title } = Typography
const { Content } = antdLayout

interface VersionsPageProps {
  location: WindowLocation
}

const VersionsPage: React.StatelessComponent<VersionsPageProps> = (
  props: VersionsPageProps
) => {
  const { location } = props
  return (
    <>
      <Layout
        showFooter
        showHeader
        Breadcrumb={{
          label: "Versions",
          location,
        }}
      >
        <Content>
          <Title level={1} style={{ marginBottom: 40 }}>
            TypeCobol Versions
          </Title>
          {Object.entries(versions).map(elem => {
            const [version, info] = elem
            return (
              <div style={{ padding: "10px 0", fontSize: 15 }}>
                <Title level={2}>{version}</Title>
                <ul>
                  <li>
                    <a href={`//${info.url}`}>Documentation</a>
                  </li>
                  <li>
                    <a href={`//${info.changelog}`}>Changelog</a>
                  </li>
                </ul>
              </div>
            )
          })}
        </Content>
      </Layout>
    </>
  )
}

export default VersionsPage
