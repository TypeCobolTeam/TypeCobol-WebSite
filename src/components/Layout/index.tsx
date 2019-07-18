import * as React from "react"

import { Layout, ConfigProvider, Icon } from "antd"

import Footer from "@components/Footer"
import HelmetInit from "@components/Helmet"
import Header from "@components/Header"
import SideNavigation from "@components/SideNavigation"
import { WindowLocation } from "@reach/router"
import "./index.less"

import { Breadcrumb as GBreadcrumb } from "gatsby-plugin-breadcrumb"
const { Content } = Layout

interface CLayoutProps {
  showHeader?: boolean
  showFooter?: boolean
  sideNavigation?: any
  customContentLayout?: boolean
  children?: React.ReactNode
  Breadcrumb: {
    location: WindowLocation
    label: string
  }
  translationCode?: string
  navPrefix?: string
  gitLink?: string
  style?: React.CSSProperties
}

const CLayout: React.FunctionComponent<CLayoutProps> = (
  props: CLayoutProps
) => {
  const {
    showHeader,
    showFooter,
    sideNavigation,
    customContentLayout,
    Breadcrumb,
    children,
    translationCode,
    navPrefix,
    gitLink,
    style,
  } = props
  const langCode = translationCode || "en"
  return (
    <>
      <HelmetInit title={Breadcrumb.label} lang={langCode} />
      <ConfigProvider prefixCls="tc">
        <Layout style={{ ...style }}>
          {showHeader && <Header translationCode={langCode} />}
          <Layout
            style={{
              flexDirection: sideNavigation ? "row" : "column",
            }}
          >
            {sideNavigation && (
              <SideNavigation
                prefix={navPrefix || ""}
                activeLang={langCode}
                navigation={sideNavigation}
              />
            )}
            <Layout>
              {customContentLayout ? (
                children
              ) : (
                <Layout style={{ padding: "0 24px 24px 24px" }}>
                  <div style={{ padding: "16px 24px" }}>
                    <GBreadcrumb
                      location={Breadcrumb.location}
                      crumbLabel={Breadcrumb.label}
                      crumbWrapperStyle={{ class: "tc-breadcrumb-wrapper" }}
                      crumbSeparator="/"
                    />
                  </div>
                  <Content
                    style={{
                      background: "#fff",
                      margin: 0,
                      padding: 24,
                    }}
                  >
                    <div
                      style={{
                        minHeight: "calc(100vh - 279px)",
                        padding: 24,
                      }}
                    >
                      {gitLink && (
                        <a
                          href={gitLink}
                          style={{
                            width: "100%",
                            float: "right",
                            textAlign: "right",
                          }}
                        >
                          Edit on Github
                          <Icon type="github" style={{ marginLeft: 5 }} />
                        </a>
                      )}
                      {children}
                    </div>
                  </Content>
                </Layout>
              )}
              {showFooter && <Footer lang={translationCode} />}
            </Layout>
          </Layout>
        </Layout>
      </ConfigProvider>
    </>
  )
}

export default CLayout
