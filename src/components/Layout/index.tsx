import * as React from "react"

import { Layout, ConfigProvider, Breadcrumb } from "antd"
const { Content } = Layout

import Footer from "@components/Footer"
import HelmetInit from "@components/Helmet"
import Header from "@components/Header"
import SideNavigation, { Navigation } from "@components/SideNavigation"
import { WindowLocation } from "@reach/router"

// @ts-ignore
import { Breadcrumb as GBreadcrumb } from "gatsby-plugin-breadcrumb"

import "./index.less"

interface CLayoutProps {
  showHeader?: boolean
  showFooter?: boolean
  sideNavigation?: any
  customContentLayout?: boolean
  Breadcrumb: {
    location: WindowLocation
    label: string
  }
}

class CLayout extends React.Component<CLayoutProps, {}> {
  public render() {
    return (
      <>
        <HelmetInit />
        <ConfigProvider prefixCls="tc">
          <Layout>
            {this.props.showHeader && <Header />}
            <Layout>
              {this.props.sideNavigation && (
                <SideNavigation navigation={this.props.sideNavigation} />
              )}
              {this.props.customContentLayout ? (
                this.props.children
              ) : (
                <Layout style={{ padding: "0 24px 0 24px" }}>
                  <div style={{ padding: "16px 24px" }}>
                    <GBreadcrumb
                      location={this.props.Breadcrumb.location}
                      crumbLabel={this.props.Breadcrumb.label}
                      crumbWrapperStyle={{ class: "tc-breadcrumb-wrapper" }}
                      crumbSeparator="/"
                    />
                  </div>
                  <Content
                    style={{
                      background: "#fff",
                      margin: 0,
                      minHeight: 280,
                      padding: 24,
                    }}
                  >
                    <div
                      style={{
                        minHeight: "calc(100vh - 234px)",
                        padding: 24,
                      }}
                    >
                      {this.props.children}
                    </div>
                  </Content>
                </Layout>
              )}
            </Layout>
            {this.props.showFooter && <Footer />}
          </Layout>
        </ConfigProvider>
      </>
    )
  }
}

export default CLayout
