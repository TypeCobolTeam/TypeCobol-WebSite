import * as React from "react"

import { Layout, ConfigProvider } from "antd"
const { Content } = Layout

import Footer from "@components/Footer"
import HelmetInit from "@components/Helmet"
import Header from "@components/Header"
import SideNavigation, { Navigation } from "@components/SideNavigation"

interface CLayoutProps {
  showHeader?: boolean
  showFooter?: boolean
  sideNavigation?: any
  customContentLayout?: boolean
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
                <>{this.props.children}</>
              ) : (
                <Layout style={{ padding: "0 24px 0 24px" }}>
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
