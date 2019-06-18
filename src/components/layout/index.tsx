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
            <Layout style={{ flexDirection: "row" }}>
              {this.props.sideNavigation && (
                <SideNavigation navigation={this.props.sideNavigation} />
              )}
              {this.props.customContentLayout ? (
                <>{this.props.children}</>
              ) : (
                <Content
                  style={{
                    margin: "24px 16px 0",
                  }}
                >
                  <div
                    style={{
                      background: "#fff",
                      minHeight: "calc(100vh - 157px)",
                      padding: 24,
                    }}
                  >
                    {this.props.children}
                  </div>
                </Content>
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
