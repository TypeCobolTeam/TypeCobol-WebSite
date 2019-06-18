import * as React from "react";
import Footer from "@components/Footer";
import HelmetInit from "@components/Helmet";
import Header from "@components/Header";

import { Layout, ConfigProvider } from "antd";
const { Content } = Layout;

import SideNavigation, { Navigation } from "@components/SideNavigation";

interface LayoutProps {
  showHeader?: boolean;
  showFooter?: boolean;
  sideNavigation?: any;
  customContentLayout?: boolean;
}

class LayoutTpl extends React.Component<LayoutProps, {}> {
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
                    margin: "24px 16px 0"
                  }}
                >
                  <div
                    style={{
                      background: "#fff",
                      padding: 24,
                      minHeight: "calc(100vh - 157px)"
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
    );
  }
}

export default LayoutTpl;
