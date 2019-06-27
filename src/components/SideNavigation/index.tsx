import * as React from "react"

import { Layout, Menu } from "antd"
import { Location } from "@reach/router"
import { Navigation, Navigations } from "./interfaces"
import ItemRenderer from "./item"
const { Sider } = Layout

interface PageProps {
  navigation?: Navigation[]
}

class SideNavigation extends React.Component<PageProps, {}> {
  public render() {
    const navigation: Navigations = this.props.navigation || []
    const width: number = 300
    return (
      <>
        <Location>
          {props => (
            <Sider
              width={width}
              breakpoint="lg"
              collapsedWidth="0"
              style={{ height: "calc(100vh - 64px)" }}
              className="hideScroll"
            >
              <div className="logo" />
              <Menu
                mode="inline"
                defaultSelectedKeys={["active"]}
                defaultOpenKeys={["submenu-active"]}
                style={{
                  height: "100%",
                  width,
                  paddingTop: 70,
                  borderRight: 0,
                  overflowY: "auto",
                  overflowX: "hidden",
                  position: "fixed",
                  top: 0,
                  zIndex: 1,
                }}
                className="hideScroll"
              >
                {navigation.map((_element, _index) =>
                  ItemRenderer(_element, _index, props.location.pathname)
                )}
              </Menu>
            </Sider>
          )}
        </Location>
      </>
    )
  }
}

export default SideNavigation
