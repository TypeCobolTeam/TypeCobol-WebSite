import * as React from "react"

import { Layout, Menu } from "antd"
import { Location } from "@reach/router"
import { SideNavProps } from "./interfaces"
import ItemRenderer from "./item"
const { Sider } = Layout

const SideNavigation: React.FunctionComponent<SideNavProps> = (
  props: SideNavProps
) => {
  const { navigation, activeLang, prefix } = props
  const width: number = 300
  return (
    <>
      <Location>
        {locationProps => (
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
              {navigation.map((_element, _index) => {
                const {
                  location: { pathname },
                } = locationProps
                return ItemRenderer(
                  _element,
                  _index,
                  pathname,
                  activeLang,
                  prefix
                )
              })}
            </Menu>
          </Sider>
        )}
      </Location>
    </>
  )
}

export default SideNavigation
