import * as React from "react"
import { Link } from "gatsby"

import { Layout, Menu, Icon, Divider } from "antd"
const { Sider } = Layout

interface PageProps {
  navigation?: Navigation[]
}

export interface Navigation {
  title: string
  color?: any
  iconColor?: any
  textColor?: any
  link?: string
  icon?: string
  divider?: string
}

const subItemRenderer = (item: Navigation, index: number) => {
  if (item.divider) {
    return (
      <Divider
        orientation="left"
        style={{
          fontSize: 14,
          marginTop: index === 0 ? "" : "32px",
          textTransform: "uppercase",
        }}
      >
        {item.divider}
      </Divider>
    )
  }
  if (item.link) {
    return (
      <Link
        key={item.link}
        to={item.link}
        className="tc-menu-item"
        activeClassName="tc-menu-item-selected"
        partiallyActive
        role="menuitem"
        style={{ paddingLeft: item.icon ? 24 : 48 }}
      >
        {item.icon && (
          <Icon
            type={item.icon}
            style={{ color: item.iconColor || item.color }}
          />
        )}
        <span
          className="nav-text"
          style={{ color: item.textColor || item.color }}
        >
          {item.title}
        </span>
      </Link>
    )
  }
  return <></>
}

class SideNavigation extends React.Component<PageProps, {}> {
  public render() {
    const navigation = this.props.navigation || []
    const width: number = 300
    return (
      <>
        <Sider
          width={width}
          breakpoint="lg"
          collapsedWidth="0"
          style={{ height: "calc(100vh - 64px)" }}
          className="hideScroll"
        >
          <Menu
            mode="inline"
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
            {navigation.map(subItemRenderer)}
          </Menu>
        </Sider>
      </>
    )
  }
}

export default SideNavigation
