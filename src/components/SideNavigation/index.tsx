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
    return (
      <>
        <Sider width={200} breakpoint="lg" collapsedWidth="0">
          <div className="logo" />
          <Menu mode="inline" style={{ height: "100%", borderRight: 0 }}>
            {navigation.map(subItemRenderer)}
          </Menu>
        </Sider>
      </>
    )
  }
}

export default SideNavigation
