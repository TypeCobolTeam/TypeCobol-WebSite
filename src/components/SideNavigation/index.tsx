import * as React from "react"
import { Link } from "gatsby"
import { Color } from "csstype"

import { Layout, Menu, Icon, Divider } from "antd"
const { Sider } = Layout

interface PageProps {
  navigation?: Navigation[]
}

export interface Navigation {
  title: string
  color?: Color
  iconColor?: Color
  textColor?: Color
  link?: string
  icon?: string
  divider?: string
}

class SideNavigation extends React.Component<PageProps, {}> {
  public render() {
    const navigation = this.props.navigation || []
    return (
      <>
        <Sider
          width={200}
          style={{ background: "#fff", margin: "24px 0 0 0" }}
          breakpoint="lg"
          collapsedWidth="0"
        >
          <div className="logo" />
          <Menu mode="inline" style={{ height: "100%", borderRight: 0 }}>
            {navigation.map(this.subItemRenderer.bind(this))}
          </Menu>
        </Sider>
      </>
    )
  }
  private subItemRenderer(item: Navigation, index: number) {
    const style: React.CSSProperties = item.color ? { color: item.color } : {}
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
    } else if (item.link) {
      return (
        <Link
          key={item.link}
          to={item.link}
          className="tc-menu-item"
          activeClassName="tc-menu-item-selected"
          partiallyActive={true}
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
    } else {
      return <></>
    }
  }
}

export default SideNavigation
