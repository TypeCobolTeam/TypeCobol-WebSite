import * as React from "react";
import { Link } from "gatsby";
import { Color } from "csstype";

import { Layout, Menu, Icon, Divider } from "antd";
const { SubMenu, Item } = Menu;
const { Sider } = Layout;

interface PageProps {
  navigation?: Navigation[];
}

export interface Navigation {
  title: string;
  color?: Color;
  iconColor?: Color;
  textColor?: Color;
  link?: string;
  icon?: string;
  divider?: string;
}

export default class SideNavigation extends React.Component<PageProps, {}> {
  public subItemRenderer(item: Navigation, index: number) {
    const style: React.CSSProperties = item.color ? { color: item.color } : {};
    if (item.divider) {
      let marginTop = index === 0 ? "" : "32px";
      return (
        <Divider
          orientation="left"
          style={{
            fontSize: 14,
            textTransform: "uppercase",
            marginTop: marginTop
          }}
        >
          {item.divider}
        </Divider>
      );
    } else {
      let left = item.icon ? 24 : 48;
      item.link = item.link || "/";
      return (
        <Link
          key={item.link}
          to={item.link}
          className="tc-menu-item"
          activeClassName="tc-menu-item-selected"
          partiallyActive={true}
          role="menuitem"
          style={{ paddingLeft: left }}
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
      );
    }
  }
  public render() {
    const navigation = this.props.navigation || [];
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
    );
  }
}
