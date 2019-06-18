import * as React from "react";

import HelmetInit from "../Helmet";

// @ts-ignore ignore import errors because of importing a yaml file
import NavbarYml from "@content/navbar.yml";

import "./index.less";

export interface NavElement {
  title: string;
  href: string;
  color?: Color;
}

import { Layout, Menu, Typography, Icon } from "antd";
import { Color } from "csstype";
import { Link } from "gatsby";
const Head = Layout.Header;
const { Title } = Typography;

class Header extends React.Component<{}, {}> {
  state = { location: "" };
  public render() {
    return (
      <>
        <HelmetInit />
        <Head className="header">
          <Menu theme="dark" mode="horizontal" style={{ lineHeight: "64px" }}>
            <Menu.Item
              className="tc-logo"
              style={{
                fontSize: 30,
                color: "white",
                textDecoration: "underline",
                textDecorationColor: "#1899ff"
              }}
            >
              <b>TypeCobol</b>
            </Menu.Item>
            {NavbarYml.map((element: NavElement) => {
              if (
                element.href.startsWith("//") ||
                element.href.startsWith("http")
              ) {
                return (
                  <a className="tc-menu-item" href={element.href}>
                    {element.title}
                  </a>
                );
              } else {
                return (
                  <>
                    <Link
                      to={element.href}
                      className="tc-menu-item"
                      activeClassName="tc-menu-item-selected"
                      partiallyActive={element.href !== "/"}
                    >
                      {element.title}
                    </Link>
                  </>
                );
              }
            })}
            <Menu.Item style={{ float: "right" }}>
              <a href="//github.com">
                <Icon type="github" style={{ fontSize: 30, lineHeight: "64px", verticalAlign: "middle" }} />
              </a>
            </Menu.Item>
          </Menu>
        </Head>
      </>
    );
  }
}

export default Header;
