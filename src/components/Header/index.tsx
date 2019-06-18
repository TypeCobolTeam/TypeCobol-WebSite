import * as React from "react"
import { Link } from "gatsby"

// @ts-ignore ignore import errors because of importing a yaml file
import NavbarYml from "@content/navbar.yml"

import HelmetInit from "@components/Helmet"

import { Layout, Menu, Typography, Icon } from "antd"
const Head = Layout.Header

import { Color } from "csstype"

import "./index.less"

export interface NavElement {
  title: string
  href: string
  color?: Color
}

class Header extends React.Component<{}, {}> {
  public render() {
    return (
      <>
        <HelmetInit />
        <Head className="header">
          <Menu theme="dark" mode="horizontal" style={{ lineHeight: "64px" }}>
            <Menu.Item
              className="tc-logo"
              style={{
                color: "white",
                fontSize: 30,
                textDecoration: "underline",
                textDecorationColor: "#1899ff",
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
                )
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
                )
              }
            })}
            <Menu.Item style={{ float: "right" }}>
              <a href="//github.com">
                <Icon
                  type="github"
                  style={{
                    fontSize: 30,
                    lineHeight: "64px",
                    verticalAlign: "middle",
                  }}
                />
              </a>
            </Menu.Item>
          </Menu>
        </Head>
      </>
    )
  }
}

export default Header
