import * as React from "react"
import { Link } from "gatsby"

// @ts-ignore ignore import errors because of importing a yaml file
import NavbarYml from "@content/navbar.yml"

import HelmetInit from "@components/Helmet"

import logo from "@content/images/620.png"

import { Layout, Menu, Typography, Icon } from "antd"
const Head = Layout.Header

import { Color } from "csstype"

import "./index.less"
import { Location } from "@reach/router"

export interface NavElement {
  label: string
  href: string
  color?: Color
}

class Header extends React.Component<{}, {}> {
  public render() {
    return (
      <>
        <HelmetInit />
        <Head>
          <div
            className="logo"
            style={{
              float: "left",
              height: 31,
              margin: "16px 10px 16px 10px",
            }}
          >
            <img
              src={logo}
              alt=""
              style={{ height: "100%", verticalAlign: "super" }}
            />
          </div>
          <Location>
            {props => {
              return (
                <Menu
                  theme="dark"
                  mode="horizontal"
                  selectedKeys={["active"]}
                  style={{ lineHeight: "64px" }}
                >
                  {NavbarYml.map((element: NavElement) => {
                    let linktag: any
                    if (
                      element.href.startsWith("//") ||
                      element.href.startsWith("http")
                    ) {
                      linktag = <a href={element.href}>{element.label}</a>
                    } else {
                      linktag = <Link to={element.href}>{element.label}</Link>
                    }
                    return (
                      <Menu.Item
                        key={
                          this.activeLink(element.href, props.location.pathname)
                            ? "active"
                            : Math.random()
                        }
                      >
                        {linktag}
                      </Menu.Item>
                    )
                  })}
                </Menu>
              )
            }}
          </Location>
        </Head>
      </>
    )
  }
  private activeLink(href: string, actualPath: string) {
    if (href === "/") {
      return href === actualPath
    } else {
      return actualPath.includes(href)
    }
  }
}

export default Header
