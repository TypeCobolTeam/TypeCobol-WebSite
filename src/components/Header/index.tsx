import * as React from "react"
import { Link } from "gatsby"

import { Layout, Menu, Icon } from "antd"

import "./index.less"
import { Location } from "@reach/router"

const logo = require("@images/logo.svg")
const NavbarYml: NavElement[] = require("@content/navbar.yml")

const Head = Layout.Header

export interface NavElement {
  label: string
  href: string
  color?: any
}

const activeLink = (href: string, actualPath: string) => {
  if (href === "/") {
    return href === actualPath
  }
  return actualPath.includes(href)
}

class Header extends React.Component<{}, {}> {
  public render() {
    return (
      <>
        <Head style={{ zIndex: 500 }}>
          <Link
            to="/"
            className="tc-logo"
            style={{
              float: "left",
              height: "100%",
            }}
          >
            <img
              src={logo}
              alt=""
              style={{
                height: "100%",
                maxHeight: 40,
                verticalAlign: "middle",
              }}
            />
          </Link>
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
                          activeLink(element.href, props.location.pathname)
                            ? "active"
                            : Math.random()
                        }
                      >
                        {linktag}
                      </Menu.Item>
                    )
                  })}
                  <Menu.Item
                    key="github"
                    style={{ float: "right", fontSize: 30, padding: 0 }}
                  >
                    <a
                      rel="noopener noreferrer"
                      target="_blank"
                      href="https://github.com/TypeCobolTeam/TypeCobol"
                    >
                      <Icon type="github" style={{ fontSize: 30 }} />
                    </a>
                  </Menu.Item>
                </Menu>
              )
            }}
          </Location>
        </Head>
      </>
    )
  }
}

export default Header
