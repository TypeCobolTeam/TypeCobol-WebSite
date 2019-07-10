import * as React from "react"
import { Link } from "gatsby"

import { Layout, Menu, Icon } from "antd"

import "./index.less"
import { Location } from "@reach/router"
import { current } from "@utils/versions"
import LanguageSwitcher from "@components/LanguageSwitcher"

const logo = require("@images/logo.svg")
const NavbarYml: NavElement[] = require("@content/navbar.yml")

const Head = Layout.Header

export interface NavElement {
  label: string
  href: string
  color?: any
}

interface HeaderProps {
  translationCode: string
}

const activeLink = (href: string, actualPath: string) => {
  if (href === "/") {
    return href === actualPath
  }
  return actualPath.includes(href)
}

const Header: React.StatelessComponent<HeaderProps> = (props: HeaderProps) => {
  const { translationCode } = props
  return (
    <Head style={{ zIndex: 500 }}>
      <Link
        to={`/${translationCode}`}
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
        {props_ => {
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
                  linktag = (
                    <Link to={`/${translationCode}/${element.href}`}>
                      {element.label}
                    </Link>
                  )
                }
                return (
                  <Menu.Item
                    key={
                      activeLink(element.href, props_.location.pathname)
                        ? "active"
                        : Math.random()
                    }
                  >
                    {linktag}
                  </Menu.Item>
                )
              })}
              <Menu.Item key="lang" className="lang" style={{ float: "right" }}>
                <LanguageSwitcher tag={translationCode} />
              </Menu.Item>
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
              <Menu.Item
                key={
                  activeLink("/versions", props_.location.pathname)
                    ? "active"
                    : "versions"
                }
                className="version"
                style={{ float: "right" }}
              >
                <Link to="/versions">
                  <span>{`v${current}`}</span>
                </Link>
              </Menu.Item>
            </Menu>
          )
        }}
      </Location>
    </Head>
  )
}

export default Header
