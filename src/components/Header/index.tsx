import * as React from "react"
import { Link } from "gatsby"

import { Layout, Menu, Icon } from "antd"

import "./index.less"
import { Location } from "@reach/router"
import LanguageSwitcher from "@components/LanguageSwitcher"

const logo = require("@assets/logo.svg")

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
  const url = href.startsWith("/") ? href : `/${href}`
  if (url.endsWith("/")) {
    return url === actualPath
  }
  return actualPath.includes(url)
}

const Header: React.StatelessComponent<HeaderProps> = (props: HeaderProps) => {
  const { translationCode } = props
  return (
    <Head style={{ zIndex: 500 }} className="navigation">
      <Location>
        {props_ => {
          const NavbarYml = require(`../../../content/i18n/${translationCode}/navbar.yml`) // eslint-disable-line
          return (
            <Menu
              theme="dark"
              mode="horizontal"
              selectedKeys={["active"]}
              style={{ lineHeight: "64px" }}
            >
              <Menu.Item key="lang" className="lang">
                <Link
                  to={`/${translationCode}/`}
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
              </Menu.Item>
              {NavbarYml.map((element: NavElement) => {
                let linktag: any
                if (
                  element.href.startsWith("//") ||
                  element.href.startsWith("http")
                ) {
                  linktag = <a href={element.href}>{element.label}</a>
                } else {
                  linktag = (
                    <Link to={`/${translationCode}${element.href}`}>
                      {element.label}
                    </Link>
                  )
                }
                return (
                  <Menu.Item
                    key={
                      activeLink(
                        `${translationCode}${element.href}`,
                        props_.location.pathname
                      )
                        ? "active"
                        : element.href
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
                  href="//github.com/TypeCobolTeam/TypeCobol"
                >
                  <Icon type="github" style={{ fontSize: 30 }} />
                </a>
              </Menu.Item>
              <Menu.Item key="lang" className="lang" style={{ float: "right" }}>
                <LanguageSwitcher tag={translationCode} />
              </Menu.Item>
              <Menu.Item key="versions" style={{ float: "right" }}>
                <a
                  href="//github.com/TypeCobolTeam/TypeCobol/releases"
                  target="_blank"
                  rel="noopener noreferrer"
                >
                  Changelog
                </a>
              </Menu.Item>
            </Menu>
          )
        }}
      </Location>
    </Head>
  )
}

export default Header
