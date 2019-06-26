import * as React from "react"

import { Menu, Icon, Divider } from "antd"
import { Link } from "gatsby"
import { Navigation } from "./interfaces"

const { SubMenu } = Menu

const activeLink = (href: string, actualPath: string) => {
  if (href === "/") {
    return href === actualPath
  }
  const result = actualPath.includes(href)
  return result
}

interface PageProps {
  element: Navigation
  index: number
  pathname: string
}

const ItemRenderer = (element: Navigation, index: number, pathname: string) => {
  const isFirstElem = index === 0
  const type =
    // eslint-disable-next-line
    typeof element.group !== "undefined"
      ? "group"
      : // eslint-disable-next-line
      typeof element.item !== "undefined" && typeof element.href !== "undefined"
      ? "item"
      : typeof element.divider !== "undefined"
      ? "divider"
      : "not_valid"
  let icon
  let elem
  let linktag
  switch (type) {
    case "group":
      return (
        <SubMenu
          key={
            element.items.some(v => activeLink(v.href, pathname))
              ? "submenu-active"
              : index
          }
          title={
            <span>
              {element.icon && <Icon type={element.icon} />}
              <span>{element.group}</span>
            </span>
          }
        >
          {element.items.map((_item, _index) =>
            ItemRenderer(_item, _index, pathname)
          )}
        </SubMenu>
      )

    case "item":
      icon = element.icon && (
        <Icon
          type={element.icon}
          style={{ color: element.iconColor || element.color }}
        />
      )
      elem = (
        <span style={{ color: element.textColor || element.color }}>
          {element.item}
        </span>
      )
      linktag =
        element.href.startsWith("//") || element.href.startsWith("http") ? (
          <a href={element.href}>
            {icon}
            {elem}
          </a>
        ) : (
          <Link to={element.href}>
            {icon}
            {elem}
          </Link>
        )
      return (
        <Menu.Item key={activeLink(element.href, pathname) ? "active" : index}>
          {linktag}
        </Menu.Item>
      )

    case "divider":
      return (
        <Divider
          orientation="left"
          style={{
            fontSize: 14,
            marginTop: isFirstElem ? "" : "32px",
            textTransform: "uppercase",
          }}
        >
          {element.divider}
        </Divider>
      )

    default:
      return <></>
  }
}

export default ItemRenderer
