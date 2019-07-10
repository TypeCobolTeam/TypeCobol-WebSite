import * as React from "react"

import { Menu, Icon, Divider } from "antd"
import { Link } from "gatsby"
import { Navigation } from "./interfaces"

const { SubMenu } = Menu

const activeLink = (href: string, actualPath: string) => {
  if (href.endsWith("/")) {
    return href === actualPath
  }
  return actualPath.includes(href)
}
const ItemRenderer = (
  element: Navigation,
  index: number,
  pathname: string,
  activeLang: string,
  prefix: string
) => {
  const urlPrefix =
    prefix !== "" ? `/${activeLang}/${prefix}` : `/${activeLang}`
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
            element.items.some(v =>
              activeLink(`${urlPrefix}/${v.href}`, pathname)
            )
              ? "submenu-active"
              : index
          }
          title={
            <span style={{ fontWeight: 600 }}>
              {element.icon && <Icon type={element.icon} />}
              <span>{element.group}</span>
            </span>
          }
        >
          {element.items.map((_item, _index) =>
            ItemRenderer(_item, _index, pathname, activeLang, prefix)
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
          <Link to={`${urlPrefix}/${element.href}`}>
            {icon}
            {elem}
          </Link>
        )
      return (
        <Menu.Item
          key={
            activeLink(`${urlPrefix}/${element.href}`, pathname)
              ? "active"
              : index
          }
        >
          {linktag}
        </Menu.Item>
      )

    case "divider":
      return (
        <Divider
          key={index}
          orientation="left"
          style={{
            fontSize: 12,
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
