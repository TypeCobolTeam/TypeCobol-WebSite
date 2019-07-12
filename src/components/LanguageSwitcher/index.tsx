/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import { Icon, Dropdown, Menu } from "antd"
import * as React from "react"
import Flag from "@components/Flag"
import { navigate } from "gatsby"

const languages = require("content/i18n/languages.yml")

interface LanguageSwitcherProps {
  tag: string
}

const menu = actualLang => {
  return (
    <Menu>
      {languages.map(element => {
        if (actualLang !== element.tag) {
          const tagg = element.tag === "en" ? "us" : element.tag
          return (
            <Menu.Item key={tagg} onClick={() => navigate(element.tag)}>
              <Flag iso={tagg} size={20} />
            </Menu.Item>
          )
        }
        return <span />
      })}
    </Menu>
  )
}

const LanguageSwitcher: React.FunctionComponent<LanguageSwitcherProps> = (
  props: LanguageSwitcherProps
) => {
  const tag = props.tag === "en" ? "us" : props.tag
  return (
    <Dropdown overlay={menu(props.tag)}>
      <span className="ant-dropdown-link">
        {tag.toUpperCase()}
        <Icon style={{ marginLeft: 5 }} type="global" />
        <Icon type="down" />
      </span>
    </Dropdown>
  )
}

export default LanguageSwitcher
