/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */

import { Modal, Icon } from "antd"
import * as React from "react"
import Flag from "@components/Flag"
import { navigate } from "gatsby"

const languages = require("content/i18n/languages.yml")

class LanguageSwitcher extends React.Component<
  { tag: string },
  { modalVisible: boolean }
> {
  constructor(props) {
    super(props)
    this.state = {
      modalVisible: false,
    }
  }

  setModalVisible(modalVisible) {
    this.setState({ modalVisible })
  }

  public render() {
    const tag = this.props.tag === "en" ? "us" : this.props.tag
    return (
      <div>
        <span onClick={() => this.setModalVisible(true)}>
          {tag.toUpperCase()}
          <Icon style={{ marginLeft: 5 }} type="global" />
        </span>
        <Modal
          title=""
          centered
          visible={this.state.modalVisible}
          onCancel={() => this.setModalVisible(false)}
          footer={null}
        >
          {languages.map(element => {
            const tagg = element.tag === "en" ? "us" : element.tag
            return (
              <Flag
                key={tagg}
                iso={tagg}
                size={40}
                onClick={() => navigate(element.tag)}
              />
            )
          })}
        </Modal>
      </div>
    )
  }
}

export default LanguageSwitcher
