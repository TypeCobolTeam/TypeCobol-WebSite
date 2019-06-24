import * as React from "react"

import "./index.less"

interface Props {
  style?: React.CSSProperties
  showButtons?: boolean
}

class CodeWindow extends React.Component<Props, {}> {
  public render() {
    return (
      <div className="codeWindow" style={this.props.style}>
        {this.props.showButtons && (
          <div className="buttons">
            <span id="close" />
            <span id="minimize" />
            <span id="fullscreen" />
          </div>
        )}
        <div
          className="content"
          style={{ marginTop: this.props.showButtons ? 30 : 0 }}
        >
          {this.props.children}
        </div>
      </div>
    )
  }
}

export default CodeWindow
