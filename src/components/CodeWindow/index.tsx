import * as React from "react"

import "./index.less"

interface CodeWindowProps {
  style?: React.CSSProperties
  showButtons?: boolean
  children?: React.ReactNode
}

const CodeWindow: React.StatelessComponent<CodeWindowProps> = (
  props: CodeWindowProps
) => {
  const { style, showButtons, children } = props
  return (
    <div className="codeWindow" style={style}>
      {showButtons && (
        <div className="buttons">
          <span id="close" />
          <span id="minimize" />
          <span id="fullscreen" />
        </div>
      )}
      <div className="content" style={{ marginTop: showButtons ? 30 : 0 }}>
        {children}
      </div>
    </div>
  )
}

export default CodeWindow
