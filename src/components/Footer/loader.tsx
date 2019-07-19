import * as React from "react"

interface FooterLoaderProps {
  lang: string
}

interface FooterLoaderStates {
  module: React.FunctionComponent
}

class FooterLoader extends React.Component<
  FooterLoaderProps,
  FooterLoaderStates
> {
  constructor(props) {
    super(props)
    this.state = {
      module: null,
    }
  }

  async componentDidMount() {
    const { lang } = this.props
    const { default: module } = await import(
      `../../../content/i18n/${lang}/footer`
    )
    this.setState({ module })
  }

  render() {
    const { module } = this.state
    if (module) return React.createElement(module)
    return <p>Loading...</p>
  }
}

export default FooterLoader
