import * as React from "react"
import { WindowLocation } from "@reach/router"

interface HomepageLoaderProps {
  params: Params
  lang: string
}
interface Params {
  title: string
  desc: string
  translation: string
  location: WindowLocation
}

interface HomepageLoaderStates {
  module: React.FunctionComponent<Params>
}

class HomepageLoader extends React.Component<
  HomepageLoaderProps,
  HomepageLoaderStates
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
      `../../../content/i18n/${lang}/homepage`
    )
    this.setState({ module })
  }

  render() {
    const { module } = this.state
    if (module) return React.createElement(module)
    return <p>done</p>
  }
}

export default HomepageLoader
