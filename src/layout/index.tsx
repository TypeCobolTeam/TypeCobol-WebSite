import * as React from "react";
import Helmet from "react-helmet";
import Header from "./Header";
import Footer from "./Footer";

import "../style/index.scss";

class Layout extends React.Component<any, any> {
  constructor(props: any) {
    super(props);
  }

  public render() {
    return (
      <div>
        <Helmet title="TypeCobol" />
        <Header />
        {this.props.children}
        <Footer />
      </div>
    );
  }
}

export default Layout;
