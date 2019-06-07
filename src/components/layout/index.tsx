import * as React from "react";
import Helmet from "react-helmet";

// import font-awesome
import { library } from "@fortawesome/fontawesome-svg-core";
import { fab } from "@fortawesome/free-brands-svg-icons";
import { faCheckSquare, faCoffee } from "@fortawesome/free-solid-svg-icons";
library.add(fab, faCheckSquare, faCoffee);

import "../../scss/index.scss";
import Header from "./Header";
import Foot from "./Footer";

class Layout extends React.Component<any, any> {
  constructor(props: any) {
    super(props);
  }

  public render() {
    if (!this.props.isHomePage) {
      return (
        <div>
          <Helmet>
            <title>TypeCobol</title>
            <meta
              name="viewport"
              content="width=device-width, initial-scale=1"
            />
          </Helmet>
          <Header />
          {this.props.children}
          <Foot />
        </div>
      );
    } else {
      return (
        <div>
          <Helmet>
            <title>TypeCobol</title>
            <meta
              name="viewport"
              content="width=device-width, initial-scale=1"
            />
          </Helmet>
          {this.props.children}
        </div>
      );
    }
  }
}

export default Layout;
