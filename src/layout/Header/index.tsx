import * as React from "react";

import HelmetInit from "../Helmet";
import "./index.scss";
import { Container, Heading } from "react-bulma-components";

class Header extends React.Component<any, any> {
  constructor(props: any) {
    super(props);
  }

  public render() {
    return (
      <div>
        <HelmetInit />
        <Container>
          <Heading>Test</Heading>
        </Container>
      </div>
    );
  }
}

export default Header;
