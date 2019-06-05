import * as React from "react";
import { Container, Box } from "bloomer";

import HelmetInit from "../Helmet";
import "./index.scss";

import { Title } from "bloomer/lib/elements/Title";

class Header extends React.Component<any, any> {
  constructor(props: any) {
    super(props);
  }

  public render() {
    return (
      <div>
        <HelmetInit />
        <Container>
          <Title>Test</Title>
        </Container>
      </div>
    );
  }
}

export default Header;
