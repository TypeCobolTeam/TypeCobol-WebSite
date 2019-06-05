import * as React from "react";

import "./index.scss";
import { Container } from "bloomer/lib/layout/Container";
import { Footer as BFooter } from "bloomer/lib/layout/Footer";
import { Content } from "bloomer/lib/elements/Content";
import { Columns } from "bloomer/lib/grid/Columns";
import { Column } from "bloomer/lib/grid/Column";
import { Icon } from "bloomer/lib/elements/Icon";

class Footer extends React.Component<any, any> {
  constructor(props: any) {
    super(props);
  }

  public render() {
    return (
      <BFooter id="footer">
        <Container>
          <Content>
            <Columns>
              <Column>
                <p>
                  Made with
                  <Icon className="fa fa-heart" style={{ margin: "5px" }} />
                  by <a>TypeCobol Team</a>
                </p>
              </Column>
            </Columns>
            <Content isSize="small">
              <p>
                The TypeCobol project is licensed under{" "}
                <a
                  target="_blank"
                  href="https://spdx.org/licenses/CECILL-C.html"
                >
                  CeCILL-C
                </a>
                .
              </p>
              <p>
                The website content is licensed under{" "}
                <a
                  target="_blank"
                  href="https://spdx.org/licenses/CECILL-C.html"
                >
                  CeCILL-C
                </a>
                .
              </p>
            </Content>
          </Content>
        </Container>
      </BFooter>
    );
  }
}

export default Footer;
