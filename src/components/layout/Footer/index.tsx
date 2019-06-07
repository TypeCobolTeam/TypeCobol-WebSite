import * as React from "react";

import "./index.scss";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";

class Footer extends React.Component<any, any> {
  constructor(props: any) {
    super(props);
  }

  public render() {
    return (
      <div>
        <p>
          Made with
          <FontAwesomeIcon icon="heart" style={{ margin: "5px" }} />
          by <a>TypeCobol Team</a>
        </p>
        <p>
          The TypeCobol project is licensed under{" "}
          <a target="_blank" href="https://spdx.org/licenses/CECILL-C.html">
            CeCILL-C
          </a>
          .
        </p>
        <p>
          The website content is licensed under{" "}
          <a target="_blank" href="https://spdx.org/licenses/CECILL-C.html">
            CeCILL-C
          </a>
          .
        </p>
      </div>
    );
  }
}

export default Footer;
