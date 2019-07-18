import * as React from "react"
import { Icon } from "antd"

const stork = require("@assets/homepage/stork.svg")

const Footer: React.FunctionComponent = () => {
  return (
    <>
      <p>
        <span style={{ color: "white" }}>Un projet </span>
        <a href="//www.e-i.com/" target="_blank" rel="noopener noreferrer">
          Euro-Information, la Fintech du Groupe Crédit Mutuel
        </a>
      </p>
      <p
        style={{
          color: "#cf1322",
          textTransform: "uppercase",
          fontWeight: "bold",
        }}
      >
        <img
          style={{
            height: 20,
            filter:
              "invert(17%) sepia(94%) saturate(7444%) hue-rotate(352deg) brightness(84%) contrast(91%)",
          }}
          src={stork}
          alt="Cigogne"
          className="tc-stork"
        />
        Fait avec
        <Icon
          type="heart"
          theme="twoTone"
          twoToneColor="#cf1322"
          style={{ margin: "0 3px" }}
        />
        en Alsace
      </p>
      <a href="mailto:typecobol@e-i.com">Contactez-nous</a>
    </>
  )
}

export default Footer
