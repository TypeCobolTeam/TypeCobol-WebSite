/* eslint-disable react/jsx-one-expression-per-line */
import * as React from "react"

import { Typography, Icon, Row, Col } from "antd"
import { Section } from "src/pages"
import { Link } from "gatsby"

const { Title, Paragraph } = Typography

const logo = require("@assets/logo.svg")

const imgTypedBefore = require("@assets/homepage/codeblocks/typed_before.png")
const imgTypedAfter = require("@assets/homepage/codeblocks/typed_after.png")
const imgProceduresBefore = require("@assets/homepage/codeblocks/procedure_before.png")
const imgProceduresAfter = require("@assets/homepage/codeblocks/procedure_after.png")

const Homepage: React.FunctionComponent = () => {
  return (
    <>
      <Section style={{ height: "calc(100vh - 64px)", padding: 0 }}>
        <div className="fullhome">
          <img src={logo} alt="TypeCobol" className="tc-logo-home" />
          <Title level={3} style={{ marginTop: 0, fontWeight: 300 }}>
            Le <span className="weight-600">nouveau</span> Cobol.
          </Title>
          <Title level={4} style={{ marginTop: 0, fontWeight: 300 }}>
            Un <span className="weight-600">parser</span> incrémental pour{" "}
            <span className="weight-600">IBM Enterprise Cobol 5.1</span> avec{" "}
            <span className="weight-600">zOS Syntax</span>.
          </Title>
        </div>
        <Icon className="scrollDown" type="down" />
      </Section>
      <Section blue>
        <Title level={3} style={{ color: "white", marginBottom: 30 }}>
          <span className="weight-300">L&apos;</span> Initiative TypeCobol
        </Title>
        <Row
          gutter={{ xs: 8, sm: 16, md: 24, lg: 32 }}
          style={{ marginBottom: 25 }}
          justify="center"
          type="flex"
        >
          <Col style={{ width: 420, maxWidth: "100%" }}>
            <Icon type="border-outer" />
            <Title level={4} style={{ color: "white" }}>
              Un parser
            </Title>
            <Paragraph style={{ color: "white" }}>
              Un parser incrémental Cobol 85 ouvert étendu à la syntaxe de Cobol
              2002
            </Paragraph>
          </Col>
          <Col style={{ width: 420, maxWidth: "100%" }}>
            <Icon type="border-outer" />
            <Title level={4} style={{ color: "white" }}>
              Un <span className="weight-300">sous-</span>langage
            </Title>
            <Paragraph style={{ color: "white" }}>
              Une extension du langage Cobol 85
            </Paragraph>
          </Col>
        </Row>
        <Row gutter={40} style={{ marginTop: 50 }}>
          <Col xs={24} lg={8}>
            <Icon type="sync" />
            <Title level={4} style={{ color: "white" }}>
              Incrémental
            </Title>
            <Paragraph style={{ color: "white" }}>
              En prenant en compte <b>seulement les lignes de code modifiées</b>
              , notre parser économise du temps et des ressources.
            </Paragraph>
          </Col>
          <Col xs={24} lg={8}>
            <Icon type="check" />
            <Title level={4} style={{ color: "white" }}>
              Malin
            </Title>
            <Paragraph style={{ color: "white" }}>
              Notre parser <b>comprend le mauvais code</b>, ce qui est vital
              lorsqu&apos;il s&apos;agit d&apos;écrire du code sain.
            </Paragraph>
          </Col>
          <Col xs={24} lg={8}>
            <Icon type="gift" />
            <Title level={4} style={{ color: "white" }}>
              Gratui
            </Title>
            <Paragraph style={{ color: "white" }}>
              Essayez le maintenant. Nous sommes sûrs, vous allez l&apos;adorer!
            </Paragraph>
          </Col>
        </Row>
        <Link to="/en/community" style={{ marginTop: 30 }}>
          Visitez la page dédiée à l&apos;initiative Cobol
        </Link>
      </Section>
      <Section>
        <Title level={2} style={{ marginBottom: 30 }}>
          <span className="weight-300">TypeCobol est</span> typé
        </Title>
        <img
          src={imgTypedBefore}
          alt=""
          style={{ maxWidth: "100%", height: 300 }}
        />
        <Icon type="caret-right" style={{ fontSize: 50, color: "#182b3d" }} />
        <img
          src={imgTypedAfter}
          alt=""
          style={{ maxWidth: "100%", height: 300 }}
        />
      </Section>
      <Section>
        <Title level={2} style={{ marginBottom: 30 }}>
          <span className="weight-300">TypeCobol comprend les</span> procédures
        </Title>
        <img
          src={imgProceduresBefore}
          alt=""
          style={{ maxWidth: "100%", height: 300 }}
        />
        <Icon type="caret-right" style={{ fontSize: 50, color: "#182b3d" }} />
        <img
          src={imgProceduresAfter}
          alt=""
          style={{ maxWidth: "100%", height: 300 }}
        />
      </Section>
      <Section blue>
        <Title level={3} style={{ color: "white", marginBottom: 30 }}>
          <span className="weight-300">TypeCobol est</span> la solution à vos
          problèmes de développement Cobol.
        </Title>
        <Link to="/en/community" style={{ marginTop: 30 }}>
          Visitez notre page dédiée pour en savoir plus
        </Link>
      </Section>
      <Section>
        <Title level={1} style={{ marginBottom: 30 }}>
          <span className="weight-300">TypeCobol est</span> open source
        </Title>
        <a href="//github.com" target="_blank" rel="noopener noreferrer">
          <Icon type="github" style={{ fontSize: 50 }} />
        </a>
      </Section>
    </>
  )
}

export default Homepage
