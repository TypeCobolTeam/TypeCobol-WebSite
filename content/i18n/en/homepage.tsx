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
            The <span className="weight-600">new</span> Cobol.
          </Title>
          <Title level={4} style={{ marginTop: 0, fontWeight: 300 }}>
            An incremental <span className="weight-600">parser</span> for{" "}
            <span className="weight-600">IBM Enterprise Cobol 5.1</span> for{" "}
            <span className="weight-600">zOS Syntax</span>.
          </Title>
        </div>
        <Icon className="scrollDown" type="down" />
      </Section>
      <Section blue>
        <Title level={3} style={{ color: "white", marginBottom: 30 }}>
          <span className="weight-300">The</span> TypeCobol Inititiative
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
              A parser
            </Title>
            <Paragraph style={{ color: "white" }}>
              An open source Cobol 85 incremental parser (+Typedef of Cobol
              2002)
            </Paragraph>
          </Col>
          <Col style={{ width: 420, maxWidth: "100%" }}>
            <Icon type="border-outer" />
            <Title level={4} style={{ color: "white" }}>
              A <span className="weight-300">sub</span>language
            </Title>
            <Paragraph style={{ color: "white" }}>
              An extension of Cobol 85 language (named TypeCobol) which can then
              be converted to Cobol 85
            </Paragraph>
          </Col>
        </Row>
        <Row gutter={40} style={{ marginTop: 50 }}>
          <Col xs={24} lg={8}>
            <Icon type="sync" />
            <Title level={4} style={{ color: "white" }}>
              Incremental
            </Title>
            <Paragraph style={{ color: "white" }}>
              By effectively taking into acount{" "}
              <b>only the lines of code that have changed</b>, our parser avoids
              wasting time and computer resource.
            </Paragraph>
          </Col>
          <Col xs={24} lg={8}>
            <Icon type="check" />
            <Title level={4} style={{ color: "white" }}>
              Smart
            </Title>
            <Paragraph style={{ color: "white" }}>
              Our parser can <b>understand wrong code</b>, which is vital when
              talking about code that is currently being written
            </Paragraph>
          </Col>
          <Col xs={24} lg={8}>
            <Icon type="gift" />
            <Title level={4} style={{ color: "white" }}>
              Free
            </Title>
            <Paragraph style={{ color: "white" }}>
              Just try it now, and get your hands on it. We are sure you will
              love it!
            </Paragraph>
          </Col>
        </Row>
        <Link to="/en/community" style={{ marginTop: 30 }}>
          Visit our dedicated page to see more
        </Link>
      </Section>
      <Section>
        <Title level={2} style={{ marginBottom: 30 }}>
          <span className="weight-300">TypeCobol is</span> Typed
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
          <span className="weight-300">TypeCobol has</span> procedures
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
          <span className="weight-300">TypeCobol has</span> everything you need
          in Cobol.
        </Title>
        <Link to="/en/community" style={{ marginTop: 30 }}>
          Visit our dedicated page to see more
        </Link>
      </Section>
      <Section>
        <Title level={1} style={{ marginBottom: 30 }}>
          <span className="weight-300">TypeCobol is</span> open source
        </Title>
        <a href="//github.com" target="_blank" rel="noopener noreferrer">
          <Icon type="github" style={{ fontSize: 50 }} />
        </a>
      </Section>
    </>
  )
}

export default Homepage
