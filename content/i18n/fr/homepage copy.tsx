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
        <div
          style={{
            textAlign: "center",
            margin: "0 0 70px 0",
            top: "50vh",
            transform: "translateY(-50%)",
            position: "fixed",
            width: "100%",
            zIndex: 1,
          }}
        >
          <img
            style={{
              filter:
                "invert(99%) sepia(3%) saturate(0%) hue-rotate(46deg) brightness(117%) contrast(100%)",
            }}
            src={logo}
            alt="TypeCobol"
            className="tc-logo-home"
          />
          <Title level={3} style={{ marginTop: 0, fontWeight: 300 }}>
            Le <span style={{ fontWeight: 600 }}>nouveau</span> Cobol.
          </Title>
          <Title level={4} style={{ marginTop: 0, fontWeight: 300 }}>
            Un <span style={{ fontWeight: 600 }}>parser</span> incrémental pour{" "}
            <span style={{ fontWeight: 600 }}>IBM Enterprise Cobol 5.1</span>{" "}
            avec <span style={{ fontWeight: 600 }}>zOS Syntax</span>.
          </Title>
        </div>
        <Icon
          className="scrollDown"
          type="down"
          style={{
            position: "absolute",
            bottom: 10,
            fontSize: 35,
            left: "calc(50vw - 20px)",
          }}
        />
      </Section>
      <Section
        style={{
          background: "#001529",
          color: "white",
        }}
      >
        <Title level={3} style={{ color: "white", marginBottom: 30 }}>
          <span style={{ fontWeight: 300 }}>L&apos;</span>Initiative TypeCobol
        </Title>
        <Row
          gutter={{ xs: 8, sm: 16, md: 24, lg: 32 }}
          style={{ marginBottom: 25 }}
          justify="center"
          type="flex"
        >
          <Col style={{ width: 420, maxWidth: "100%" }}>
            <Icon type="border-outer" style={{ fontSize: 20 }} />
            <Title level={4} style={{ color: "white" }}>
              Un parser
            </Title>
            <Paragraph style={{ color: "white" }}>
              Un parser incrémental Cobol 85 ouvert étendu à la syntaxe de Cobol
              2002
            </Paragraph>
          </Col>
          <Col style={{ width: 420, maxWidth: "100%" }}>
            <Icon type="border-outer" style={{ fontSize: 20 }} />
            <Title level={4} style={{ color: "white" }}>
              Un <span style={{ fontWeight: 300 }}>sous-</span>langage
            </Title>
            <Paragraph style={{ color: "white" }}>
              Une extension du langage Cobol 85
            </Paragraph>
          </Col>
        </Row>
        <Row gutter={40} style={{ marginTop: 50 }}>
          <Col xs={24} lg={8}>
            <Icon type="sync" style={{ fontSize: 20 }} />
            <Title level={4} style={{ color: "white" }}>
              Incrémental
            </Title>
            <Paragraph style={{ color: "white" }}>
              En prenant en compte <b>seulement les lignes de code modifiées</b>
              , notre parser économise du temps et des ressources.
            </Paragraph>
          </Col>
          <Col xs={24} lg={8}>
            <Icon type="check" style={{ fontSize: 20 }} />
            <Title level={4} style={{ color: "white" }}>
              Malin
            </Title>
            <Paragraph style={{ color: "white" }}>
              Notre parser <b>comprend le mauvais code</b>, ce qui est vital
              lorsqu&apos;il s&apos;agit d&apos;écrire du code sain.
            </Paragraph>
          </Col>
          <Col xs={24} lg={8}>
            <Icon type="gift" style={{ fontSize: 20 }} />
            <Title level={4} style={{ color: "white" }}>
              Gratuit
            </Title>
            <Paragraph style={{ color: "white" }}>
              Essayez le maintenant. Nous sommes sûrs que allez l&apos;adorer!
            </Paragraph>
          </Col>
        </Row>
        <Link to="/en/community" style={{ marginTop: 30 }}>
          Visitez la page dédiée à l&apos;initiative Cobol
        </Link>
      </Section>
      <Section
        style={{
          background: "#f0f2f5",
          paddingTop: 50,
          paddingBottom: 50,
        }}
      >
        <Title level={2} style={{ marginBottom: 30 }}>
          <span style={{ fontWeight: 300 }}>TypeCobol est</span> typé
        </Title>
        <img
          src={imgTypedBefore}
          alt=""
          style={{ maxWidth: "100%", maxHeight: 300 }}
        />
        <Icon type="caret-right" style={{ fontSize: 50, color: "#182b3d" }} />
        <img
          src={imgTypedAfter}
          alt=""
          style={{ maxWidth: "100%", maxHeight: 300 }}
        />
      </Section>
      <Section
        style={{
          background: "#f0f2f5",
          paddingTop: 50,
          paddingBottom: 50,
        }}
      >
        <Title level={2} style={{ marginBottom: 30 }}>
          <span style={{ fontWeight: 300 }}>TypeCobol comprend les</span>{" "}
          procédures
        </Title>
        <img
          src={imgProceduresBefore}
          alt=""
          style={{ maxWidth: "100%", maxHeight: 300 }}
        />
        <Icon type="caret-right" style={{ fontSize: 50, color: "#182b3d" }} />
        <img
          src={imgProceduresAfter}
          alt=""
          style={{ maxWidth: "100%", maxHeight: 300 }}
        />
      </Section>
      <Section
        style={{
          background: "#001529",
          color: "white",
        }}
      >
        <Title level={3} style={{ color: "white", marginBottom: 30 }}>
          <span style={{ fontWeight: 300 }}>TypeCobol est</span> la réponse à
          vos problèmes de développement.
        </Title>
        <Link to="/en/community" style={{ marginTop: 30 }}>
          Visitez notre page dédiée pour en savoir plus
        </Link>
      </Section>
      <Section
        style={{
          background: "#f0f2f5",
          color: "white",
        }}
      >
        <Title level={1} style={{ marginBottom: 30 }}>
          <span style={{ fontWeight: 300 }}>TypeCobol est</span> open source
        </Title>
        <a href="//github.com" target="_blank" rel="noopener noreferrer">
          <Icon type="github" style={{ fontSize: 50 }} />
        </a>
      </Section>
    </>
  )
}

export default Homepage
