import * as React from "react"

interface FlagProps {
  iso: string
  size: number
  style?: React.CSSProperties
  onClick?: (action: any) => void
}

const Flag: React.FunctionComponent<FlagProps> = (props: FlagProps) => {
  const { iso, size, style, onClick } = props
  const flagUrl = `https://unpkg.com/svg-country-flags@latest/svg/${iso}.svg`
  return (
    // eslint-disable-next-line
    <span
      role="img"
      style={{
        position: "relative",
        display: "inline-block",
        width: size * 3,
        height: size,
        backgroundImage: `url(${flagUrl})`,
        backgroundPosition: "center",
        backgroundRepeat: "no-repeat",
        backgroundSize: "contain",
        fontSize: size * 2,
        lineHeight: size * 2,
        verticalAlign: "middle",
        ...style,
      }}
      onClick={onClick || null}
      title={iso}
    />
  )
}

export default Flag
