module Util.Render {
  fun circle (
    number : Number,
    posX : Number,
    posY : Number,
    isHovered : Bool,
    isFilled : Bool
  ) : Html {
    <g>
      <circle
        cx={Number.toString(posX)}
        cy={Number.toString(posY)}
        r={Number.toString(Const.circleRadius)}
        fill={fill}
        stroke={stroke}
        strokeWidth="0.5"/>

      <text
        x={Number.toString(posX)}
        y={Number.toString(posY + 1.5)}
        textAnchor="middle"
        dominantBaseline="central"
        fill={textFill}
        fontSize="#{Const.circleRadius}pt"
        fontFamily="Arial">

        <tspan>
          "#{number}"
        </tspan>

      </text>
    </g>
  } where {
    {fill, stroke, textFill} =
      if (!isHovered && isFilled) {
        {"#494850", "#494850", "#F8F8F8"}
      } else if (isHovered) {
        {"#228CDB", "#494850", "#F8F8F8"}
      } else {
        {"#F8F8F8", "#494850", "#494850"}
      }
  }

  fun line (
    startX : Number,
    startY : Number,
    endX : Number,
    endY : Number,
    lineStyle : String
  ) : Html {
    <line
      x1={Number.toString(startX)}
      y1={Number.toString(startY)}
      x2={Number.toString(endX)}
      y2={Number.toString(endY)}
      style={lineStyle}/>
  }

  fun color (r : Number, g : Number, b : Number) : String {
    "rgb(#{r}, #{g}, #{b})"
  }
}
