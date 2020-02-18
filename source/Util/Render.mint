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
        stroke={color(0, 0, 0)}/>

      <text
        x={Number.toString(posX)}
        y={Number.toString(posY)}
        textAnchor="middle"
        dominantBaseline="central"
        fontSize="#{Const.circleRadius}pt">

        <tspan>
          "#{number}"
        </tspan>

      </text>
    </g>
  } where {
    fill =
      if (!isHovered && isFilled) {
        color(127, 127, 127)
      } else if (isHovered) {
        color(255, 255, 255)
      } else {
        color(255, 0, 0)
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
