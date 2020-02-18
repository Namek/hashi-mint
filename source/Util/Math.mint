module Util.Math {
  fun rescale (
    fromLeft : Number,
    fromRight : Number,
    toLeft : Number,
    toRight : Number,
    value : Number
  ) : Number {
    (value - fromLeft) / fromWidth * toWidth + toLeft
  } where {
    fromWidth =
      fromRight - fromLeft

    toWidth =
      toRight - toLeft
  }
}
