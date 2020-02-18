module Html.Extra {
  fun getOffsetPos (evt : Html.Event) : Vec2 {
    {
      x = evt.clientX - `#{rect}.left`,
      y = evt.clientY - `#{rect}.top`
    }
  } where {
    rect =
      `#{evt}.currentTarget.getBoundingClientRect()`
  }
}
