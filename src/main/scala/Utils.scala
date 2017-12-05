object Utils {
  def nonEmptyLine(line: String): Boolean = {
    !line.isEmpty && !line.forall(_.isWhitespace)
  }
}
