package linguistics


trait Noun {
  def singularNoun: String
  def pluralNoun: String
  def indefiniteArticle: String
}

trait Sentence {
  def numbered(noun: Noun, amount: Int) = amount match {
    case x if x == 0 => s"no ${noun.singularNoun.toLowerCase}"
    case x if x == 1 => s"${noun.indefiniteArticle} ${noun.singularNoun.toLowerCase}"
    case x if x >= 2 => s"$amount ${noun.pluralNoun.toLowerCase}"
  }
}

