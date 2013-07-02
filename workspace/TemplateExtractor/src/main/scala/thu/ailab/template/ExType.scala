package thu.ailab.template

/**
 * Extraction field name.
 * 
 * Add new enum value if a new field is needed.
 */
object ExType extends Enumeration {
  type ExType = Value
  // Add any type you like
  val MAGIC, TITLE, AUTHOR, CONTENT = Value
}
