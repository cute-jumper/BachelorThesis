package thu.ailab.global

import com.twitter.logging.Logger

/**
 * Make it easy to mix-in a logger
 */
trait LoggerTrait {
  protected val logger = Logger.get(getClass) 
}