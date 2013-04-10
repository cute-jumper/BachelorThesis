package thu.ailab.global

import com.twitter.logging.Logger

trait LoggerTrait {
  protected val logger = Logger.get(getClass) 
}