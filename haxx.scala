package org.ekrich.config.impl

import scala.jdk.CollectionConverters.*
import org.ekrich.config.impl.SimpleConfigOrigin

object Origin:
  def withComments(comments: Seq[String]): SimpleConfigOrigin =
    SimpleConfigOrigin.newSimple("").withComments(comments.asJava)

  def withComment(comment: String): SimpleConfigOrigin =
    SimpleConfigOrigin.newSimple("").withComments(comment.split("\n").toList.asJava)
