package com.yunjae.levenshtein

import scala.io.Source
import org.apache.commons.text.similarity.LevenshteinDistance

case class B2CItemInfo(itemCd: String, itemNm: String, cateNm: String, stdNm: String)
case class PosmallStdInfo(stdCd: String, stdNm: String, stdFullNm: String)

object LevenshteinMain extends App {
  val b2cItems = Source
    .fromResource("b2cItemList.txt")
    .getLines
    .map { items =>
      val item = items.split(s"\\t")
      B2CItemInfo(item(0), item(1), item(2), item(3))
    }.toList

  val posmallStd = Source
    .fromResource("posmallStdCode.txt")
    .getLines
    .map { items =>
      val item = items.split(s"\\t")
      PosmallStdInfo(item(0), item(1), item(2))
    }.toList

  def getTextSimilarity(tokenB: String, tokenQ: String): Double = {
    val lev = LevenshteinDistance.getDefaultInstance.apply(tokenB, tokenQ)
    lev.doubleValue() / Math.max(tokenB.length, tokenQ.length).doubleValue()
  }

  val results = b2cItems.map { b2cItem =>
    posmallStd
      .map { posItem =>
        val rate1 = getTextSimilarity(b2cItem.stdNm, posItem.stdFullNm)
        val rate2 = getTextSimilarity(b2cItem.cateNm, posItem.stdFullNm)
        val rate3 = getTextSimilarity(b2cItem.cateNm, posItem.stdNm)
        val rate4 = getTextSimilarity(b2cItem.stdNm, posItem.stdNm)
        val rate5 = getTextSimilarity(b2cItem.itemNm, posItem.stdNm)
        (b2cItem.itemCd, posItem.stdCd, rate1 + rate2 + rate3 + rate4 + rate5)
      }
      .minBy(_._3)
  }

  results.foreach { item =>
    println(s"UPDATE B2C_ITEM SET STD_CODE = '${item._2}' WHERE ITEM_CD = '${item._1}';")
  }


}
