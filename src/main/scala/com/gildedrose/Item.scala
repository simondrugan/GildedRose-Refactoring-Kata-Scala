package com.gildedrose


class Item(val name: String, var daysTilExpiry: Int, var quality: Int) {
  def createItem: WarcraftItem = {
    name match {
      case "Sulfuras, Hand of Ragnaros" => SulfurasHandOfRagnaros(daysTilExpiry = daysTilExpiry, quality = quality)
      case "Aged Brie" => AgedBrie(daysTilExpiry = daysTilExpiry, quality = quality)
      case "Backstage passes to a TAFKAL80ETC concert" => BackstagePassesToATAFKAL80ETCconcert(daysTilExpiry = daysTilExpiry, quality = quality)
      case _ => NormalItem(name = name, daysTilExpiry = daysTilExpiry, quality = quality)
    }
  }

  def fromWarcraftItem(warcraftItem: WarcraftItem): Unit = {
    quality = warcraftItem.quality
    daysTilExpiry = warcraftItem.daysTilExpiry
  }

}

trait WarcraftItem {

  val name: String
  var daysTilExpiry: Int
  var quality: Int

  def updateQuality(): Unit
  def updateDaysTilExpiry(): Unit

  def decrementDaysTilExpiry(): Unit = {
    daysTilExpiry = daysTilExpiry - 1
  }

  def qualityToZero(): Unit = {
    quality = 0
  }

  def decrementQuality(): Unit = {
    quality = quality - 1
  }

  def incrementQuality(): Unit = {
    quality = quality + 1
  }

  def isBackstagePasses() = {
    name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  def isNotBackStagePasses() = {
    !isBackstagePasses()
  }

  def isNotAgedBrie() = {
    !name.equals("Aged Brie")
  }

  def isNotSulfuras() = {
    !name.equals("Sulfuras, Hand of Ragnaros")
  }

  def isNotAgedBrieOrSulfuras(): Boolean = {
    isNotAgedBrie() && isNotBackStagePasses()
  }
}

case class SulfurasHandOfRagnaros(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Sulfuras, Hand of Ragnaros"

  def updateQuality(): Unit = ()

  def updateDaysTilExpiry(): Unit = ()
}
case class AgedBrie(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Aged Brie"

  def updateQuality(): Unit = {
    if (daysTilExpiry < 0 && quality < 50) incrementQuality()
    else ()
  }

  def updateDaysTilExpiry(): Unit = decrementDaysTilExpiry()
}
case class BackstagePassesToATAFKAL80ETCconcert(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Backstage passes to a TAFKAL80ETC concert"

  def updateQuality(): Unit = {
    if (daysTilExpiry < 0) qualityToZero()
    else ()
  }

  def updateDaysTilExpiry(): Unit = decrementDaysTilExpiry()
}
case class NormalItem(name: String, var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  def updateQuality(): Unit = {
    if (daysTilExpiry < 0 && quality > 0) decrementQuality()
    else ()
  }

  def updateDaysTilExpiry(): Unit = decrementDaysTilExpiry()
}