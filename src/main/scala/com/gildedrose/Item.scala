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

  def updateItem(): Unit =
    updateQuality()
    updateDaysTilExpiry()

  def updateQuality(): Unit
  def updateDaysTilExpiry(): Unit = decrementDaysTilExpiry()

  def decrementDaysTilExpiry(): Unit = {
    daysTilExpiry = daysTilExpiry - 1
  }

  def qualityToZero(): Unit = {
    quality = 0
  }

  def decrementQuality(): Unit = {
    if (quality > 0) quality = quality - 1
  }

  def incrementQuality(): Unit = {
    if (quality < 50) quality = quality + 1
  }

}

case class SulfurasHandOfRagnaros(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Sulfuras, Hand of Ragnaros"

  def updateQuality(): Unit = ()
  override def updateDaysTilExpiry(): Unit = ()
}
case class AgedBrie(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Aged Brie"

  def updateQuality(): Unit = {
    incrementQuality()
    if (daysTilExpiry < 0) incrementQuality()
  }
}
case class BackstagePassesToATAFKAL80ETCconcert(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Backstage passes to a TAFKAL80ETC concert"

  def updateQuality(): Unit = {
    incrementQuality()
    if (daysTilExpiry < 11) incrementQuality()
    if (daysTilExpiry < 6)  incrementQuality()
    if (daysTilExpiry < 0)  qualityToZero()
  }
}
case class NormalItem(name: String, var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  def updateQuality(): Unit = {
    decrementQuality()
    if (daysTilExpiry < 0) decrementQuality()
  }
}