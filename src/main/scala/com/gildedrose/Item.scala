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
}

trait WarcraftItem {

  val name: String
  var daysTilExpiry: Int
  var quality: Int

  def updateQuality(): Unit
  def updateDaysTilExpiry(): Unit

}

case class SulfurasHandOfRagnaros(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Sulfuras, Hand of Ragnaros"

  def updateQuality(): Unit = ()

  def updateDaysTilExpiry(): Unit = ()
}
case class AgedBrie(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Aged Brie"

  def updateQuality(): Unit = ()

  def updateDaysTilExpiry(): Unit = ()
}
case class BackstagePassesToATAFKAL80ETCconcert(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Backstage passes to a TAFKAL80ETC concert"

  def updateQuality(): Unit = ()

  def updateDaysTilExpiry(): Unit = ()
}
case class NormalItem(name: String, var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  def updateQuality(): Unit = ()

  def updateDaysTilExpiry(): Unit = ()
}