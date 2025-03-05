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

}

case class SulfurasHandOfRagnaros(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Sulfuras, Hand of Ragnaros"
}
case class AgedBrie(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Aged Brie"
}
case class BackstagePassesToATAFKAL80ETCconcert(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Backstage passes to a TAFKAL80ETC concert"
}
case class NormalItem(name: String, var daysTilExpiry: Int, var quality: Int) extends WarcraftItem