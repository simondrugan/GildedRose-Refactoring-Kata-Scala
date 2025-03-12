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
    this.quality = warcraftItem.quality
    this.daysTilExpiry = warcraftItem.daysTilExpiry
  }

}

trait WarcraftItem {

  val name: String
  var daysTilExpiry: Int
  var quality: Int

  def updateQuality(): Unit
  def updateDaysTilExpiry(): Unit

  def decrementDaysTilExpiry(): Unit = {
    this.daysTilExpiry = this.daysTilExpiry - 1
  }

  def qualityToZero(): Unit = {
    this.quality = 0
  }

  def decrementQuality(): Unit = {
    this.quality = this.quality - 1
  }

  def incrementQuality(): Unit = {
    this.quality = this.quality + 1
  }

  def isBackstagePasses() = {
    this.name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  def isNotBackStagePasses() = {
    !this.isBackstagePasses()
  }

  def isNotAgedBrie() = {
    !this.name.equals("Aged Brie")
  }

  def isNotSulfuras() = {
    !this.name.equals("Sulfuras, Hand of Ragnaros")
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
    if (daysTilExpiry < 0 && quality < 50) this.incrementQuality()
    else ()
  }

  def updateDaysTilExpiry(): Unit = this.decrementDaysTilExpiry()
}
case class BackstagePassesToATAFKAL80ETCconcert(var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  val name = "Backstage passes to a TAFKAL80ETC concert"

  def updateQuality(): Unit = {
    if (daysTilExpiry < 0) this.qualityToZero()
    else ()
  }

  def updateDaysTilExpiry(): Unit = this.decrementDaysTilExpiry()
}
case class NormalItem(name: String, var daysTilExpiry: Int, var quality: Int) extends WarcraftItem {
  def updateQuality(): Unit = {
    if (daysTilExpiry < 0 && quality > 0) this.decrementQuality()
    else ()
  }

  def updateDaysTilExpiry(): Unit = this.decrementDaysTilExpiry()
}