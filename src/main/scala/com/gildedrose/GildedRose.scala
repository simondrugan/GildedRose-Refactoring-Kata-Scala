package com.gildedrose

class GildedRose(val items: Array[Item]) {

  // Code smell + refactoring in commit history

  private val warcraftItems = items.map(_.createItem)

  def updateQuality() {
    for (i <- warcraftItems.indices) {
      val item = warcraftItems(i)
      if (isNotAgedBrieOrSulfuras(i)) {
        if (item.quality > 0) {
          if (isNotSulfuras(i)) {
            item.quality = item.quality - 1
          }
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1

          if (isBackstagePasses(i)) {
            if (item.daysTilExpiry < 11) {
              if (item.quality < 50) {
                item.quality = item.quality + 1
              }
            }

            if (item.daysTilExpiry < 6) {
              if (item.quality < 50) {
                item.quality = item.quality + 1
              }
            }
          }
        }
      }

      if (isNotSulfuras(i)) {
        item.daysTilExpiry = item.daysTilExpiry - 1
      }

      if (item.daysTilExpiry < 0) {
        if (isNotAgedBrie(i)) {
          if (isNotBackStagePasses(i)) {
            if (item.quality > 0) {
              if (isNotSulfuras(i)) {
                item.quality = item.quality - 1
              }
            }
          } else {
            item.quality = item.quality - item.quality
          }
        } else {
          if (item.quality < 50) {
            item.quality = item.quality + 1
          }
        }
      }
      items(i).quality        = warcraftItems(i).quality
      items(i).daysTilExpiry  = warcraftItems(i).daysTilExpiry
    }
  }

  private def isBackstagePasses(index: Int) = {
    warcraftItems(index).name.equals("Backstage passes to a TAFKAL80ETC concert")
  }

  private def isNotBackStagePasses(index: Int) = {
    !isBackstagePasses(index)
  }


  private def isNotAgedBrie(index: Int) = {
    !warcraftItems(index).name.equals("Aged Brie")
  }

  private def isNotSulfuras(index: Int) = {
    !warcraftItems(index).name.equals("Sulfuras, Hand of Ragnaros")
  }

  private def isNotAgedBrieOrSulfuras(index: Int): Boolean = {
    isNotAgedBrie(index) && isNotBackStagePasses(index)
  }
}