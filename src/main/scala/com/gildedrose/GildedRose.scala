package com.gildedrose

class GildedRose(val items: Array[Item]) {

  // Code smell + refactoring in commit history

  private val warcraftItems = items.map(_.createItem)

  def updateQuality() {
    for (i <- warcraftItems.indices) {
      val item = warcraftItems(i)
      var quality = item.quality
      var expiry = item.daysTilExpiry

      if (isNotAgedBrieOrSulfuras(i)) {
        if (quality > 0) {
          if (isNotSulfuras(i)) {
            quality = quality - 1
          }
        }
      } else {
        if (quality < 50) {
          quality = quality + 1

          if (isBackstagePasses(i)) {
            if (expiry < 11) {
              if (quality < 50) {
                quality = quality + 1
              }
            }

            if (expiry < 6) {
              if (quality < 50) {
                quality = quality + 1
              }
            }
          }
        }
      }

      if (isNotSulfuras(i)) {
        expiry = expiry - 1
      }

      if (expiry < 0) {
        if (isNotAgedBrie(i)) {
          if (isNotBackStagePasses(i)) {
            if (quality > 0) {
              if (isNotSulfuras(i)) {
                quality = quality - 1
              }
            }
          } else {
            quality = quality - quality
          }
        } else {
          if (quality < 50) {
            quality = quality + 1
          }
        }
      }
      items(i).fromWarcraftItem(warcraftItems(i))
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