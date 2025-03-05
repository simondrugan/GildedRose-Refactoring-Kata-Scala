package com.gildedrose

class GildedRose(val items: Array[Item]) {

  // Code smell + refactoring in commit history

  private val warcraftItems = items.map(_.createItem)
  def updateQuality() {
    for (i <- warcraftItems.indices) {
      if (isNotAgedBrieOrSulfuras(i)) {
        if (items(i).quality > 0) {
          if (isNotSulfuras(i)) {
            items(i).quality = items(i).quality - 1
          }
        }
      } else {
        if (items(i).quality < 50) {
          items(i).quality = items(i).quality + 1

          if (isBackstagePasses(i)) {
            if (items(i).daysTilExpiry < 11) {
              if (items(i).quality < 50) {
                items(i).quality = items(i).quality + 1
              }
            }

            if (items(i).daysTilExpiry < 6) {
              if (items(i).quality < 50) {
                items(i).quality = items(i).quality + 1
              }
            }
          }
        }
      }

      if (isNotSulfuras(i)) {
        items(i).daysTilExpiry = items(i).daysTilExpiry - 1
      }

      if (items(i).daysTilExpiry < 0) {
        if (isNotAgedBrie(i)) {
          if (isNotBackStagePasses(i)) {
            if (items(i).quality > 0) {
              if (isNotSulfuras(i)) {
                items(i).quality = items(i).quality - 1
              }
            }
          } else {
            items(i).quality = items(i).quality - items(i).quality
          }
        } else {
          if (items(i).quality < 50) {
            items(i).quality = items(i).quality + 1
          }
        }
      }
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