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
              item.increaseQuality()
            }

            if (item.daysTilExpiry < 6) {
              item.increaseQuality()
            }
          }
        }
      }

      if (isNotSulfuras(i)) {
        item.decrementDaysTilExpiry()
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
          item.increaseQuality()
        }
      }
      items(i).fromWarcraftItem(warcraftItems(i))
    }
  }


}