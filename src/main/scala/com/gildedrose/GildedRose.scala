package com.gildedrose

class GildedRose(val items: Array[Item]) {

  // Code smell + refactoring in commit history

  private val warcraftItems = items.map(_.createItem)

  def updateQuality() {
    for (i <- warcraftItems.indices) {
      val item = warcraftItems(i)

      if (item.isNotAgedBrieOrSulfuras()) {
        if (item.quality > 0) {
          if (item.isNotSulfuras()) {
            item.quality = item.quality - 1
          }
        }
      } else {
        if (item.quality < 50) {
          item.quality = item.quality + 1

          if (item.isBackstagePasses()) {
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

      if (item.isNotSulfuras()) {
        item.decrementDaysTilExpiry()
      }

      if (item.daysTilExpiry < 0) {
        if (item.isNotAgedBrie()) {
          if (item.isNotBackStagePasses()) {
            if (item.quality > 0) {
              if (item.isNotSulfuras()) {
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
      items(i).fromWarcraftItem(warcraftItems(i))
    }
  }


}