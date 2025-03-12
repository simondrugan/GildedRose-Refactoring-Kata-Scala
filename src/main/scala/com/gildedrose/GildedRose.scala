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
            item.decrementQuality()
          }
        }
      } else {
        if (item.quality < 50) {
          item.incrementQuality()

          if (item.isBackstagePasses()) {
            if (item.daysTilExpiry < 11) {
              if (item.quality < 50) {
                item.incrementQuality()
              }
            }

            if (item.daysTilExpiry < 6) {
              if (item.quality < 50) {
                item.incrementQuality()
              }
            }
          }
        }
      }

      item.updateDaysTilExpiry()


      item.updateQuality()
      items(i).fromWarcraftItem(warcraftItems(i))
    }
  }


}