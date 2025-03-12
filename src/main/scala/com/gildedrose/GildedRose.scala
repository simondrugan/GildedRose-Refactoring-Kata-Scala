package com.gildedrose

class GildedRose(val items: Array[Item]) {

  // Code smell + refactoring in commit history

  private val warcraftItems = items.map(_.createItem)

  def updateQuality() {
    for (i <- warcraftItems.indices) {
      val item = warcraftItems(i)

      if (item.isNotAgedBrieOrBackstagePasses()) {
      } else {
        if (item.quality < 50) {
          if (item.isBackstagePasses()) {
          }
        }
      }

      item.updateDaysTilExpiry()


      item.updateQuality()
      items(i).fromWarcraftItem(warcraftItems(i))
    }
  }


}