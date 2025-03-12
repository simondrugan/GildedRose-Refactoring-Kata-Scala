package com.gildedrose

class GildedRose(val items: Array[Item]) {

  private val warcraftItems = items.map(_.createItem)

  def updateQuality() {
    for (i <- warcraftItems.indices) {
      val item = warcraftItems(i)

      item.updateItem()

      items(i).fromWarcraftItem(warcraftItems(i))
    }
  }

}