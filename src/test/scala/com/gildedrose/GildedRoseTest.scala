package com.gildedrose

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GildedRoseTest extends AnyWordSpec with Matchers {

  // Use more domain language in test name
  // Increase sbt stryker to 100% to cover edge cases

  "updateItem" when {
    "name is: Aged Brie" when {
      "quality is less than 50" should {
        "increase quality by 1" in {
          val items = Array[Item](new Item("Aged Brie", 0, 49))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
        }
      }
      "quality is 50 or over" should {
        "don't increase quality" in {
          val items = Array[Item](new Item("Aged Brie", 0, 50))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
        }
      }
      "all circumstances" should {
        "reduce sellIn by 1" in {
          val items = Array[Item](new Item("Aged Brie", 1, 0))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).dayTilExpiry should equal(0)
        }
      }
      "sellIn is negative and quality is 49" should {
        "increase quality by 1" in {
          val items = Array[Item](new Item("Aged Brie", -1, 49))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
        }
      }
      "sellIn is negative and quality is below 49" should {
        "increase quality by 2" in {
          val items = Array[Item](
            new Item("Aged Brie", -1, 48),
            new Item("Aged Brie", -1, 10),
            new Item("Aged Brie", -1, 0),
            new Item("Aged Brie", -1, -5))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
          app.items(1).quality should equal(12)
          app.items(2).quality should equal(2)
          app.items(3).quality should equal(-3)
        }
      }
    }

    "name is: Backstage passes to a TAFKAL80ETC concert" when {
      "quality is less than 50 and sellIn is 11 or greater" should {
        "increase quality by 1" in {
          val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 11, 49))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
        }
      }
      "quality is less than 49 and sellIn is lower than 11" should {
        "increase quality by 2" in {
          val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 10, 48))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
        }
      }
      "quality is less than 48 and sellIn is lower than 6" should {
        "increase quality by 3" in {
          val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 5, 47))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(50)
        }
      }
      "all circumstances" should {
        "reduce sellIn by 1" in {
          val items = Array[Item](new Item("Backstage passes to a TAFKAL80ETC concert", 1, 0))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).dayTilExpiry should equal(0)
        }
      }
      "sellIn is negative and quality is above 50" should {
        "quality should be set to zero" in {
          val items = Array[Item](
            new Item("Backstage passes to a TAFKAL80ETC concert", -1, 51),
            new Item("Backstage passes to a TAFKAL80ETC concert", -1, 100)
          )

          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(0)
          app.items(1).quality should equal(0)
        }
      }
    }

    "name is: Sulfuras, Hand of Ragnaros" when {
      "always" should {
        "do nothing" in {
          val range = -100 to 100 by 1

          for {
            sellIn <- range
            quality <- range
          } {
            val items = Array[Item](
              new Item("Sulfuras, Hand of Ragnaros", sellIn, quality)
            )

            val app = new GildedRose(items)
            app.updateQuality()

            app.items(0).quality should equal(quality)
            app.items(0).dayTilExpiry should equal(sellIn)
          }
        }
      }
    }

    "name is: TestName" when {
      "sellIn is negative and quality is 1" should {
        "reduce quality by 1" in {
          val items = Array[Item](new Item("TestName", 0, 1))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(0)
        }
      }
      "sellIn is negative and quality is above 1" should {
        "reduce quality by 2" in {
          val items = Array[Item](new Item("TestName", -1, 2))
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(0)
        }
      }
      "sellIn is positive and quality is above 0" should {
        "reduce quality by 1" in {
          val items = Array[Item](
            new Item("TestName", 1, 1),
            new Item("TestName", 1, 10)
          )
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).quality should equal(0)
          app.items(1).quality should equal(9)
        }
      }
      "always" should {
        "reduce sellIn by 1" in {
          val items = Array[Item](
            new Item("TestName", -10, 0),
            new Item("TestName", 10, 0)
          )
          val app = new GildedRose(items)
          app.updateQuality()
          app.items(0).dayTilExpiry should equal(-11)
          app.items(1).dayTilExpiry should equal(9)
        }
      }
    }
  }
}