package it.unibo.pps.bunny.view.scalaFX.components.charts.pedigree

import it.unibo.pps.bunny.model.bunny.Bunny
import it.unibo.pps.bunny.view.scalaFX.components.charts.pedigree.PedigreeChart.SpacingGenerator
import scalafx.scene.image.{ Image, ImageView }
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.text.Text
import it.unibo.pps.bunny.view.scalaFX.ScalaFXConstants.GenealogicalTree._
import it.unibo.pps.bunny.view.scalaFX.components.charts.pedigree.PedigreeChart.bunnyIconSize
import it.unibo.pps.bunny.view.scalaFX.utilities._
import it.unibo.pps.bunny.view.scalaFX.utilities.DirectionUtils._
import it.unibo.pps.bunny.view.scalaFX.utilities.{ BunnyImageUtils, ImageType }

/**
 * Represents the view of a [[Bunny]] in a tree.
 */
trait BunnyPedigreeView {

  /** Reference to the model [[Bunny]] entity */
  val bunny: Bunny

  /** Pane with view of the [[Bunny]] */
  val pane: Pane
}

object BunnyPedigreeView {

  private val BunnyViewer: Bunny => HBox = bunny =>
    new HBox {

      children = Seq(
        SpacingGenerator(),
        new ImageView {
          image = BunnyImageUtils.bunnyToImage(bunny, ImageType.Normal)
          fitWidth = bunnyIconSize
          fitHeight = bunnyIconSize
          preserveRatio = true
          scaleX = scaleXValue(Right)
        },
        SpacingGenerator()
      )

    }

  private val GenderViewer: Bunny => HBox = bunny =>
    new HBox(
      SpacingGenerator(),
      new Text {
        text = bunny.gender.toString
        styleClass = Iterable("tree-bunny-text")
        style = "-fx-font-size: " + bunnyIconSize / BUNNY_FONT_PROPORTION + "px;"
        fill = Color.DimGray
      },
      SpacingGenerator()
    )

  private val AllelesViewer: Bunny => HBox = bunny =>
    new HBox(
      SpacingGenerator(),
      new Text {
        text = bunny.genotype.toString
        styleClass = Iterable("tree-bunny-text")
        style = "-fx-font-size: " + bunnyIconSize / BUNNY_FONT_PROPORTION + "px;"
      },
      SpacingGenerator()
    )

  private val DeadImageGenerator: () => ImageView = () => infoImage("/img/death.png")
  private val MutationImageGenerator: () => ImageView = () => infoImage("/img/mutation.png")

  private val InfoViewer: Bunny => HBox = bunny =>
    new HBox(
      SpacingGenerator(),
      if (bunny.alive) new Region() else DeadImageGenerator(),
      if (bunny.genotype.isJustMutated) MutationImageGenerator() else new Region(),
      SpacingGenerator()
    )

  def apply(bunny: Bunny): BunnyPedigreeView = BunnyPedigreeViewImpl(bunny)

  private def infoImage(path: String): ImageView = new ImageView {
    image = new Image(path)
    fitWidth = bunnyIconSize / BUNNY_INFO_PROPORTION
    fitHeight = bunnyIconSize / BUNNY_INFO_PROPORTION
  }

  private case class BunnyPedigreeViewImpl(override val bunny: Bunny) extends BunnyPedigreeView {
    override val pane: Pane = new VBox(BunnyViewer(bunny), GenderViewer(bunny), AllelesViewer(bunny), InfoViewer(bunny))
  }

}
