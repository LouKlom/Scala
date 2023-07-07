import javafx.scene.input.KeyCode
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.*
import scalafx.scene.shape.Circle

import java.awt.Toolkit
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends JFXApp3 {
  val nPoisson = 30
  val nRequin = 30
  val pBreed = 60
  val rBreed = 100
  val WIDTH = 200
  val HEIGHT = 200
  val PARTICULE_SIZE = 3
  val canvaPoisson: ArrayBuffer[ArrayBuffer[Boolean]] = ArrayBuffer.fill(WIDTH, HEIGHT)(false)
  val canvaRequin: ArrayBuffer[ArrayBuffer[Boolean]] = ArrayBuffer.fill(WIDTH, HEIGHT)(false)
  val allPoisson: ListBuffer[Poisson] = generatePoisson(nPoisson)
  val allRequin: ListBuffer[Shark] = generateRequin(nRequin)

  def generateRequin(n: Int): ListBuffer[Requin] = ListBuffer.fill(n)(0).map(_ =>

  val x = Random.nextInt(WIDTH)
  val y = Random.nextInt(HEIGHT)
  Requin(x, y, 1, 100)
  )

  def generatePoisson(n: Int): ListBuffer[Poisson] = ListBuffer.fill(n)(0).map(_ =>
    val x = Random.nextInt(WIDTH)
    val y = Random.nextInt(HEIGHT)
    Poisson(x, y, 1)
  )


  def gameLoop(update: () => Unit): Unit =
    def tick = Future {update(); Thread.sleep(50)}
    def loopAgain = Future(gameLoop(update))
    for {
      _ <- tick
      _ <- loopAgain
    } yield ()

  override def start(): Unit = {
    val state = ObjectProperty(State(canvaPoisson, canvaRequin, allPoisson, allRequin))
    val frame = IntegerProperty(0) //boucle

    frame.onChange {
      state.update(state.value.newState)
    }

    stage = new PrimaryStage {
      title = "Projet Wa-tor"
      width = WIDTH
      height = HEIGHT
      scene = new Scene {
        fill = Black
        content =
          allRequin.map(p => p.draw)
          allPoisson.map(p => p.draw)

        frame.onChange(Platform.runLater {
          content = state.value.draw()
        })
      }
    }
    gameLoop(() => frame.update(frame.value + 1))
  }
}
