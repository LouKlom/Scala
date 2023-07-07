import scalafx.scene.shape.Circle
import scalafx.scene.paint.Color
import Main.PARTICULE_SIZE

case class Poisson(X: Int, Y: Int, pBreed : Int) {

  def draw: Circle = new Circle {
    centerX = X
    centerY = Y
    radius = PARTICULE_SIZE
    fill = Color.Yellow
  }

  def canBreed : Boolean = {
    if (pBreed == Main.pBreed) {
      true
    } else {
      false
    }
  }
}