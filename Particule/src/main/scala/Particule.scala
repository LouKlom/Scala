import scalafx.scene.shape.Circle
import scalafx.scene.paint.Color
import Main.PARTICULE_SIZE

case class Particule(x: Int, y: Int, direction: Int, color: Color) {

  def draw: Circle = new Circle {
    centerX = x
    centerY = y
    radius = PARTICULE_SIZE
    fill = color
  }
}