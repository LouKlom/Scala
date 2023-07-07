import scalafx.scene.shape.Circle
import scalafx.scene.paint.Color
import Main.PARTICULE_SIZE

case class Requin(X: Int, Y: Int, rBreed : Int, rEnergy : Int) {

  def draw: Circle = new Circle {
    centerX = X
    centerY = Y
    radius = PARTICULE_SIZE
    fill = Color.Green
  }
  
  def canBreed: Boolean = {
    if (rBreed == Main.rBreed) {
      true
    } else {
      false
    }
  }
  
  def hasEnergy : Boolean = {
    if (rEnergy == 0) {
      false
    } else {
      true
    }
  }
}