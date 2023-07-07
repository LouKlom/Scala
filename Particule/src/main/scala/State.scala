import Main.{HEIGHT, PARTICULE_SIZE, WIDTH}
import scalafx.scene.paint.Color.{Green, Red}
import scalafx.scene.shape.Circle

import scala.collection.mutable.ArrayBuffer

case class State(canva : ArrayBuffer[ArrayBuffer[Boolean]], allParticules: List[Particule]) {

  def newState = {
    val newAllParticule = allParticules.map(p =>
      p.direction match
        case 0 => if (collision(p.x, p.y, p.direction)) {
          p.copy(y = p.y, direction = 1)
        } else {
          if (p.y - PARTICULE_SIZE < 0) {
            p.copy(y = p.y, direction = 1)
          } else {
            p.copy(y = p.y - PARTICULE_SIZE)
          }
        }
        case 1 => if (collision(p.x, p.y, p.direction)) {
          p.copy(y = p.y, direction = 0)
        } else {
          if (p.y + PARTICULE_SIZE > HEIGHT - 1) {
            p.copy(y = p.y, direction = 0)
          } else {
            p.copy(y = p.y + PARTICULE_SIZE)
          }
        }
        case 2 => if (collision(p.x, p.y, p.direction)) {
          p.copy(x = p.x, direction = 3)
        } else {
          if (p.x - PARTICULE_SIZE < 0) {
            p.copy(x = p.x, direction = 3)
          } else {
            p.copy(x = p.x - PARTICULE_SIZE)
          }
        }
        case 3 => if (collision(p.x, p.y, p.direction)) {
          p.copy(x = p.x, direction = 2)
        } else {
          if (p.x + PARTICULE_SIZE > WIDTH - 1) {
            p.copy(x = p.x, direction = 2)
          } else {
            p.copy(x = p.x + PARTICULE_SIZE)
          }
        }
    )
    for (i <- 0 until WIDTH; j <- 0 until HEIGHT) {
      canva(i)(j) = false
    }
    fillCanva(newAllParticule)
    State(canva, newAllParticule)
  }

  def draw(): List[Circle] = {
    val particuleCircle: List[Circle] = {
      allParticules.map { cell =>
        cell.draw
      }
    }
    particuleCircle
  }

  def fillCanva(newAllParticule : List[Particule]): Unit = {
    newAllParticule.foreach(p =>
      canva(p.x)(p.y) = true
    )
  }

  def collision(x: Int, y: Int, direction: Int): Boolean = {
    direction match
      case 0 => if (y - PARTICULE_SIZE < 0) false else if (canva(x)(y - PARTICULE_SIZE)) true else false
      case 1 => if (y + PARTICULE_SIZE > HEIGHT - 1) false else if (canva(x)(y + PARTICULE_SIZE)) true else false
      case 2 => if (x - PARTICULE_SIZE < 0) false else if (canva(x - PARTICULE_SIZE)(y)) true else false
      case 3 => if (x + PARTICULE_SIZE > WIDTH - 1) false else if (canva(x + PARTICULE_SIZE)(y)) true else false
  }
}