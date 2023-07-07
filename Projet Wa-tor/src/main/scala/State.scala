import Main.{HEIGHT, PARTICULE_SIZE, WIDTH}
import scalafx.scene.paint.Color.{Green, Red}
import scalafx.scene.shape.Circle

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Random

case class State(canvaPoisson : ArrayBuffer[ArrayBuffer[Boolean]], canvaRequin : ArrayBuffer[ArrayBuffer[Boolean]], allPoisson: ListBuffer[Poisson], allRequin: ListBuffer[Requin]) {

  // Etat des poissons/requins
  // Permet de gérer les deux populations (grandir/détruire)
  def newState: State = {

    val board: Map[(Int, Int), FishType] =
      (allPoisson.map { poisson => (poisson.X, poisson.Y) -> FishType.POISSON } ++ allRequin.map { requin => (requin.X, requin.Y) -> FishType.REQUIN }).toMap


    // Changement des états pour les requins
    val newAllRequin = allRequin.flatMap { requin =>
      val neighbours = Random.shuffle(neighboursOf(requin.X, requin.Y, PARTICULE_SIZE))
      val freeNeighbour = neighbours.find { case (x: Int, y: Int) => !board.contains((x, y)) }
      val fishNeighbours = neighbours.find { case (x : Int, y : Int) => board.contains((x, y)) }


      fishNeighbours match {
        // si case non vide
        case Some((x, y)) if board.contains((x, y)) =>
          if (board.get((x, y)).contains(FishType.POISSON)) {
            List(requin.copy(rBreed = requin.rBreed + 1, rEnergy = requin.rEnergy + 100))
          } else {
            freeNeighbour match {
              case Some((freeX: Int, freeY: Int)) if requin.canBreed && requin.hasEnergy =>
                val parent = requin.copy(X = freeX, Y = freeY, rBreed = 1)
                val child = requin.copy(rBreed = 1, rEnergy = requin.rEnergy - 1)
                List(parent, child)
              case Some((freeX: Int, freeY: Int)) if requin.hasEnergy => List(requin.copy(X = freeX, Y = freeY, rBreed = requin.rBreed + 1, rEnergy = requin.rEnergy - 1))
              case _ => List()
            }
          }

          // Si case vide
        case None =>
          freeNeighbour match {
            case Some((freeX: Int, freeY: Int)) if requin.canBreed && requin.hasEnergy =>
              val parent = requin.copy(X = freeX, Y = freeY, sBreed = 1)
              val child = requin.copy(rBreed = 1, rEnergy = requin.rEnergy - 1)
              List(parent, child)
            case Some((freeX: Int, freeY: Int)) if requin.hasEnergy => List(requin.copy(X = freeX, Y = freeY, sBreed = requin.rBreed + 1, rEnergy = requin.rEnergy - 1))
            case _ => List()
          }
      }


    }

    // Changement des états pour les poissons
    val newAllPoisson = allPoisson.flatMap { poisson =>
      val neighbours = Random.shuffle(neighboursOf(poisson.X, poisson.Y, PARTICULE_SIZE))
      val freeNeighbour = neighbours.find { case (x: Int, y: Int) => !board.contains((x, y)) }
      val poissonNeighbours = neighbours.find { case (x: Int, y: Int) => board.contains((x, y)) }

      poissonNeighbours match {
        // Si cases non vides
        case Some((x, y)) if board.contains((x, y)) =>
          if (board.get((x, y)).contains(FishType.REQUIN)) {
            List()
          } else {
            freeNeighbour match {
              case Some((freeX: Int, freeY: Int)) if tuna.canBreed =>
                val parent = poisson.copy(X = freeX, Y = freeY, tBreed = 1)
                val child = poisson.copy(pBreed = 1)
                List(parent, child)
              case Some((freeX: Int, freeY: Int)) => List(poisson.copy(X = freeX, Y = freeY, pBreed = poisson.pBreed + 1))
              case _ => List(poisson)
            }
          }
          // si cases vides
        case None =>
          freeNeighbour match {
            case Some((freeX: Int, freeY: Int)) if poisson.canBreed =>
              val parent = poisson.copy(X = freeX, Y = freeY, pBreed = 1)
              val child = poisson.copy(pBreed = 1)
              List(parent, child)
            case Some((freeX: Int, freeY: Int)) => List(poisson.copy(X = freeX, Y = freeY, pBreed = poisson.pBreed + 1))
            case _ => List(poisson)
          }
      }
    }


    for (i <- 0 until WIDTH; j <- 0 until HEIGHT) {
      canvaPoisson(i)(j) = false
      canvaRequin(i)(j) = false
    }

    fillCanva(newAllPoisson, newAllRequin)
    State(canvaPoisson, canvaRequin, newAllPoisson, newAllRequin)
  }

  def draw(): ListBuffer[Circle] = {
    val poissonCircle : ListBuffer[Circle] = {
      allPoisson.map { cell =>
        cell.draw
      }
    }

    val requinCircle: ListBuffer[Circle] = {
      allRequin.map { cell =>
        cell.draw
      }
    }
    val particuleCircle: ListBuffer[Circle] = ListBuffer.concat(poissonCircle, requinCircle)
    particuleCircle
  }

  def fillCanva(newAllThon : ListBuffer[Poisson], newAllRequin : ListBuffer[Requin]): Unit = {
    newAllPoisson.foreach(p =>
      canvaPoisson(p.X)(p.Y) = true
    )

    newAllRequin.foreach(p =>
      canvaRequin(p.X)(p.Y) = true
    )
  }

  def neighboursOf(x: Int, y: Int, distance: Int): Seq[(Int, Int)] =
    for {
      i <- -distance to distance
      j <- -distance to distance if (i, j) != (0, 0) && (0 until WIDTH).contains(x + i) && (0 until HEIGHT).contains(y + j)
    } yield (x + i, y + j)
}