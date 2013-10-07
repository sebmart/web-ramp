package web_ramp

import edu.berkeley.path.ramp_metering.{Freeway, FreewayScenario, SimulationOutput}


object SimulationGrapher {
  type ColorGraph = Array[Array[Double]]
  val kInvalidValue = -1

  case class SimulationGrapherOutput(density: ColorGraph, queue: ColorGraph)

  def resify(graph: ColorGraph, resolution: Int): ColorGraph = {
    val tGroup = math.max(1, graph.length / resolution)
    val xGroup = math.max(1, graph(0).length / resolution)
    val first = if (xGroup < 2) graph
    else {
      graph.map {
        _.grouped(xGroup).map(mean).toArray
      }
    }
    if (tGroup < 2) first
    else {
      first.transpose.map {
        _.grouped(tGroup).map {
          mean
        }.toArray
      }.transpose
    }
  }

  def mean(arr: Array[Double]) = {
    val x = arr.filter {
      _ >= 0
    }
    if (x.size == 0) {
      kInvalidValue.toDouble
    } else {
      x.sum / x.size
    }
  }

}

import SimulationGrapher._


class SimulationGrapher(simulation: SimulationOutput, scenario: FreewayScenario, resolution: Int = 100) {
  lazy val density = {
    resify(simulation.density.map {
      _.zip(scenario.fw.rhoMaxs).map {
        case (rho, rhoMax) => rho / rhoMax
      }
    }, resolution)
  }


  lazy val queue = {
    val maxQueue = simulation.queue.flatten.max
    resify(simulation.queue.map {
      _.zip(scenario.fw.onrampIndex.map {
        _ + 1
      }).map {
        case (q, or) => {
          if (or < 0) kInvalidValue else q / maxQueue
        }
      }
    }, resolution)
  }
  lazy val output = SimulationGrapherOutput(density, queue)
}

class DiffGraph(fw: Freeway, controlled: SimulationOutput, uncontrolled: SimulationOutput, resolution: Int) {
  val densityDiff = diff(controlled.density, uncontrolled.density)
  val queueDiff = diff(controlled.queue, uncontrolled.queue)

  case class DiffGraphOutput(density: ColorGraph, queue: ColorGraph)

  def diff(a: Array[Array[Double]], b: Array[Array[Double]]) = {
    a.zip(b).map {
      case (c, u) => {
        c.zip(u).map {
          case (cc, uu) => cc - uu
        }
      }
    }
  }

  lazy val density = {
    resify(densityDiff.map {
      _.zip(fw.rhoMaxs).map {
        case (rho, rhoMax) => rho / rhoMax
      }
    }, resolution)
  }

  lazy val queue = {
    val maxDiff = queueDiff.flatten.map{math.abs(_)}.max
    resify(queueDiff.map {
      _.map {_ / maxDiff}
    }, resolution)
  }

  lazy val output = DiffGraphOutput(density, queue)
}
