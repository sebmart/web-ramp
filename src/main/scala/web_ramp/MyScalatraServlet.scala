package web_ramp

import org.scalatra._
import web_ramp.Scenarios.CostSummary
import org.scalatra
import com.mongodb.casbah.commons.MongoDBObject
import com.novus.salat.global._
import com.novus.salat._
import com.mongodb.casbah.Imports._
import org.json4s._
import org.scalatra.json._
import edu.berkeley.path.ramp_metering._
import edu.berkeley.path.ramp_metering.io._
import edu.berkeley.path.ramp_metering.DifferentiableObjective._

case class DBScenario(scenario: FreewayScenario, name: String)

object Scenarios {
  val dir = "data/networks/"
  val nameMap = Map("small" -> "samitha1onramp", "big" -> "2On2Off", "I15" -> "I15", "half" -> "half")
  val scenarioMap = nameMap.map {
    case (k, v) => k -> LoadScenario.loadScenario("%s/%s.json" format(dir, v))
  }.toMap

  val optimizerMap = Map("ipopt" -> (() => new IpOptAdjointOptimizer), "multistart" -> (() => new MultiStartOptimizer(() => new Rprop)),
    "chained" -> (() => new ChainedOptimizer))

  case class CostSummary(ttt: Double, tMainline: Double, tQueue: Double)

  val goalMap = Map("ttt" -> ((s: FreewayScenario) => new TotalTravelTimeObjective(s)), "crit" -> ((s: FreewayScenario) => new CriticalDensityObjective(s)),
                    "minHalf1Dens" -> ((s: FreewayScenario) => CustomTTTObjective.minHalf1Dens(s)), "maxHalf" -> ((s: FreewayScenario) => CustomTTTObjective.maxHalf2Dens(s)),
                    "minMaxHalfDens" ->((s: FreewayScenario) => CustomTTTObjective.minMaxHalfDens(s)))
}

class NetworkSimulatorController(mongoColl: MongoCollection) extends ScalatraServlet with JacksonJsonSupport {
  Adjoint.optimizer = new MultiStartOptimizer(() => new Rprop)

  var topZoomFactor = 20

  // Before every action runs, set the content type to be in JSON format.
  before() {
  }

  error {
    case e => println("sh*t" + e.printStackTrace())
  }


  def simulationSummary(sim: SimulationOutput, scen: FreewayScenario, zoom: Int) = {
    Map("colorGraph" -> new SimulationGrapher(sim, scen, zoom).output, "costSummary" -> CostSummary(FreewaySimulator.totalTravelTime(scen.fw, sim, scen.policyParams.deltaTimeSeconds), FreewaySimulator.totalDensities(scen.fw, sim, scen.policyParams.deltaTimeSeconds), FreewaySimulator.totalQueues(scen.fw, sim, scen.policyParams.deltaTimeSeconds)))
  }

  def loadScenario(params: scalatra.Params) = {
    val oldScen = Scenarios.scenarioMap(params("network"))
    if (!params("edit").toBoolean) {
      oldScen
    } else {
      val fw = createFreeway(oldScen.fw, parse2dArray(params("freeway")))
      val policyParams = oldScen.policyParams
      val ic = oldScen.simParams.ic
      val demand = parse2dArray(params("demand"))
      val splits = parse2dArray(params("splits"))
      val bc = BoundaryConditions(demand, splits)
      val simParams = SimulationParameters(bc, ic)
      FreewayScenario(fw, simParams, policyParams)
    }
  }

  def createFreeway(oldFw: Freeway, array: IndexedSeq[IndexedSeq[Double]]) = {
    val links = array.tail.zip(oldFw.links).map{ case (row, link) => {
      FreewayLink(FundamentalDiagram(row(2), row(4), row(3)), row(1), link.rmax, link.p)
    }}
    Freeway(links, oldFw.onramps, oldFw.offramps)
  }

  get("/optimizers") {
    contentType = formats("json")
    Scenarios.optimizerMap.keys
  }
  get("/goals") {
    contentType = formats("json")
    Scenarios.goalMap.keys
  }



  get("/network-info") {
    contentType = formats("json")
    Scenarios.scenarioMap(params("network"))
  }

  def parse2dArray(string: String): IndexedSeq[IndexedSeq[Double]] = {
    parse(string).extract[List[List[Double]]].toIndexedSeq.map{_.toIndexedSeq}
  }

  get("/simulate") {
    contentType = formats("json")
    topZoomFactor = params("topZoomFactor").toInt
    val scen = loadScenario(params)
    simulationSummary(uncontrolledSim(scen), scen, topZoomFactor)
  }

  get("/optimize") {
    contentType = formats("json")
    val scen = loadScenario(params)
    topZoomFactor = params("topZoomFactor").toInt
    simulationSummary(optimalSim(scen, params), scen, topZoomFactor)
  }

  get("/mpc") {
    contentType = formats("json")
    val scen = loadScenario(params)
    topZoomFactor = params("topZoomFactor").toInt
    simulationSummary(mpcSim(scen, params), scen, topZoomFactor)
  }

  def uncontrolledSim(scen: FreewayScenario) = {
    val sim = new BufferCtmSimulator(scen)
    val control = FreewaySimulator.noControl(scen)
    sim.simulate(control)
  }

  def optimalSim(scen: FreewayScenario, params: scalatra.Params) = {
    val adjoint = optimizer(scen, params)
    new BufferCtmSimulator(scen).simulate(adjoint.givePolicy().flatten)
  }

  def mpcSim(scen: FreewayScenario, params: scalatra.Params) = {
    Adjoint.optimizer = Scenarios.optimizerMap(params("optimizer"))()
    val newIters = params("nIters").toInt
    StandardOptimizer.maxEvaluations = newIters
    MultiStartOptimizer.nStarts = params("nRestarts").toInt
    val mpcParams = ModelPredictiveControlParams(params("tHorizon").toInt, params("tUpdate").toInt, params("noiseFactor").toDouble)
    val mpc = new ModelPredictiveControl(scen, new AdjointRampMetering(scen), mpcParams)
    mpc.simulation
  }

  def optimizer(scen: FreewayScenario, params: scalatra.Params) = {
    Adjoint.optimizer = Scenarios.optimizerMap(params("optimizer"))()
    val newIters = params("nIters").toInt
    StandardOptimizer.maxEvaluations = newIters
    MultiStartOptimizer.nStarts = params("nRestarts").toInt
    val metering = new AdjointRampMetering(scen)
    metering.adjoint.diffObjective = Scenarios.goalMap(params("goal"))(scen)
    metering
  }


  get("/compare") {
    contentType = formats("json")
    val scen = loadScenario(params)
    val uncontrolled = uncontrolledSim(scen)
    val controlled = optimalSim(scen, params)
    new DiffGraph(scen.fw, controlled, uncontrolled, params("topZoomFactor").toInt).output
  }

  get("/loadscenariostodb") {
    Scenarios.scenarioMap.foreach{case (k,v) => mongoColl insert MongoDBObject("scenario" -> MongoDBObject("name" -> k, "value" -> grater[FreewayScenario].asDBObject(v)))}
  }

  get("/drop") {
    mongoColl.drop()
  }

  get("/networks") {
    contentType = formats("json")
    Scenarios.scenarioMap.keys
  }
  // Sets up automatic case class to JSON output serialization, required by
  // the JValueResult trait.
  protected implicit val jsonFormats: Formats = DefaultFormats

}
