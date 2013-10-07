package web_ramp

/**
 * Created with IntelliJ IDEA.
 * User: jdr
 * Date: 10/3/13
 * Time: 6:46 PM
 * To change this template use File | Settings | File Templates.
 */

import edu.berkeley.path.ramp_metering.FreewayScenario

case class MetaDataColumn(name: String, label: String, datatype: String, editable: Boolean)
case class MetaData(columns: Seq[MetaDataColumn]) {
  lazy val names = columns.map{_.name}
  def processRow(row: DataRow) = {
    Map("values" -> names.zip(row.values.map{_.toString}).toMap, "id" ->row.id)
  }

  def processData(data: Data) = {
    data.rows.map{processRow _}
  }


}

case class DataRow(id: String, values: Seq[Any])
case class Data(rows: Seq[DataRow])

class EditableGrid(scenario: FreewayScenario) {
  type Grid = Map[String, Seq[Equals]]
  case class BoundaryConditions(demand: Grid, splits: Grid)
  case class Output(bc: BoundaryConditions)
  def gridify(array: Seq[Seq[Double]]): Grid = {
    val t = array.length
    if (t < 1) return Map("metadata" -> List(), "data" -> List())
    val n = array(0).length
    val metadata = MetaData((0 until n).map{i => {
      MetaDataColumn(i.toString,i.toString,"double", true)
    }})
    val data = metadata.processData(Data(array.zipWithIndex.map {case (row, i) => {
      DataRow(i.toString, row)
    }}))
    Map("metadata" -> metadata.columns, "data" -> data)
  }
  lazy val output = Output(BoundaryConditions(gridify(scenario.simParams.bc.demands), gridify(scenario.simParams.bc.splitRatios)))
}
