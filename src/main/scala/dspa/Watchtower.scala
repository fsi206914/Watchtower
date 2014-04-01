package dspa
import scala.collection.mutable._

case class baseWatchtower(val ID: Int, val srcNode: Int, val srcCost: Float)

class Watchtower (val ID: Int, val srcNode: Int, val srcCost: Float)extends Serializable{
	
	//---objTuple is represented by[ObjectID, object distance]
	def this(bw: baseWatchtower) = this(bw.ID, bw.srcNode, bw.srcCost)

	var objTuple: Map[Int, Double] = new HashMap[Int, Double]

	def addObj(objID: Int, dist: Double) {
		objTuple.put(objID, dist);	
	}

	def rmObj(objID: Int) {
		objTuple.remove(objID);	
	}

	def update(objID: Int, dist: Double) {
		objTuple.update(objID,dist)
	}
}

