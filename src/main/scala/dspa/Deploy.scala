package dspa
import scala.collection.mutable._
import scala.collection.Seq;
import scala.io.Source

object Deploy{
	
	def AnchorPointDeploy(graph: Map[Int, GraphNode] ){

		val degBig3:List[GraphNode] =  graph.filter{ case (k,v) => v.edges.length > 2}.values.toList
		for(node <- degBig3){
			
		}
	}
}
