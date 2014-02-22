package dspa
import scala.collection.mutable._
import scala.collection.Seq;

object Util{
	
	def uniform(graph: Map[Any, Node], num: Int) = {
		
		val numNode = graph.size;
		val division = numNode/(num+3);
		val retMap:Map[Node, Any] = new HashMap[Node, Any]
		val nodeSeq:Seq[Node] = graph.valuesIterator.toSeq
		for(i <- 1 to num){
			val n:Node = nodeSeq(i * division % numNode);
			retMap.put(n, i);
		}
		retMap
	}
}
