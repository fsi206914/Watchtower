package dspa
import scala.collection.mutable._
import scala.collection.Seq;
import scala.io.Source

object Util{
	def uniform(num: Int) = {
		val numNode = Config.numNodes;
		val division = numNode/(num+3);

		/*
		 * retMap is the map of (object's location in a node, i)
		 */
		val retMap:Map[Int, Int] = new HashMap[Int, Int]
		for(i <- 1 to num){
			val n = i * division % numNode;
			retMap.put(n, i);
		}

		retMap
	}

	def readGraph(nodeFile:String, edgeFile:String) = {
		/*
		 * graph is a data structure for map, which maps NodeID to 
		 * the target Node.
		 * numNode, and numEges to do statistics of number of nodes and edges.
		 */
		val graph:Map[Int, GraphNode] = new HashMap[Int, GraphNode]
		var numNodes = 0
		var numEdges = 0

		// reading node file.
		for(line <- Source.fromFile(nodeFile).getLines()){
			val div = line.split(" ");
			val aNode = new GraphNode(div(0).toInt, java.lang.Float.parseFloat(div(1)), java.lang.Float.parseFloat(div(2)));
			graph.put(div(0).toInt, aNode);
			numNodes += 1
		}

		// reading edge file.
		for(line <- Source.fromFile(edgeFile).getLines()){
			val div = line.split(" ")

			val na = graph(div(1).toInt)
			val nb = graph(div(2).toInt)
			val oriEdgeID = div(0).toInt

			na.addEdge(new GraphEdge(oriEdgeID*2, na.name, 
									nb.name, java.lang.Float.parseFloat(div(3))))
			nb.addEdge(new GraphEdge(oriEdgeID*2 + 1, nb.name, 
									na.name, java.lang.Float.parseFloat(div(3))))
			numEdges += 1
			numEdges += 1
		}

		/*
		 * save numNodes and numEdges to singleton object Config
		 */
		Config.numNodes = numNodes;
		Config.numEdges = numEdges;

		graph
	}
}
