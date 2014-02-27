package dspa
import scala.collection.mutable._
import scala.collection.immutable.List

object NaiveUpdate{

	var objMap: LinkedHashMap[Int, ObjectTuple] = null
	var countFalse = 0
	var countTrue = 0

	def AddEdge(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, len: Float, allObjMap: Map[Int, Int]){

		var edge = new GraphEdge(Config.numEdges, stNode, endNode, len)
		var reverseEdge = new GraphEdge(Config.numEdges+1, endNode, stNode, len)
		graph(stNode).addEdge(edge)
		graph(endNode).addEdge(reverseEdge)
		Config.numEdges += 2
		
		for((k,v) <- allObjMap) rmObject(graph, v, k)
		SaveInfo.selectWatchtower(allObjMap, graph, Config.Interval)

		Config.numEdges -= 2
		graph(stNode).rmEdge(edge)
		graph(endNode).rmEdge(reverseEdge)
		//println("true count = " + countTrue.toString + "true count = " + countTrue.toString  )
	}

	def rmEdge(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, allObjMap: Map[Int, Int]){

		var edge = SaveInfo.findEdge(graph(stNode), graph(endNode))
		var reverseEdge = Deploy.findReverseEdge(edge)
		graph(stNode).rmEdge(edge)
		graph(endNode).rmEdge(reverseEdge)
		Config.numEdges -= 2
		
		for((k,v) <- allObjMap) rmObject(graph, v, k)
		SaveInfo.selectWatchtower(allObjMap, graph, Config.Interval)

		Config.numEdges += 2
		graph(stNode).addEdge(edge)
		graph(endNode).addEdge(reverseEdge)
	}

	def addObject(graph: Map[Int, GraphNode], objID: Int, stNode: Int){
		val aObjMap: Map[Int, Int] = new HashMap[Int, Int]
		aObjMap += objID->stNode
		SaveInfo.selectWatchtower(aObjMap, graph, Config.Interval)
	}

	def rmObject(graph: Map[Int, GraphNode], objID: Int, stNode: Int){
		//TODO: Remove Object needs expanding the whole graph to find
		SaveInfo.rmWatchtower( (stNode, objID), graph);
	}

	def QueueOrdering = new Ordering[GraphNode] {
		def compare(a:GraphNode, b:GraphNode) = a.compareTo(b)
	}
}
