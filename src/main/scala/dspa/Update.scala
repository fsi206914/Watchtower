package dspa
import scala.collection.mutable._
import scala.collection.immutable.List

object Update{

	var objMap: LinkedHashMap[Int, ObjectTuple] = null

	def AddEdge(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, len: Float){

		var edge = new GraphEdge(Config.numEdges, stNode, endNode, len)
		var reverseEdge = new GraphEdge(Config.numEdges+1, endNode, stNode, len)
		graph(stNode).addEdge(edge)
		graph(endNode).addEdge(reverseEdge)
		Config.numEdges += 2
		
		var wtMap: HashMap[Int, Watchtower] = new HashMap[Int, Watchtower]
		expand(graph, expansion, stNode, endNode, wtMap)
		expand(graph, expansion, endNode, stNode, wtMap)

		Config.numEdges -= 2
		graph(stNode).rmEdge(edge)
		graph(endNode).rmEdge(reverseEdge)
	}

	def rmEdge(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int){

		var edge = SaveInfo.findEdge(graph(stNode), graph(endNode))
		var reverseEdge = Deploy.findReverseEdge(edge)
		graph(stNode).rmEdge(edge)
		graph(endNode).rmEdge(reverseEdge)
		Config.numEdges -= 2
		
		var wtMap: HashMap[Int, Watchtower] = new HashMap[Int, Watchtower]
		expand(graph, expansion, stNode, endNode, wtMap)
		expand(graph, expansion, endNode, stNode, wtMap)

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
	}

	def expand(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, wtMap: HashMap[Int, Watchtower]){

		val pivot = graph(endNode)
		var queue = new PriorityQueue[GraphNode]()(QueueOrdering)
		graph(stNode).distance = 0.0
		queue += graph(stNode)

		var break = false
		while(queue.nonEmpty && break == false){
			val node= queue.dequeue
			if(node.distance > expansion) break = true;

			if(node.name != stNode && break == false){
				val prev = node.previous;
				if(metEdge(node, graph(stNode), pivot)){
					nodeIncor(prev, wtMap)
					edgeIncor(SaveInfo.findEdge(prev, node), wtMap)
				}
			}


			if(!node.visited && break==false){
				for(edge <- node.edges){
					val otherNode = graph(edge.endNode)
					if(!otherNode.visited && otherNode.distance > node.distance + edge.w){
						otherNode.distance = node.distance + edge.w;
						otherNode.previous = node
						queue += otherNode
					}
				}
				
				node.visited = true;
			}
		}
		
		/*
		 * re initialize attribute value in graphNode for next computation.
		 */
		for( (i,j) <- graph){
			j.distance = Double.MaxValue
			j.visited = false
			j.previous = null
		}
		
	}


	def metEdge(node: GraphNode, st: GraphNode, pivot: GraphNode): Boolean = {	
		
		if(node == null || node == st) true;
		if(node == pivot) false;
		metEdge(node.previous, st, pivot)
	}
	


	def nodeIncor(node: GraphNode, wtMap: HashMap[Int, Watchtower]){
		if(node.wt != null)
			if(!wtMap.contains(node.wt.ID))
				wtMap.update(node.wt.ID, node.wt)

	}

	def edgeIncor(edge: GraphEdge, wtMap: HashMap[Int, Watchtower]){
		for(wt <- edge.wtList)
			if(!wtMap.contains(wt.ID))
				wtMap.update(wt.ID, wt)
	}


	def QueueOrdering = new Ordering[GraphNode] {
		def compare(a:GraphNode, b:GraphNode) = a.compareTo(b)
	}
}
