package dspa
import scala.collection.mutable._
import scala.collection.immutable.List

object Update{

	var countFalse = 0
	var countTrue = 0

	def AddEdge(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, len: Float, allObjMap: Map[Int, Int]){

		var edge = new GraphEdge(Config.numEdges, stNode, endNode, len)
		var reverseEdge = new GraphEdge(Config.numEdges+1, endNode, stNode, len)
		graph(stNode).addEdge(edge)
		graph(endNode).addEdge(reverseEdge)
		Config.numEdges += 2
		
		var wtMap: HashMap[Int, Watchtower] = new HashMap[Int, Watchtower]
		expand(graph, expansion, stNode, endNode, wtMap)
		expand(graph, expansion, endNode, stNode, wtMap)

	  reSelectWatchtower(graph, stNode, wtMap, allObjMap)

		Config.numEdges -= 2
		graph(stNode).rmEdge(edge)
		graph(endNode).rmEdge(reverseEdge)
		//println("true count = " + countTrue.toString + "true count = " + countTrue.toString  )

		println("how many wt affected = "+ wtMap.size)
	}

	def rmEdge(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, allObjMap: Map[Int, Int]){

		println("graph(stNode) = " + graph(stNode).toString + " " + graph(stNode).edges.toString)
		println("graph(endNode) = " + graph(endNode).toString + " " + graph(endNode).edges.toString)
		var edge = SaveInfo.findEdge(graph(stNode), graph(endNode))
		var reverseEdge = Deploy.findReverseEdge(edge)
		graph(stNode).rmEdge(edge)
		graph(endNode).rmEdge(reverseEdge)
		Config.numEdges -= 2
		
		var wtMap: HashMap[Int, Watchtower] = new HashMap[Int, Watchtower]
		expand(graph, expansion, stNode, endNode, wtMap)
		expand(graph, expansion, endNode, stNode, wtMap)

	  reSelectWatchtower(graph, stNode, wtMap, allObjMap)

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
		SaveInfo.rmWatchtower((stNode, objID), graph)
	}

	def expand(graph: Map[Int, GraphNode], expansion: Float, stNode: Int, endNode: Int, wtMap: HashMap[Int, Watchtower]){
		val pivot = graph(endNode)
		var queue = new PriorityQueue[GraphNode]()(QueueOrdering)
		graph(stNode).distance = 0.0
		queue += graph(stNode)

		var break = false
		while(queue.nonEmpty && break == false){
			val node= queue.dequeue
			//if(node.distance > expansion) break = true;

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

	def reSelectWatchtower(graph: Map[Int, GraphNode], stNode: Int, wtMap: HashMap[Int, Watchtower], allObjMap: Map[Int, Int]){

		for((k,v)<- allObjMap){
			var queue = new PriorityQueue[GraphNode]()(QueueOrdering)
			graph(stNode).distance = 0.0
			queue += graph(stNode)

			var break = false
			while(queue.nonEmpty && break == false){
				val node= queue.dequeue
				//if(node.distance > expansion) break = true;

				/*
				 * when this watchtower needes to be update, we update all distance tuples in every
				 * object in wt.
				 */
				if(node.name != stNode && break == false){
					val prev = node.previous;
					if( prev.wt != null && wtMap.contains(prev.wt.ID)){
						prev.wt.update(v, prev.distance)
						SaveInfo.findEdge(prev,node).update(v, prev.distance)
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
	}


	def metEdge(node: GraphNode, st: GraphNode, pivot: GraphNode): Boolean = {	
		var iterNode = node
		while(iterNode != null){
			if(iterNode == st) {
				countTrue += 1
				true
			}	
			if(iterNode == pivot){
				countFalse += 1
				false 
			}
			iterNode = iterNode.previous
		}
		return true
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
