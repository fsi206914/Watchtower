package dspa
import scala.collection.mutable._
import scala.collection.immutable.List

object Search{

	var objMap: LinkedHashMap[Int, ObjectTuple] = null

	def uniformSearch(queryPoint: Int, graph: Map[Int, GraphNode], expansion: Float, kNN: Int) = {

		objMap = new LinkedHashMap[Int, ObjectTuple]

		/*
		 * our uniforma search operation is embedded in the Dijkstra's Algorithm.
		 */
		var queue = new PriorityQueue[GraphNode]()(QueueOrdering)
		graph(queryPoint).distance = 0.0
		queue += graph(queryPoint)

		var break = false
		while(queue.nonEmpty && break == false){
			val node= queue.dequeue
			if(node.distance > expansion) break = true;

			if(node.name != queryPoint && break == false){
				val prev = node.previous;
				wtSearch(prev, prev.distance)
				wtSearch(SaveInfo.findEdge(prev, node), prev.distance)
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

		println("The number of POI founded is " + objMap.size)
		
		/*
		 * re initialize attribute value in graphNode for next computation.
		 */
		for( (i,j) <- graph){
			j.distance = Double.MaxValue
			j.visited = false
			j.previous = null
		}

		val objTupleList: List[ObjectTuple] = objMap.values.toList.sortBy(_.dist)
		val kNNList = objTupleList.take( math.min(kNN, objTupleList.length))

		println(kNNList);
	}

	def wtSearch(x: Any, y: Double){x match{
		case i: GraphNode => nodeSearch(i, y)	
		case j: GraphEdge => edgeSearch(j, y)	
	}}


	def nodeSearch(node: GraphNode, y: Double){

		/*
		 * v+y represents the diatance from query point to watchtower, watchtower to object.
		 */
		if(node.wt != null){
			for( (k,v) <- node.wt.objTuple) update(k,v+y)
		}
	}

	def edgeSearch(edge: GraphEdge, y: Double){

		/*
		 * v+y represents the diatance from query point to srcNode next to watchtower, watchtower to object.
		 * edge.w-wt.srcCost represents the distance from srcNode to Watchtower.
		 */
		for(wt<- edge.wtList){
			for( (k,v) <- wt.objTuple) update(k, v + y + edge.w-wt.srcCost)
		}
	}


	def update(k: Int, v: Double){

		if(objMap.contains(k)){
			val prevNode = objMap(k)
			objMap(k).dist = math.min(prevNode.dist, v)
		}
		else{
			var objTuple = new ObjectTuple(k, v);
			objMap.update(k, objTuple)
		}
	}


	def QueueOrdering = new Ordering[GraphNode] {
		def compare(a:GraphNode, b:GraphNode) = a.compareTo(b)
	}
}
