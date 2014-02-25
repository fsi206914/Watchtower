package dspa
import scala.collection.mutable._

object Search{

	var objMap: LinkedHashMap[Int, ObjectTuple] = null
	//val objQueue: PriorityQueue[ObjectTuple] = null

	def uniformSearch(queryPoint: Int, graph: Map[Int, GraphNode], expansion: Float) = {

		objMap = new LinkedHashMap[Int, ObjectTuple]
		//objQueue = new PriorityQueue[ObjectTuple]()(objOrdering)
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
				wtSearch(prev)
				wtSearch(SaveInfo.findEdge(prev, node))
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

	def wtSearch(x: Any){x match{
		case i: GraphNode => nodeSearch(i)	
		case j: GraphEdge => edgeSearch(j)	
	}}


	def nodeSearch(node: GraphNode){

		if(node.wt != null){
			for( (k,v) <- node.wt.objTuple) update(k,v)
		}
	}

	def edgeSearch(edge: GraphEdge){

		for(wt<- edge.wtList){
			for( (k,v) <- wt.objTuple) update(k,v)
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

	def ObjOrdering = new Ordering[ObjectTuple] {
		def compare(a:ObjectTuple, b:ObjectTuple) = a.compareTo(b)
	}
}
