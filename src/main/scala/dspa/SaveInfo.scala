package dspa
import scala.collection.mutable._

object SaveInfo{

	val wtMap: Map[Int, Watchtower] = new HashMap[Int, Watchtower]
	var wtCount = 0
	

	def selectWatchtower(objectMap: Map[Int, Int], graph: Map[Int, GraphNode], interval: Int) = {
		wtCount = 0

		/*
		 * our selectwatchtower operation is embedded in the Dijkstra's Algorithm.
		 */
		for( (k,v) <-objectMap ){
			
			val startNode = k
			val objectID = v

			val queue = new PriorityQueue[GraphNode]()(QueueOrdering)
			graph(startNode).distance = 0.0
			queue += graph(startNode)

			while(queue.nonEmpty && queue.head.distance < Config.bd){
				val node= queue.dequeue

				if(node.name != startNode){
					val prev = node.previous;
					if(prev == null) throw new Exception("prev can not be null")
					
					/*
					 * we set up a watchtower in a node, if interval requirement is met.
					 */
					if(prev.bw != null){
						prev.selectRemain += 1
						if(prev.selectRemain >= interval){
							if(prev.wt == null)
								prev.wt = new Watchtower(prev.bw)

							prev.wt.addObj(objectID, prev.distance)
							prev.selectRemain = 0
							wtCount += 1
						}
					}

					/*
					 * set up watchtowers on edges.
					 */

					node.selectRemain = setInfoEdge(prev, node, prev.selectRemain, interval, prev.distance, objectID)
				}

				if(!node.visited){
					
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


	def rmWatchtower(objTuple: (Int, Int), graph: Map[Int, GraphNode]) = {
		wtCount = 0

		/*
		 * our selectwatchtower operation is embedded in the Dijkstra's Algorithm.
		 */
			
			val startNode = objTuple._1
			val objectID = objTuple._2

			val queue = new PriorityQueue[GraphNode]()(QueueOrdering)
			graph(startNode).distance = 0.0
			queue += graph(startNode)

			while(queue.nonEmpty){
				val node= queue.dequeue

				if(node.name != startNode){
					val prev = node.previous;
					if(prev == null) throw new Exception("prev can not be null")
					
					/*
					 * if there's a watchtower in a node, we remove the object from the watchtower.
					 */
					if(prev.wt!= null)
						prev.wt.rmObj(objectID);


					/*
					 * remove watchtowers on edge and its reverse edge.
					 */
					var edge = findEdge(prev,node)
					edge.rmObj(objectID)
					Deploy.findReverseEdge(edge).rmObj(objectID)
				}

				if(!node.visited){
					
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


	def setInfoEdge(start: GraphNode, end: GraphNode, initInterval: Int, interval: Int, initDist: Double, objID: Int): Int = {
		val edge = findEdge(start, end);
		val alterEdge = Deploy.findReverseEdge(edge);
		var aggregateInterval = initInterval;

		if(edge.bwList.length > 0){
			for(bw <- edge.bwList){
				aggregateInterval += 1

				if(aggregateInterval >= interval){
				
					//  watchtowers are set up on the dge and the reverse edge	
					var wt:Watchtower = null
					if(!wtMap.contains(bw.ID)){
						wt = new Watchtower(bw) 
						edge.addWT(wt)
						alterEdge.addWT(wt)
					}
					else
						wt = wtMap(bw.ID)

					wt.addObj(objID, initDist+bw.srcCost)
					wtCount += 1
					aggregateInterval= 0
				}
			}
		}
		aggregateInterval
	}


	def findEdge(start: GraphNode, end: GraphNode) = {
		val t = start.edges.filter{case n => n.endNode == end.name}.toList
		t.head
	}


	def QueueOrdering = new Ordering[GraphNode] {
		def compare(a:GraphNode, b:GraphNode) = a.compareTo(b)
	}
}
