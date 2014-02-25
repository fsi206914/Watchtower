package dspa
import scala.collection.mutable._

object SaveInfo{
	
	def selectWatchtower(objectMap: Map[Int, Int], graph: Map[Int, GraphNode], interval: Int) = {

		/*
		 * our selectwatchtower operation is embedded in the Dijkstra's Algorithm.
		 */
		for( (k,v) <-objectMap ){
			
			val startNode = k
			val objectID = v

			val queue = new PriorityQueue[GraphNode]()(QueueOrdering)
			queue += graph(startNode)

			while(queue.nonEmpty){
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
							prev.wt = new Watchtower(prev.bw)
							prev.wt.addObj(objectID, prev.dist)
						}
					}

					/*
					 * set up watchtowers on edges.
					 */

					setInfoEdge(prev, node)

					
				}

				if(!node.visited){
					
					for(edge <- node.edges){
							
						val otherNode = graph(edge.endNode)
						if(otherNode.visited && otherNode.distance > node.distance + edge.w){
							
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

	def setInfoEdge(start: GraphNode, end: GraphNode) {

		val edge = findEdge(start, end);
		val alterEdge = Deploy.findReverseEdge(edge);

		if(edge.bwList.length > 0){
			
			
		}

	}



	def findEdge(start: GraphNode, end: GraphNode) = {
		val t = start.edges.filter{case n => n.endNode == end.name}.toList
		t.head
	}


	def QueueOrdering = new Ordering[GraphNode] {
		def compare(a:GraphNode, b:GraphNode) = a.compareTo(b)
	}

}
