package dspa
import scala.collection.mutable._
import scala.collection.Seq;
import scala.io.Source


/**
 * Object Deploy is in charge of assigning base watchtowers evenly
 * in the graph. 
 */
object Deploy{

	var numBW = 0
	var edgeVisited: ArraySeq[Boolean] = null
	var currGraph: Map[Int, GraphNode] = null
	var lambda:Float = 0

	def anchorPointDeploy(graph: Map[Int, GraphNode], aLambda: Float ){

		currGraph = graph
		lambda = aLambda

		/*
		 * degBig3 is a List which contains all nodes whose degree larger than 2.
		 */
		val degBig3:List[GraphNode] =  graph.filter{ case (k,v) => v.edges.length > 2}.values.toList
		edgeVisited = new ArraySeq[Boolean](Config.numEdges);

		for(node <- degBig3){
			// contruct base watchtowers on every big degree 3 node.
			node.bw = baseWatchtower(numBW, node.name, 0)
			numBW += 1
			for(edge <- node.edges){
				setEdges(edge, lambda)
			}
		}
	}


	/*
	 * setEdges is the method that it deploy watchtowers every lambda
	 * distance along the edges.
	 */
	def setEdges(edge: GraphEdge, remain: Float){
		
		if(edgeVisited(edge.name) == false){
			edgeVisited(edge.name) = true;

			if(edge.name % 2 ==0) edgeVisited(edge.name+1) = true
			else edgeVisited(edge.name-1) = true

			var nextRemain = remain+edge.w

			if(nextRemain > lambda){
				nextRemain = setOneEdge(edge, remain)	
			}

			if(currGraph(edge.endNode).edges.length == 2 )
				setEdges(nextEdge(edge), nextRemain)
		}
	}

	/*
	 * construct watchtowers in edge and its reverse edge, append
	 * watchtowers to edges.
	 */
	def setOneEdge(edge: GraphEdge, remain: Float) = {

		var nextRemain = 0	
		val alterEdge = findReverseEdge(edge)
		var endPos = edge.w
		var startPos: Float = 0
		var retRemain = remain

		while(startPos < endPos){
			
			val nextPos = startPos + lambda - retRemain 
			if(nextPos < endPos){
				val bw = baseWatchtower(numBW, edge.startNode, nextPos)
				val bw2 = baseWatchtower(numBW, edge.endNode, edge.w-nextPos)
				edge.addBW(bw)
				alterEdge.addBW(bw2)

				numBW += 1
				retRemain = 0
			}
			else
				retRemain = endPos - startPos

			startPos = nextPos
		}
		retRemain
	}


	/**
	 * In this case, expansion always meets 2-degree nodes.
	 */
	def nextEdge(edge: GraphEdge) = {
		
		val currStart = edge.startNode	
		val currEnd = edge.endNode
		val node = currGraph(currEnd)

		val t = node.edges.filter{case n => n.endNode != currStart}.toList
		t.head
	}


	/**
	 * In our design scenario, undirected edges are composed an edge an reverse edge.
	 */
	def findReverseEdge(edge: GraphEdge) = {

		val startNode = edge.startNode
		val endNode = edge.endNode
		val node = currGraph(endNode)

		val t = node.edges.filter{case n => n.endNode == startNode}.toList
		t.head
	}
}
