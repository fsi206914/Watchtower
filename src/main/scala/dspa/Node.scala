package dspa

import scala.collection.mutable.{Set, HashSet, ArrayBuffer}

/**
 * Node is in charge of Node for graph search. 
 */
class Node(val name: Any) {
    private val _edges:Set[Edge] = new HashSet[Edge]
    var distance = Double.MaxValue
    var visited = false
    var previous:Node = null

    def addEdge(edge:Edge){
        _edges += edge
    }
    
    def edges = _edges.toList

    def compareTo(that:Node): Int ={
    	val ret = this.distance - that.distance;
    	if(ret < 0) return 1;
    	else if(ret > 0) return -1;
    	else return 0;
    }

		override def toString(): String = name.toString
}


/*
 * m_x and m_y are x and y coordinates of Node.
 */
class GraphNode(val name: Int, val m_x: Float, val m_y: Float) {

    private val _edges:Set[GraphEdge] = new HashSet[GraphEdge]
    var distance = Double.MaxValue
		var bw:baseWatchtower = null 
		var wt:Watchtower = null 
    var visited = false
    var previous:GraphNode= null

		var selectRemain: Int = 0

    def addEdge(edge:GraphEdge){
        _edges += edge
    }
    def edges = _edges.toList

    def compareTo(that:GraphNode): Int ={
    	val ret = this.distance - that.distance;
    	if(ret < 0) return 1;
    	else if(ret > 0) return -1;
    	else return 0;
    }

		override def toString(): String = name.toString
}


class GraphEdge(val name: Int, val startNode: Int, val endNode: Int,
								  val w:Float) {
		override def toString(): String = name.toString

		var bwList:ArrayBuffer[baseWatchtower] = new ArrayBuffer[baseWatchtower]

    def addBW(bw:baseWatchtower){
        bwList += bw 
    }
}
