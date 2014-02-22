package dspa

import scala.collection.mutable.{Set, HashSet}

class Node(val name: Any) {
    private val _edges:Set[Edge] = new HashSet[Edge]
    var distance = Double.MaxValue
    var visited = false
    var previous:Node = null

    def addEdge(edge:Edge) {
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
