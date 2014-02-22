package dspa
import scala.collection.mutable._

object Main2 {
    def main(args: Array[String]) {
        
			println("More complex graph:")
			val graph = DSPA.genGraph(Graph3.edges)
			val objectNode = Util.uniform(graph, 2)
			for ((node, objID) <- objectNode) 
				println(node);
    }
}
