package dspa
import scala.collection.mutable._

object Main2 {
    def main(args: Array[String]) {
        
			//println("More complex graph:")
			//val graph = DSPA.genGraph(Graph3.edges)
			//val objectNode = Util.uniform(graph, 2)
			//for ((node, objID) <- objectNode) 
				//println(node);

			val graph = Util.readGraph("cal.cnode", "cal.cedge");
			val lambda :Float = 0.001f
			Deploy.anchorPointDeploy(graph, lambda)

			val objectMap: Map[Int, Int] = Util.uniform(Config.numObj) 
			SaveInfo.selectWatchtower(objectMap, graph, Config.Interval)



			println(Deploy.numBW);
    }
}
