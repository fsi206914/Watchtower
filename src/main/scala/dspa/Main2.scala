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
			
			import java.io._

			/*
			 * The ObjectOutput templete for serialization using 
			//val out = new ObjectOutputStream(new FileOutputStream("./file.bin"))
			//out.writeObject(graph(0))
			//out.close()
			
			/*
			 * Serialize Graph to a binary File using scala pickling
			 */
			{
				import scala.pickling._
				import binary._

				val out = new ObjectOutputStream(new FileOutputStream("./file.bin"))
				print("index construction time === ");
				time{
				val pcklByte = graph.pickle.value
				out.write(pcklByte)
				out.close()}
			}

			val lambda :Float = 0.001f
			Deploy.anchorPointDeploy(graph, lambda)

			val objectMap: Map[Int, Int] = Util.uniform(Config.numObj) 
			SaveInfo.selectWatchtower(objectMap, graph, Config.Interval)

			println(Deploy.numBW);
			println(SaveInfo.wtCount);
			Search.uniformSearch(34, graph, Config.Interval*lambda*2, Config.K)

			//time{ Update.AddEdge(graph, 2.0f, 1, 3, 0.1f, objectMap)}
			//time{ NaiveUpdate.rmEdge(graph, 2.0f, 2, 3, objectMap)}
			//time{ Update.addObject(graph, 12, 400)}
			val first = objectMap.toList.head

			print("a sample query time === ");
			time{ Update.rmObject(graph, first._2, first._1)}
    }

		def time[R](block: => R): R = {
			val t0 = System.nanoTime()
				val result = block    // call-by-name
				val t1 = System.nanoTime()
				println("Elapsed time: " + (t1 - t0)/1000000 + "ms")
				result
		}
}
