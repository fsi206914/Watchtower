package dspa
import scala.collection.mutable._

object WTMain {
    def main(args: Array[String]) {

			val graph = Util.readGraph("NA.cnode", "NA.cedge");
			import java.io._

			/*
			 * The ObjectOutput templete for serialization using 
			//val out = new ObjectOutputStream(new FileOutputStream("./file.bin"))
			//out.writeObject(graph(0))
			//out.close()
			*/
			val lambda :Float = 2.00f
			Deploy.anchorPointDeploy(graph, lambda)

			println("The weight sum of G = " + Deploy.graphWeightSum)

			val objectMap: Map[Int, Int] = Util.uniform(Config.numObj) 
			SaveInfo.selectWatchtower(objectMap, graph, Config.Interval)

			println("The number of Anchor Points= " + Deploy.numBW);

			/*
			 * Serialize Graph to a binary File using scala pickling
			 */
			if(Config.serialEnable == true)
			{
				import scala.pickling._
				import binary._

				val out = new ObjectOutputStream(new FileOutputStream("./file.bin"))
				print("index construction time === :    ");
				time{
				SaveInfo.selectWatchtower(objectMap, graph, Config.Interval)
				print("The number of Watchtowers selected = " + SaveInfo.wtCount + "    ");

				val pcklByte = graph.pickle.value
				out.write(pcklByte)
				out.close()}
			}

			/*
			 * Query evaluation.
			 */
			Search.uniformSearch(34, graph, Config.Interval*lambda*2, Config.K)

			//time{ Update.AddEdge(graph, 2.0f, 1, 3, 0.1f, objectMap)}
			//time{ NaiveUpdate.rmEdge(graph, 2.0f, 2, 3, objectMap)}
			//time{ Update.addObject(graph, 12, 400)}
			val first = objectMap.toList.head

			print("remove object === ");
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
