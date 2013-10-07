import com.mongodb.casbah.MongoClient
import web_ramp._
import org.scalatra._
import javax.servlet.ServletContext

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    // As you can see, there's not much to do in order to get MongoDb working with Scalatra.
    // We're connecting with default settings - localhost on port 27017 -
    // by calling MongoClient() with no arguments.
    val mongoClient =  MongoClient()
    val mongoColl = mongoClient("casbah_test")("test_data")

    context.mount(new NetworkSimulatorController(mongoColl), "/*")
  }
}