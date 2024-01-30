import cats.effect.{ExitCode, IO, IOApp}
import doobie.Transactor
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.client.Client
import org.http4s.client.middleware.Logger
import telegramium.bots.high.{Api, BotApi}

object Application extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val databaseUrl = "jdbc:postgresql:todobot"
    val databaseUser = "postgres"
    val databasePassword = "1111111"

    BlazeClientBuilder[IO].resource
      .use { 
        httpClient =>
        
            val http = Logger(logBody = false, logHeaders = false)(httpClient)
            val token = "6868386388:AAGJ0KpOmn1ATXq7zkrM1KOGi-h1WNA3QPI"
            implicit val api: Api[IO] = BotApi(http, baseUrl = s"https://api.telegram.org/bot$token")
            val transactor =
              Transactor.fromDriverManager[IO]("org.postgresql.Driver", databaseUrl, databaseUser, databasePassword)
            val db = new Database(transactor)
            db.initializeDB()
            val todoBot = new TodoBot(db)
            todoBot.start()
      }.as(ExitCode.Success)
  }

}
