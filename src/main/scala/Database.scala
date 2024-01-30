import cats.effect.unsafe.implicits.global
import cats.effect.IO
import doobie.WeakAsync.doobieWeakAsyncForAsync
import doobie._
import doobie.implicits._

class Database(transactor: Transactor.Aux[IO, Unit]) {

  def initializeDB(): Unit = {
    sql"""
           |CREATE TABLE IF NOT EXISTS users (
           |  id SERIAL PRIMARY KEY,
           |  chat_id BIGINT,
           |  state INT,
           |  last_number INT,
           |  tasks_amount INT
           |)
       """.stripMargin.update.run.transact(transactor).unsafeRunSync() : Unit

    sql"""
           |CREATE TABLE IF NOT EXISTS tasks (
           |  id SERIAL PRIMARY KEY,
           |  task VARCHAR(2048),
           |  user_id BIGINT, 
           |  task_number INT
           |)
       """.stripMargin.update.run.transact(transactor).unsafeRunSync() : Unit
  }

  def getUserStage(chatId: Long): Int = {
    val result =
      sql"""
           | SELECT state
           | FROM users
           | WHERE chat_id = $chatId
         """.stripMargin.query[Int].option.transact(transactor).unsafeRunSync()
    result match {
      case Some(state) => state
      case _           => 0
    }
  }

  def setUserState(chatId: Long, state: Int): IO[Unit] = for {
    result <-
      sql"""
        | SELECT COUNT(*) as contains
        | FROM users
        | WHERE chat_id = $chatId
         """.stripMargin.query[Long].option.transact(transactor)
    _ <- result match {
      case Some(1) => sql"""
                            | UPDATE users
                            | SET state=$state
                            | WHERE chat_id=$chatId
                            | """.stripMargin.update.run.transact(transactor)
      case _ => sql"""
                     | INSERT INTO users(chat_id, state, last_number, tasks_amount)
                     | VALUES ($chatId, $state, 0, 0)
                     | """.stripMargin.update.run.transact(transactor)
    }
  } yield ()

  def insertNewTask(task: String, user_id: Long, task_number: Int): IO[Unit] = for {
    _ <- sql"""
         | INSERT INTO tasks(task, user_id, task_number)
         | VALUES ($task, $user_id, $task_number)
         | """.stripMargin.update.run.transact(transactor)
  } yield()

  def getAmountTasks(chatid: Long): Int = {
    val result =
      sql"""
           | SELECT COUNT(*)
           | FROM users
           | WHERE chat_id = $chatid
         """.stripMargin.query[Int].unique.transact(transactor).map(_ >= 1).unsafeRunSync()
    result match {
      case true => sql"""
                               | SELECT tasks_amount
                               | FROM users
                               | WHERE chat_id = $chatid
         """.stripMargin.query[Int].unique.transact(transactor).unsafeRunSync()
      case _           => 0
    }
  }

  def getTask(id: Long, number: Int): IO[Option[String]] = {
    sql"""
         | SELECT task
         | FROM tasks
         | WHERE user_id = $id AND task_number = $number
       """.stripMargin.query[String].option.transact(transactor)
  }

  def canEditTask(id: Long, number: Int) : IO[Boolean] = {
    val result =
      sql"""
           | SELECT COUNT(*)
           | FROM tasks
           | WHERE user_id = $id AND task_number = $number
         """.stripMargin.query[Int].unique
    result.transact(transactor).map(_ >= 1)
  }
  def increaseTaskAmount(chatId: Long, newAmount: Int): IO[Unit] = for {
    _ <- sql"""
         | UPDATE users
         | SET tasks_amount = $newAmount
         | WHERE chat_id = $chatId
         | """.stripMargin.update.run.transact(transactor)
  } yield()

  def editTask(newTask: String, id: Long, number: Int): IO[Unit] = for {
    _ <- sql"""
         | UPDATE tasks
         | SET task = $newTask
         | WHERE task_number = $number
         | """.stripMargin.update.run.transact(transactor)
  } yield()
  def listAll(id: Long): IO[List[String]] = {
      sql"""
           | SELECT task
           | FROM tasks
           | WHERE user_id = $id
         """.stripMargin.query[String].to[List].transact(transactor)
  }
  def setLastAccessedNumber(id: Long, number: Int) : IO[Unit] = for {
    _ <- sql"""
              | UPDATE users
              | SET last_number = $number
              | WHERE chat_id = $id AND last_number = $number
              | """.stripMargin.update.run.transact(transactor)
  } yield()

  def getLastAccessedNumber(id: Long) : Int = {
    sql"""
         | SELECT last_number
         | FROM users 
         | WHERE chat_id = $id 
         | """.stripMargin.query[Int].unique.transact(transactor).unsafeRunSync()
  } 
  def owns(token: String): IO[Boolean] = {
    val result =
      sql"""
           | SELECT COUNT(*)
           | FROM texts
           | WHERE owner_token = $token
         """.stripMargin.query[Int].unique
    result.transact(transactor).map(_ >= 1)
  }

  def delete(id: Long, number: Int): IO[Unit] = for {
    _ <- sql"""
         | DELETE FROM tasks
         | WHERE task_number = $number
         | """.stripMargin.update.run.transact(transactor)
  } yield()

}

