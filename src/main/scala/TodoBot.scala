import Keyboards._
import UserState._
import cats.Parallel
import cats.effect.Async
import cats.syntax.all._
import telegramium.bots.{ChatIntId, Message}
import telegramium.bots.high.Api
import telegramium.bots.high.LongPollBot
import telegramium.bots.high.implicits._
import cats.effect.unsafe.implicits.global

class TodoBot[F[_]](database: Database)(implicit
  bot: Api[F],
  asyncF: Async[F],
  parallel: Parallel[F]
) extends LongPollBot[F](bot) {

  private def getUUID: String = java.util.UUID.randomUUID.toString

  private def matchInitialState(message: String, senderId: ChatIntId): F[Unit] = {
    message match {
      case "/create" =>
        database.setUserState(senderId.id, insertingTextState).unsafeRunSync() : Unit
        sendMessage(
            chatId = senderId,
            text = "Введите задачу",
            replyMarkup = Some(backKeyboard)
        ).exec.void

      case "/view" =>
        database.setUserState(senderId.id, gettingViewState).unsafeRunSync() : Unit
        sendMessage(
          chatId = senderId,
          text = "Введите номер задачи, которую вы хотите прочитать",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case "/edit" => 
        database.setUserState(senderId.id, gettingEditState).unsafeRunSync(): Unit
        sendMessage(
          chatId = senderId,
          text = "Введите номер задачи для редактирования",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case "/delete" =>
        database.setUserState(senderId.id, gettingRemoveState).unsafeRunSync() : Unit
        sendMessage(
          chatId = senderId,
          text = "Введите номер задачи, необходимую для удаления",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case "/list_all" =>
        database.setUserState(senderId.id, gettingListState).unsafeRunSync() : Unit
        sendMessage(
          chatId = senderId,
          text = "Напишите 'все задачи', чтобы посмотреть все задачи",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case _ => startingMessage(senderId)
    }
  }

  private def handleInsertingState(task: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val tasksAmount = database.getAmountTasks(senderId.id)
    val newTaskAmount = tasksAmount + 1
    database.insertNewTask(task, senderId.id, newTaskAmount).unsafeRunSync() : Unit
    database.increaseTaskAmount(senderId.id, newTaskAmount).unsafeRunSync() : Unit
    sendMessage(
      chatId = senderId,
      text = s"Задача успешно сохранена! \n" +
        s"Она будет всегда под номером $newTaskAmount \n",

      replyMarkup = Some(startKeyboard)
    ).exec.void
  }

  private def handleViewTextState(message: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val result = database.getTask(senderId.id, message.toInt).unsafeRunSync() 
    result match {
      case Some(task) =>
        sendMessage(
          chatId = senderId,
          text = task.toString(),
          replyMarkup = Some(startKeyboard)
        ).exec.void
      case None =>
        sendMessage(
          chatId = senderId,
          text = "Нет текста с таким токеном :(",
          replyMarkup = Some(startKeyboard)
        ).exec.void
    }
  }

  private def handleGettingEditState(message: String, senderId: ChatIntId): F[Unit] = {
    val canEdit = database.canEditTask(senderId.id, message.toInt).unsafeRunSync()
    if (canEdit) {
      database.setUserState(senderId.id, editingTextState).unsafeRunSync() : Unit
      database.setLastAccessedNumber(senderId.id, message.toInt).unsafeRunSync(): Unit
      sendMessage(
        chatId = senderId,
        text = "Теперь введите новую версию задачи",
        replyMarkup = Some(backKeyboard)
      ).exec.void
    } else {
      database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
      sendMessage(
        chatId = senderId,
        text = "Нет задачи с таким номером",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    }
  }

  private def handleEditingTextState(task: String, senderId: ChatIntId): F[Unit] = {
    val numoftask = database.getLastAccessedNumber(senderId.id)
    database.editTask(task, senderId.id, numoftask).unsafeRunSync() : Unit
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    sendMessage(
      chatId = senderId,
      text = s"Текст был обновлен, новое значение:" +
        s"\n\n$task",
      replyMarkup = Some(startKeyboard)
    ).exec.void
  }

  private def handleGettingRemove(message: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val exists = database.canEditTask(senderId.id, message.toInt).unsafeRunSync()
    if (exists) {
      database.delete(senderId.id, message.toInt).unsafeRunSync() : Unit
      sendMessage(
        chatId = senderId,
        text = s"Задча успешно удалена",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    } else {
      sendMessage(
        chatId = senderId,
        text = s"Нет задачи с таким номером :(",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    }
  }

  private def handleListing(message: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val list = database.listAll(senderId.id).unsafeRunSync()
      sendMessage(
        chatId = senderId,
        text = list.toString(),
        replyMarkup = Some(startKeyboard)
      ).exec.void

  }

  private def matchState(state: Int, message: String, senderId: ChatIntId): F[Unit] = {
    state match {
      case `initialState`            => matchInitialState(message, senderId)
      case `insertingTextState`      => handleInsertingState(message, senderId)
      case `gettingViewState`        => handleViewTextState(message, senderId)
      case `gettingEditState`        => handleGettingEditState(message, senderId)
      case `editingTextState`        => handleEditingTextState(message, senderId)
      case `gettingRemoveState`      => handleGettingRemove(message, senderId)
      case `gettingListState`        => handleListing(message, senderId)
    }
  }

  private def startingMessage(senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    sendMessage(
      chatId = senderId,
      text = "Выберите действие, которое вам нужно: " +
        "\n /create - создать задачу \n /view - посмотреть список задач " +
        "\n /edit - отредактировать задачу \n /delete - удалить задачу \n /list_all - вывести все задачи",
      replyMarkup = Some(startKeyboard)
    ).exec.void
  }

  override def onMessage(msg: Message): F[Unit] = {
    val senderId = ChatIntId(msg.chat.id)
    val stage = database.getUserStage(senderId.id)
    (msg.text, stage) match {
      case (Some("/back"), _)     => startingMessage(senderId)
      case (Some("/start"), _)    => startingMessage(senderId)
      case (Some(message), state) => matchState(state, message, senderId)
      case _                      => asyncF.unit
    }
  }
}
