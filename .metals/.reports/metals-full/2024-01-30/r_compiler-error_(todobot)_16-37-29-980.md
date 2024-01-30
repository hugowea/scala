file:///D:/TodoBot/src/main/scala/TodoBot.scala
### scala.reflect.internal.Types$TypeError: illegal cyclic reference involving object all

occurred in the presentation compiler.

action parameters:
offset: 5286
uri: file:///D:/TodoBot/src/main/scala/TodoBot.scala
text:
```scala
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
            text = "Введите текст, который вы хотите сохранить",
            replyMarkup = Some(backKeyboard)
        ).exec.void

      case "/view" =>
        database.setUserState(senderId.id, gettingViewTokenState).unsafeRunSync() : Unit
        sendMessage(
          chatId = senderId,
          text = "Введите токен текста, который вы хотите получить (любой токен)",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case "/edit" => 
        database.setUserState(senderId.id, gettingEditTokenState) .unsafeRunSync(): Unit
        sendMessage(
          chatId = senderId,
          text = "Введите токен для редактирования текста (edit_token или owner_token)",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case "/delete" =>
        database.setUserState(senderId.id, gettingRemoveTokenState).unsafeRunSync() : Unit
        sendMessage(
          chatId = senderId,
          text = "Введите токен, необходимый для удаления записи (owner_token)",
          replyMarkup = Some(backKeyboard)
        ).exec.void
      case "/change_tokens" =>
        database.setUserState(senderId.id, gettingChangeTokenState).unsafeRunSync() : Unit
        sendMessage(
          chatId = senderId,
          text = "Введите токен, необходимый для обновления токенов записи (owner_token)",
          replyMarkup = Some(backKeyboard)
        ).exec.void
    }
  }

  private def handleInsertingState(task: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val vToken = getUUID
    val eToken = getUUID
    val oToken = getUUID
    val tasksamount = database.getAmountTasks(senderId.id) + 1 
    database.insertNewTask(task, vToken, eToken, oToken, senderId.id, tasksamount).unsafeRunSync() : Unit
    database.increaseTaskAmount(senderId.id, tasksamount).unsafeRunSync()
    sendMessage(
      chatId = senderId,
      text = s"Задача успешно сохранена! \n" +
        s"view_token (Токен по которому доступен только просмотр):\n$vToken \n" +
        s"edit_token (Токен по которому доступно редактирование и просмотр):\n$eToken \n" +
        s"owner_token (Токен по которому доступна вся работа с текстом):\n$oToken \n" +
        s"Она будет всегда под номером $tasksamount \n",

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

  private def handleGettingEditTokenState(message: String, senderId: ChatIntId): F[Unit] = {
    val canEdit = database.canEditTask(senderId.id, message.toInt).unsafeRunSync()
    if (canEdit) {
      database.setUserState(senderId.id, editingTextState).unsafeRunSync() : Unit
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

  private def handleGettingRemoveToken(message: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val exists = database.canEditTask(senderId.id, message.toInt).unsafeRunSync()
    if (exists) {
      database.delete(senderId.id, message.toInt) : Unit
      sendMessage(
        chatId = s@@enderId,
        text = s"Текст успешно удален",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    } else {
      sendMessage(
        chatId = senderId,
        text = s"Нет текста с таким токеном :(",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    }
  }

  private def handleGettingChangeToken(message: String, senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    val owner = database.owns(message).unsafeRunSync()
    if (owner) {
      val newVToken = getUUID
      val newEToken = getUUID
      database.changeTokens(message, newVToken, newEToken) : Unit
      sendMessage(
        chatId = senderId,
        text = "ТОКЕНЫ ОБНОВЛЕНЫ\n" +
          s"view_token (Токен по которому доступен только просмотр):\n$newVToken \n" +
          s"edit_token (Токен по которому доступно редактирование и просмотр):\n$newEToken \n" +
          s"owner_token (Токен по которому доступна вся работа с текстом):\n$message \n",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    } else {
      sendMessage(
        chatId = senderId,
        text = s"Нет текста с таким токеном :(",
        replyMarkup = Some(startKeyboard)
      ).exec.void
    }
  }

  private def matchState(state: Int, message: String, senderId: ChatIntId): F[Unit] = {
    state match {
      case state if state == initialState            => matchInitialState(message, senderId)
      case state if state == insertingTextState      => handleInsertingState(message, senderId)
      case state if state == gettingViewTokenState   => handleViewTextState(message, senderId)
      case state if state == gettingEditTokenState   => handleGettingEditTokenState(message, senderId)
      case state if state == editingTextState        => handleEditingTextState(message, senderId)
      case state if state == gettingRemoveTokenState => handleGettingRemoveToken(message, senderId)
      case state if state == gettingChangeTokenState => handleGettingChangeToken(message, senderId)
    }
  }

  private def startingMessage(senderId: ChatIntId): F[Unit] = {
    database.setUserState(senderId.id, initialState).unsafeRunSync() : Unit
    sendMessage(
      chatId = senderId,
      text = "Выберите действие, которое вам нужно: " +
        "\n /create - создать задачу \n /view - посмотреть список задач " +
        "\n /edit - отредактировать задачу \n /delete - удалить задачу \n /change_tokens - обновить токены задач",
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

```



#### Error stacktrace:

```

```
#### Short summary: 

scala.reflect.internal.Types$TypeError: illegal cyclic reference involving object all