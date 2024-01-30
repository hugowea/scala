import telegramium.bots.{KeyboardButton, ReplyKeyboardMarkup}

object Keyboards {
  val startKeyboard: ReplyKeyboardMarkup = ReplyKeyboardMarkup(
    List(
      List(
        KeyboardButton("/create"),
        KeyboardButton("/view"),
        KeyboardButton("/edit"),
        KeyboardButton("/delete"),
        KeyboardButton("/list_all")
      )
    )
  )

  val backKeyboard: ReplyKeyboardMarkup = ReplyKeyboardMarkup(List(List(KeyboardButton("/back"))))
}

