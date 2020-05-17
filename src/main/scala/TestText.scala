import javafx.scene.control.skin.TextAreaSkin
import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, TextArea}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.MouseEvent

object TestText extends JFXApp {
  // Catch unhandled exceptions on FX Application thread
  Thread
    .currentThread()
    .setUncaughtExceptionHandler(
      (_: Thread, ex: Throwable) => {
        ex.printStackTrace()
        val _ = new Alert(AlertType.Error) {
          initOwner(owner)
          title = "Unhandled exception"
          headerText = "Exception: " + ex.getClass + ""
          contentText = Option(ex.getMessage).getOrElse("")
        }.showAndWait()
      }
    )

  private val textArea = new TextArea {
    editable = true
    wrapText = true
    promptText = "Type text here and ctrl+click on a word to duplicate it!"

    onMouseClicked = { event =>
      event.eventType match {
        case MouseEvent.MouseClicked  if event.controlDown =>
          val typedEvent: MouseEvent = event
          val clickX = typedEvent.sceneX
          val clickY = typedEvent.sceneY
          val index = skin().asInstanceOf[TextAreaSkin].getIndex(clickX, clickY).getCharIndex

          val textClickedSuffix = text().drop(index).takeWhile(_.isLetterOrDigit)
          val textClickedPrefix = text().take(index).reverse.takeWhile(_.isLetterOrDigit).reverse

          val wordStart = index - textClickedPrefix.length
          val wordLength = textClickedPrefix.length + textClickedSuffix.length

          if (wordLength > 0) {
            val (textBefore, textWithAndAfter) = text().splitAt(wordStart)
            val (textClicked, textAfter) = textWithAndAfter.splitAt(wordLength)

            text = textBefore ++ s"($textClicked, $textClicked)" ++ textAfter
            this.positionCaret(text().length - textAfter.length)
          }
        case _ => ()
      }
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "Test Text Area"
    scene = new Scene(textArea)
  }
}
