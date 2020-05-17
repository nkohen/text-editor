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

  private val textArea: TextArea = new TextArea {
    editable = true
    wrapText = true
    promptText = "Type text here and ctrl+click on a word to duplicate it!"

    def getCharIndex(event: MouseEvent): Int = {
      val clickX = event.sceneX
      val clickY = event.sceneY
      skin().asInstanceOf[TextAreaSkin].getIndex(clickX, clickY).getCharIndex
    }

    def getWord(text: String, index: Int): (Int, Int) = {
      val textClickedSuffix = text.drop(index).takeWhile(_.isLetterOrDigit)
      val textClickedPrefix = text.take(index).reverse.takeWhile(_.isLetterOrDigit).reverse

      val wordStart = index - textClickedPrefix.length
      val wordLength = textClickedPrefix.length + textClickedSuffix.length

      (wordStart, wordLength)
    }

    onMouseClicked = { event =>
      if (event.controlDown) {
        val index = getCharIndex(event)
        val (wordStart, wordLength) = getWord(text(), index)

        if (wordLength > 0) {
          val (textBefore, textWithAndAfter) = text().splitAt(wordStart)
          val (textClicked, textAfter) = textWithAndAfter.splitAt(wordLength)

          text = textBefore ++ s"($textClicked, $textClicked)" ++ textAfter
          this.positionCaret(text().length - textAfter.length)
        }
      } else if (event.clickCount == 2) {
        val index = getCharIndex(event)
        val (wordStart, wordLength) = getWord(text(), index)

        if (wordLength > 0) {
          this.positionCaret(wordStart)
          this.extendSelection(wordStart + wordLength + 4)
        }
      }
    }
  }

  stage = new JFXApp.PrimaryStage {
    title = "Test Text Area"
    scene = new Scene(textArea)
  }
}
