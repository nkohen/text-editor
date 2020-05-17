import javafx.scene.control.skin.TextAreaSkin
import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, Label, TextArea}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.BorderPane

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

  private val statusText: StringProperty = StringProperty("")

  private val elaborationState = ElaborationState(TextModel.example)

  private val statusLabel = new Label {
    maxWidth = Double.MaxValue
    padding = Insets(0, 10, 10, 10)
    text <== statusText
  }

  private val textArea: TextArea = new TextArea {
    editable = false
    wrapText = true
    promptText = "Type text here and ctrl+click on a word to duplicate it!"
    text <== elaborationState.text

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

    onMouseExited = { _ =>
      statusText.value = ""
    }

    onMouseMoved = { event =>
      val index = getCharIndex(event)

      if (index == text().length) {
        statusText.value = ""
      } else {
        val (wordStart, wordLength) = getWord(text(), index)

        if (wordLength <= 0) {
          statusText.value = ""
        } else {
          statusText.value = text().slice(wordStart, wordStart + wordLength)
        }
      }
    }

    onMouseClicked = { event =>
      if (event.controlDown) {
        val index = getCharIndex(event)
        elaborationState.expand(index)
      } else if (event.clickCount == 2) {
        val index = getCharIndex(event)
        val (nodeStart, nodeLength) = elaborationState.select(index)

        if (nodeLength > 0) {
          this.positionCaret(nodeStart)
          this.extendSelection(nodeStart + nodeLength)
        }
      }
    }
  }

  private val borderPane = new BorderPane {
    center = textArea
    bottom = statusLabel
  }

  stage = new JFXApp.PrimaryStage {
    title = "Test Text Area"
    scene = new Scene(borderPane)
  }
}
