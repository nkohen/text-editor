import javafx.scene.control.skin.TextAreaSkin
import scalafx.application.JFXApp
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.{Alert, Label, Menu, MenuBar, MenuItem, TextArea}
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.BorderPane
import scalafx.stage.{FileChooser, Window}

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

  private val fileMenu: Menu = new Menu {
    text = "File"

    private val newItem = new MenuItem {
      text = "New"
      onAction = { _ =>
        println("New button pressed")
      }
    }

    private val openItem = new MenuItem {
      text = "Open"
      onAction = { _ =>
        println("Open button pressed")

        val fileChooser = new FileChooser {
          title = "Open File"
        }

        val fileOrNull = fileChooser.showOpenDialog(null.asInstanceOf[Window])

        if (fileOrNull != null) {
          elaborationState.reset(TextRoot.fromFile(fileOrNull))
          statusText.value = s"Opened ${fileOrNull.getCanonicalPath}"
        }
      }
    }

    private val saveItem = new MenuItem {
      text = "Save"
      onAction = { _ =>
        val fileChooser = new FileChooser {
          title = "Save File"
        }

        val fileOrNull = fileChooser.showSaveDialog(null.asInstanceOf[Window])

        if (fileOrNull != null) {
          elaborationState.getRoot.save(fileOrNull)
          statusText.value = "File saved!"
        }
      }
    }

    private val exitItem = new MenuItem {
      text = "Exit"
      onAction = { _ =>
        sys.exit()
      }
    }

    items = Seq(newItem, openItem, saveItem, exitItem)
  }

  private val menuBar = new MenuBar {
    menus = Seq(fileMenu)
  }

  private val textArea: TextArea = new TextArea {
    editable = false
    wrapText = true
    promptText = "Type text here and ctrl+click on a word to duplicate it!"
    text <== elaborationState.text

    def getCharIndex(event: MouseEvent): Int = {
      getCharIndex(event.x, event.y)
    }

    def getCharIndex(clickX: Double, clickY: Double): Int = {
      skin().asInstanceOf[TextAreaSkin].getIndex(clickX, clickY).getCharIndex
    }

    onMouseMoved = { event =>
      if (!editable()) {
        val index = getCharIndex(event)

        if (index != text().length) {
          val parent = elaborationState.selectParent(index)

          if (!parent.isEmpty) {
            statusText.value = parent
          }
        }
      }
    }

    onMouseClicked = { event =>
      val currentlyEditable = editable()
      val index = getCharIndex(event)
      if (event.controlDown) {
        if (event.shiftDown && !currentlyEditable) {
          elaborationState.compress(index)
        } else if (event.altDown && !currentlyEditable) {
          elaborationState.expand(index)
        } else {
          val textFunc = { () => text() }
          val selectFunc = { (start: Int, end: Int) =>
            this.positionCaret(start)
            this.extendSelection(end)
          }
          if(elaborationState.toggleEditingForNode(index, textFunc, selectFunc)) {
            if (!currentlyEditable) {
              text.unbind()
              statusText.value = "Edit Mode"
            } else {
              statusText.value = ""
              text <== elaborationState.text
            }

            editable = !editable()
          }
        }
      } else if (!currentlyEditable) {
        val (nodeStart, nodeLength) =
          if (event.clickCount == 2) {
            elaborationState.select(index)
          } else if (event.clickCount == 3) {
            elaborationState.selectParentalRelatives(index)
          } else {
            (caretPosition(), 0)
          }

        if (nodeLength > 0) {
          this.positionCaret(nodeStart)
          this.extendSelection(nodeStart + nodeLength)
        }
      }
    }
  }

  private val borderPane = new BorderPane {
    top = menuBar
    center = textArea
    bottom = statusLabel
  }

  stage = new JFXApp.PrimaryStage {
    title = "Test Text Area"
    scene = new Scene(borderPane)
  }
}
