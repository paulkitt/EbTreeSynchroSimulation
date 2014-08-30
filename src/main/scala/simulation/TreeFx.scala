package simulation


import java.util.Random

import src.main.scala.model.EbTree
import src.main.scala.model.NodeIf

import javafx.animation.PathTransition
import javafx.application.Application
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import javafx.event.Event
import javafx.event.EventHandler
import javafx.geometry.VPos
import javafx.scene.Group
import javafx.scene.Node
import javafx.scene.Parent
import javafx.scene.Scene
import javafx.scene.effect.DropShadow
import javafx.scene.effect.Effect
import javafx.scene.effect.Light
import javafx.scene.effect.Lighting
import javafx.scene.paint.Color
import javafx.scene.shape.Circle
import javafx.scene.shape.Line
import javafx.scene.shape.LineTo
import javafx.scene.shape.MoveTo
import javafx.scene.shape.Path
import javafx.scene.shape.Rectangle
import javafx.scene.shape.StrokeLineCap
import javafx.scene.text.Font
import javafx.scene.text.Text
import javafx.scene.text.TextAlignment
import javafx.stage.Stage
import javafx.util.Duration
import simulation.TreeFx.Tree
import javafx.scene.input.MouseEvent
;

/**
 * Created by prototyp on 24.08.14.
 */
object TreeFx {

  def main(args: Array[String]) {
    Application.launch(classOf[TreeFx], args: _*)
  }

  object ParentPos extends Enumeration {
    type ParentPos = Value
    val Left, Right, None = Value
  }

  import ParentPos._

  class Tree() extends Parent {

    var YDist: Double = 60
    var NodeSize: Double = 30
    var LeafSize: Double = 50
    var nodeEffect: Effect = null
    var leafEffect: Effect = null
    var myLight: Lighting = null
    var myTree: NodeIf = null

    def this(bla: String) {
      this()
      myLight = new Lighting
      val l: Light.Distant = new Light.Distant
      l.setAzimuth(225)
      myLight.setLight(l)
      val d: DropShadow = new DropShadow(4, 4, 4, Color.web("#404040"))
      d.setInput(myLight)
      nodeEffect = d
      leafEffect = new DropShadow(4, 4, 4, Color.BLACK)
    }

    def randomTree {
      val rand: Random = new Random(System.nanoTime)
      val tree: EbTree[String] = new EbTree[String]
      val values = (0 to 20).toList
      values.foreach(x => tree.put(rand.nextInt(999).toLong,"bla"))

      setTree(tree.myRoot.get.getLeft)
    }

    def setTree(tree: NodeIf) {
      myTree = tree
      buildView
    }

    def buildView {
      getChildren.clear
      val w: Double = Math.max(boundsInParentProperty.get.getWidth, 1400)
      val h: Double = Math.max(boundsInParentProperty.get.getHeight, 700)
      val y: Double = 50
      val treeRoot: Group = new Group
      val bg: Rectangle = new Rectangle(0, 0, w, h)
      bg.setFill(Color.web("#FFFFFF")) //#406040
      getChildren.add(bg)
      if(myTree!=null){
        buildNodeView(treeRoot, myTree, 30, w - 60, y, ParentPos.None)
      }
      treeRoot.setEffect(nodeEffect)
      getChildren.add(treeRoot)
    }

    def buildNodeView(in: Group, node: NodeIf, x0: Double, w: Double, y: Double, p: ParentPos) {
      val x: Double = x0 + w / 2
      var hsize: Double = NodeSize
      var wsize: Double = NodeSize
      if (node.isLeaf) {
        wsize = Math.min(w - 2, wsize)
        val rect: Rectangle = new Rectangle(x - wsize / 2, y - hsize / 2, wsize, hsize)
        rect.setStroke(Color.BLACK)
        rect.setStrokeWidth(0.4)
        rect.setFill(Color.web("#f0f0c0"))
        val text: Text = new Text(x - wsize / 2, y, node.getLabel._1)
        text.setTextOrigin(VPos.CENTER)
        text.setWrappingWidth(wsize)
        if (wsize < NodeSize) {
          val font: Font = Font.font("MODERN", text.getFont.getSize / 2)
          text.setFont(font)
        }
        text.setTextAlignment(TextAlignment.CENTER)
        in.getChildren.addAll(rect, text)
      }
      else {
        wsize = Math.min(w - 2, wsize)
        hsize = wsize
        val circle: Circle = new Circle(x, y, wsize / 2)
        circle.setStroke(Color.BLACK)
        circle.setStrokeWidth(0.0)
        circle.setFill(Color.web("#f0c0c0"))
        val text: Text = new Text(x - wsize / 2, y, node.getLabel._1)
        text.setTextOrigin(VPos.CENTER)
        text.setWrappingWidth(wsize)
        text.setTextAlignment(TextAlignment.CENTER)
        if (wsize < NodeSize) {
          val font: Font = Font.font("MODERN", text.getFont.getSize * 0.75)
          text.setFont(font)
        }
        val child: Group = new Group
        child.getChildren.addAll(circle, text)
        in.getChildren.add(child)
        buildNodeView(child, node.getLeft, x0, w / 2, y + YDist, ParentPos.Right)
        buildNodeView(child, node.getRight, x0 + w / 2, w / 2, y + YDist, ParentPos.Left)
        text.setOnMouseClicked(new EventHandler[MouseEvent] {
          def handle(event: MouseEvent) {
            System.out.println("Move")
            val on: Node = circle
            val path: Path = new Path
            path.getElements.add(new MoveTo(x, y))
            path.getElements.add(new LineTo(x + 5, y))
            val pt: PathTransition = new PathTransition
            pt.setDuration(Duration.millis(40))
            pt.setNode(on)
            pt.setPath(path)
            pt.setCycleCount(20)
            pt.setAutoReverse(true)
            pt.play
          }
        })
      }
      var line: Line = null
      p match {
        case Left => {
          line = new Line(x0, y - YDist + NodeSize / 2 + 1, x, y - hsize / 2 - 1)
        }
        case Right => {
          line = new Line(x0 + w, y - YDist + NodeSize / 2 + 1, x, y - hsize / 2 - 1)
        }
        case None =>
      }
      if (line != null) {
        line.setStrokeWidth(3.5)
        line.setStrokeLineCap(StrokeLineCap.ROUND)
        line.setStroke(Color.web("#d0d0d0"))
        in.getChildren.add(line)
      }
    }


  }

}

class TreeFx extends Application {
  var myTree: TreeFx.Tree = null
  def this(bla:String){
    this()
    Application.launch(classOf[TreeFx])
  }

  def start(primaryStage: Stage) {
    val root: Group = new Group
    val scene: Scene = new Scene(root)
    primaryStage.setScene(scene)
    myTree = new Tree("")
    myTree.randomTree
    root.getChildren.add(myTree)
    primaryStage.show
    primaryStage.widthProperty.addListener(new ChangeListener[Number] {
      def changed(observable: ObservableValue[_ <: Number], oldValue: Number, newValue: Number) {
        //myTree.randomTree
      }
    })
  }
  def setTree(tree:NodeIf){
    myTree.setTree(tree)
  }



}