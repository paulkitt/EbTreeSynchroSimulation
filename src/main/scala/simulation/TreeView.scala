package simulation

/**
 * Created by prototyp on 15.08.14.
 */
/**
 * Created by prototyp on 15.08.14.
 */

import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import scala.util.Random
import src.main.scala.model.{NodeIf, EbTree}

//import src.main.scala.model.{EbTree, NodeIf}
import javax.swing.JFrame
import javax.swing.JPanel

object TreeView {
  def main(args: Array[String]) {
    val tree: EbTree[String] = new EbTree[String]()

//      var i: Int = 0
//      while (i < 50) {
//        {
//          val id: Int = Random.nextInt(100)
//          tree.put(id, "I" + id)
//        }
//        ({
//          i += 1; i - 1
//        })
//      }
    //List(1000L,1001L,1024L,1023L,1025L,1026L,1032L,1033L,1045L,1312L,1800L).foreach(x =>tree.put(x,""))
    //List(1000L,1001L,1022L,1023L,1024L).foreach(x =>tree.put(x,""))
    //List(1000L,1001L,1023L,1024L).foreach(x =>tree.put(x,""))
    //List(1000L,1001L,1022L,1024L).foreach(x =>tree.put(x,""))
    //List(1000L,1001L).foreach(x =>tree.put(x,""))
    List(1022L,1023L).foreach(x =>tree.put(x,""))
    try {
      val view: TreeView = new TreeView("")
      view.setTree(tree.myRoot.get.myZero)
    }
    catch {
      case t: Throwable => {
        t.printStackTrace
      }
    }
  }

  class TreePainter extends JPanel {
    override def paint(g: Graphics) {
      val g2: Graphics2D = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
      val w: Int = getWidth
      val h: Int = getHeight
      g2.setColor(Color.white)
      g2.fillRect(0, 0, w, h)
      g2.setColor(Color.black)
      g2.drawString("Baum B",40,45)
//      g2.setColor(new Color(0x00, 0x99, 0x33))
//      g2.drawString("Knoten Change ID",40,60)
//      g2.setColor(Color.black)
//      g2.drawString("Knoten Bitebene",40,75)
//      g2.setColor(Color.blue)
//      g2.drawString("Change ID",40,90)
      if(myRoot!=null){
        paintNode(g2, null, myRoot, 0, 0, w, false)
      }

    }

    def paintNode(g2: Graphics2D, parent: NodeIf, node: NodeIf, x0: Int, y0: Int, w: Int, left: Boolean) {

      if (node.isLeaf) {
        g2.setColor(Color.blue)
      }
      else {
        g2.setColor(Color.black)
      }
      val x: Int = x0 + w / 2
      val y: Int = y0 + levelDistance
      val d: Int = 2 * radius / 3
      if (parent != null) {
        if (left) {
          g2.drawLine(x, y - radius, x0 + w, y0 + radius)
        }
        else {
          g2.drawLine(x, y - radius, x0, y0 + radius)
        }
      }
      if (node.isLeaf) {
        if(node.getLabel()._1=="10"||node.getLabel()._1=="1000"){g2.setColor(Color.red)}
        g2.drawRect(x - radius, y - radius, 4 * radius, 2 * radius -2)
        g2.drawString(node.getLabel._1, x - radius +4 , y + radius -4 )
      }
      else {
        g2.drawOval(x - radius, y - radius, 2 * radius, 2 * radius)
        g2.drawString(node.getLabel._1, x - radius + 4, y + radius - 4)
        g2.setColor(new Color(0x00, 0x99, 0x33))
        g2.drawString(node.getLabel._2, x - radius + 24, y + radius - 4)
      }
      val l: NodeIf = node.getLeft
      if (l != null) {
        paintNode(g2, node, l, x0, y, w / 2, true)
      }
      val r: NodeIf = node.getRight
      if (r != null) {
        paintNode(g2, node, r, x0 + w / 2, y, w / 2, false)
      }
    }

    var myRoot: NodeIf = null
    var levelDistance: Int = 50
    var radius: Int = 10
  }

}

class TreeView extends JFrame {

  var painter: TreeView.TreePainter = null

  def this(inc:String) {
    this()
    addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent) {
        System.exit(0)
      }
    })
    painter = new TreeView.TreePainter
    add(painter)
    setTitle("BETree Viewer")
    this.setSize(1000, 600)
    setVisible(true)
  }
  def setTree(root: NodeIf) {
    painter.myRoot = root

    this.repaint()
  }


}