/**
* Created by prototyp on 24.08.14.
*/
import java.util.Random;

import javafx.animation.PathTransition;
import javafx.application.Application;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.geometry.VPos;
import javafx.scene.Group;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.effect.DropShadow;
import javafx.scene.effect.Effect;
import javafx.scene.effect.Light;
import javafx.scene.effect.Lighting;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Line;
import javafx.scene.shape.LineTo;
import javafx.scene.shape.MoveTo;
import javafx.scene.shape.Path;
import javafx.scene.shape.Rectangle;
import javafx.scene.shape.StrokeLineCap;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;
import javafx.stage.Stage;
import javafx.util.Duration;




public class TreeFx extends Application {
    Tree myTree;

    enum ParentPos { Left, Right, None };

    public static class Tree extends Parent {
        double YDist = 60;
        double NodeSize = 30;
        double LeafSize = 50;
        Effect nodeEffect;
        Effect leafEffect;
        Lighting myLight;
        EBTree.NodeIf myTree;
        public Tree() {
            myLight = new Lighting();
            Light.Distant l = new Light.Distant();
            l.setAzimuth(225);
            myLight.setLight(l);

            DropShadow d = new DropShadow(4, 4, 4, Color.web("#404040"));
            d.setInput(myLight);
            nodeEffect = d;
            leafEffect = new DropShadow(4, 4, 4, Color.BLACK);
        }

        void randomTree() {
            Random rand = new Random(System.nanoTime());
            EBTree<String> tree = new EBTree<String>();

            for (int i = 1; i < 20; i ++) {
                int l = rand.nextInt(999);
                tree.put(l, ""+l);
            }

            setTree(tree.myRoot.getLeft());
        }

        public void setTree(EBTree.NodeIf tree) {
            myTree = tree;
            buildView();
        }

        public void buildView() {
            getChildren().clear();
            double w = Math.max(boundsInParentProperty().get().getWidth(), 1400);
            double h = Math.max(boundsInParentProperty().get().getHeight(), 700);
            double y = 50;
            Group treeRoot = new Group();
            Rectangle bg = new Rectangle(0,0,w,h);
            bg.setFill(Color.web("#406040"));
            getChildren().add(bg);
            buildNodeView(treeRoot, myTree, 30, w-60, y, ParentPos.None);
            treeRoot.setEffect(nodeEffect);

            getChildren().add(treeRoot);
        }

        void buildNodeView(Group in, EBTree.NodeIf node, double x0, double w, final double y, ParentPos p) {
            final double x = x0 + w/2;
            double hsize = NodeSize;
            double wsize = NodeSize;
            if (node.isLeaf()) {
                wsize = Math.min(w-2, wsize);


                Rectangle rect = new Rectangle(x-wsize/2, y-hsize/2, wsize, hsize);
                rect.setStroke(Color.BLACK);
                rect.setStrokeWidth(0.4);
                rect.setFill(Color.web("#f0f0c0"));

                Text text = new Text(x-wsize/2, y, node.getLabel());
                text.setTextOrigin(VPos.CENTER);
                text.setWrappingWidth(wsize);
                if (wsize < NodeSize) {
                    Font font = Font.font("MODERN",text.getFont().getSize()/2);
                    text.setFont(font);
                }

                text.setTextAlignment(TextAlignment.CENTER);

                in.getChildren().addAll(rect, text);
            } else {
                wsize = Math.min(w-2, wsize);
                hsize = wsize;
                final Circle circle = new Circle(x, y, wsize/2);
                circle.setStroke(Color.BLACK);
                circle.setStrokeWidth(0.0);
                circle.setFill(Color.web("#f0c0c0"));



                Text text = new Text(x-wsize/2, y, node.getLabel());
                text.setTextOrigin(VPos.CENTER);
                text.setWrappingWidth(wsize);
                text.setTextAlignment(TextAlignment.CENTER);
                if (wsize < NodeSize) {
                    Font font = Font.font("MODERN",text.getFont().getSize()*0.75);
                    text.setFont(font);
                }

                Group child = new Group();
                child.getChildren().addAll(circle, text);
                in.getChildren().add(child);
                buildNodeView(child, node.getLeft(), x0, w/2, y+YDist, ParentPos.Right);
                buildNodeView(child, node.getRight(), x0+w/2, w/2, y+YDist, ParentPos.Left);


                text.setOnMouseClicked(new EventHandler(){
                    @Override
                    public void handle(Event event) {
                        System.out.println("Move");
                        Node on = circle;
                        Path path = new Path();
                        path.getElements().add(new MoveTo(x, y));
                        path.getElements().add(new LineTo(x+5, y));

                        PathTransition pt = new PathTransition();
                        pt.setDuration(Duration.millis(40));
                        pt.setNode(on);
                        pt.setPath(path);
                        pt.setCycleCount(20);
                        pt.setAutoReverse(true);


                        pt.play();
                    }
                });
            }
            Line line = null;
            switch (p) {
                case Left: {
                    line = new Line(x0,y-YDist+NodeSize/2+1,x,y-hsize/2-1);
                    break;
                }
                case Right: {
                    line = new Line(x0+w,y-YDist+NodeSize/2+1,x,y-hsize/2-1);
                    break;
                }
                case None: break;
            }
            if (line != null) {
                line.setStrokeWidth(3.5);
                line.setStrokeLineCap(StrokeLineCap.ROUND);
                line.setStroke(Color.web("#d0d0d0"));
                in.getChildren().add(line);
            }
        }
    }


    @Override
    public void start(Stage primaryStage) throws Exception {
        Group root = new Group();
        Scene scene = new Scene(root);
        primaryStage.setScene(scene);
        myTree = new Tree();
        myTree.randomTree();
        root.getChildren().add(myTree);

        primaryStage.show();

        primaryStage.widthProperty().addListener(new ChangeListener() {
            @Override
            public void changed(ObservableValue observable, Object oldValue,
                                Object newValue) {
                myTree.randomTree();
            }
        });

    }

    public static void main(String[] args) { launch(args); }
}