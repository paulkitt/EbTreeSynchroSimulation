import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.Queue;

/** Elastic Binary Tree.
 *
 * Map of <long,T>
 * put(), get(), remove()...
 *
 * @author Jens-Peter.Haack@swheel.de
 *
 * @param <T>
 *
 * speed (2.5GHz 64bit inteli5 core) on 1.000.000 entries
 * rem/add: 134 ns
 * get: 57 ns
 * replace: 60 ns
 */

//TODO make tree death dynamic => remove 64 constant
public class EBTree<T> {
    int  mySize = 0;
    int nodeCount = 0;
    Node<T> myRoot;

    static class Child<T> {
        Node<T> myParent ;
    }

    static class Node<T> extends Child<T> {
        int  myBit;
        public int nr;
        Child<T> myZero, myOne;

        public Node(int bit, int nrInc) {
            myBit = bit; nr = nrInc;
        }

        final boolean bitOne(long uid) {
            long shift = 1L << myBit;
            long xored = uid & shift;
            return (xored != 0) && (myBit < 64);
        }

        final public Child<T> getChild(long uid) {
            if(bitOne(uid)){
                return myOne;
            }else{
                return myZero;
            }
            //return (bitOne(uid) ? myOne : myZero);
        }

        final public void setChild(long uid, Child<T> child) {
            if (bitOne(uid)) {
                myOne = child;
            } else {
                myZero = child;
            }
            child.myParent = this;
        }

        public String toString() {
            return "["+myZero+","+myOne+"]";
        }
    }

    static class Leaf<T> extends Child<T> {
        long myUid;
        public int nr;
        T myPayload;

        public Leaf(long uid, T payload,int nrInc) {
            myUid = uid;
            nr = nrInc;
            myPayload = payload;
        }

        public String toString() {
            return "<"+myUid+":"+myPayload+">";
        }
    }

    public int size() { return mySize; }
    //
    Leaf<T> findLeaf(long uid) {
        Child<T> cursor = myRoot;
        while (cursor instanceof Node) {
            cursor = ((Node<T>)cursor).getChild(uid);
        }
        return (Leaf<T>)cursor;
    }

    public T put(long uid, T payload) {
        Leaf<T> leaf = findLeaf(uid);
        if (leaf == null) {
            myRoot = new Node<T>(64,nodeCount);
            myRoot.setChild(uid,  new Leaf<T>(uid, payload,mySize));
            nodeCount++;
            mySize++;
        } else {
            if (leaf.myUid == uid) {
                T oldPayload = leaf.myPayload;
                leaf.myPayload = payload;
                return oldPayload;
            } else {
                long xored = uid ^ leaf.myUid; // findet hoechstes unterscheidungsbit
                int bit = 63-Long.numberOfLeadingZeros(xored); // ergibt höchste
                Node<T> parent = leaf.myParent;
                while (parent.myBit < bit) parent = parent.myParent;

                Node<T> node = new Node<T>(bit,nodeCount);
                node.setChild(uid, new Leaf<T>(uid, payload,mySize));

                node.setChild(~uid, parent.getChild(uid));
                parent.setChild(uid, node);
                nodeCount++;
                mySize++;
            }
        }
        return null;
    }

    public T get(long uid) {
        Leaf<T> leaf = findLeaf(uid);
        if (leaf != null && leaf.myUid == uid) return leaf.myPayload;
        return null;
    }

    public T remove(long uid) {
        Leaf<T> leaf = findLeaf(uid);
        if (leaf != null && leaf.myUid == uid) {
            Node<T> node = leaf.myParent;
            Node<T> parent = node.myParent;
            if (parent == null) {
                myRoot = null;
            } else {
                parent.setChild(uid, node.getChild(~uid));
            }
            this.mySize--;
            return leaf.myPayload;
        }
        return null;
    }




    ///
    public long firstKey() {
        if (myRoot == null) return -1; // here we have the problem, that if -1 is the only single key, it will not work
        return findLeaf(0L).myUid;
    }

    public long lastKey() {
        if (myRoot == null) return 0; // here we have the problem, that if 0 is the only single key, it will not work
        return findLeaf(-1L).myUid;
    }
    //iteration von links nach rechts
    long snapUp(long uid) {
        Leaf<T> leaf = findLeaf(uid);

        if (leaf.myUid != uid) {
            Child<T> child;
            Node<T> parent = leaf.myParent;
            // 1. go up till highest difference bit level
            int bit = 63-Long.numberOfLeadingZeros(uid ^ leaf.myUid);
            while (parent.myBit < bit) parent = parent.myParent;

            if ((uid & (1L << bit)) != 0) {
                // 2a. if that bit is one in uid: go up
                while (parent.bitOne(uid)) parent = parent.myParent;
                child = parent.myOne;
                if (child == null) return 0;
            } else {
                // 2b. if that bit is zero in uid: go down
                child = parent.getChild(uid);
            }
            // 3. go down, keep left
            while (child instanceof Node) {
                child = ((Node<T>)child).myZero;
            }
            leaf = (Leaf<T>)child;
        }

        return leaf.myUid;
    }
    //iteration von rechts
    long snapDown(long uid) {
        Leaf<T> leaf = findLeaf(uid);

        if (leaf.myUid != uid) {
            Child<T> child;
            Node<T> parent = leaf.myParent;
            // 1. go up till highest difference bit level
            int bit = 63-Long.numberOfLeadingZeros(uid ^ leaf.myUid);
            while (parent.myBit < bit) parent = parent.myParent;

            if ((uid & (1L << bit)) == 0) {
                // 2a. if that bit is one in uid: go up
                while (!parent.bitOne(uid)) {
                    parent = parent.myParent;
                    if (parent == null) return -1;
                }
                child = parent.myZero;
            } else {
                // 2b. if that bit is zero in uid: go down
                child = parent.getChild(uid);
            }
            // 3. go down, keep right
            while (child instanceof Node) {
                child = ((Node<T>)child).myOne;
            }
            leaf = (Leaf<T>)child;
        }

        return leaf.myUid;
    }
    // iteration durch leafs
    long next(long uid) {
        if (uid == -1) return 0;
        return snapUp(uid+1);
    }
    // iteration durch leafs
    long prev(long uid) {
        if (uid == 0) return -1;
        return snapDown(uid-1);
    }

    public String toString() {
        return "{"+myRoot+"}";
    }

    public void byLevel(){
        Node<T> root = myRoot;
        Queue<Node<T>> level  = new LinkedList<>();
        level.add(root);
         int lvl = 0;
        while(!level.isEmpty()){
            Node<T> node = level.poll();
            System.out.println(node.nr + " " +"LVL "+lvl);
            if(node.myZero instanceof Node)
                level.add((Node<T>)node.myZero);
            else if (node.myZero instanceof Leaf)
                System.out.println("Leaf");
            if(node.myOne instanceof Node)
                level.add((Node<T>)node.myOne);
            else if (node.myZero instanceof Leaf)
                System.out.println("Leaf");

             lvl = lvl +1;
        }
    }

    public void printByLevel(){
        printBreadthFristLike(0,myRoot);
    }
    void printBreadthFristLike(int lvl,Child<T> ele){
        if(ele instanceof Leaf){
            Leaf<T> leaf = (Leaf<T>)ele;
            System.out.println("LEAF onLvL: "+lvl+" NR: " +leaf.nr+" payload"+leaf.myPayload);
        }
        else if (ele instanceof Node){
            Node<T> node = (Node<T>)ele;
            System.out.println("Node onLvL: "+lvl+" NR: " +node.nr);
            printBreadthFristLike(lvl+1,node.myZero);
            printBreadthFristLike(lvl+1,node.myOne);
        }

    }
}