import java.util.Iterator;

public class MyIterator<E> implements Iterator<E> {
    //Overview: classe che prende un iteratore e ne invalida il metodo remove

    private Iterator<E> trueIterator;

    public MyIterator(Iterator<E> realIt){
        trueIterator = realIt;
    }

    public boolean hasNext(){
        return trueIterator.hasNext();
    }

    public E next(){
        return trueIterator.next();
    }

    public void remove(){
        throw new UnsupportedOperationException();
    }
}