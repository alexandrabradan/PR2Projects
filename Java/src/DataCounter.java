import java.util.Iterator;

public interface DataCounter<E> {
    //Overview: tipo modificabile di una funzione con dominio E e codominio V, che vuole rapprensetare
    //          la frequenza di parole in un file di testo
    // Typical element: f: E -> V con {e | f(e) >= 1}


    // incrementa il valore associato all’elemento data di tipo E
    public void incCount(E data);
    /*
    EFFECTS: incrementa f(data)
     */


    // restituisce il numero degli elementi presenti nella collezione
    public int getSize();
    /*
    EFFECTS: restituisce sum{e | f(e) >= 1}
     */


    // restituisce il valore corrente associato al parametro data,
    // e 0 se data non appartiene alla collezione
    public int getCount(E data);
    /*
    EFFECTS: restiruisce f(data) se f(data) >= 1, altrimenti 0
     */


    // restituisce un iteratore (senza remove) per la collezione
    public Iterator<E> getIterator();
    /*
    EFFECTS: restituisce un iteratore(senza remove) per la collezione
     */
}
