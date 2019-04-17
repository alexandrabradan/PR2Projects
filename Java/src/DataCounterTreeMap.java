import java.util.Map;
import java.util.TreeMap;
import java.util.Iterator;


public class DataCounterTreeMap<E> implements DataCounter<E> {
    //Overview: tipo modificabile di una funzione con dominio E e codominio V, che vuole rapprensetare
    //          la frequenza di parole in un file di testo
    //AF: f: E -> V con {e | f(e) >= 1}
    //IR: for all e in E => f(e) >= 1 && for all e1, e2 in E => e1 != e2

    private Map<E, Integer> frequencies;

    public DataCounterTreeMap(){
        frequencies = new TreeMap<>();
    }
    /*
    EFFECTS: costruisce un TreeMap vuoto
     */

    // incrementa il valore associato all’elemento data di tipo E
    public void incCount(E data){
        frequencies.put(data, this.getCount(data) + 1);
    }
    /*
    EFFECTS: incrementa f(data)
     */


    // restituisce il numero degli elementi presenti nella collezione
    public int getSize(){
        return frequencies.size();
    }
    /*
    EFFECTS: restituisce sum{e | f(e) >= 1}
     */


    // restituisce il valore corrente associato al parametro data,
    // e 0 se data non appartiene alla collezione
    public int getCount(E data){
        if (frequencies.get(data) == null)
            return 0;
        else return frequencies.get(data);
    }
    /*
    EFFECTS: restiruisce f(data) se f(data) >= 1, altrimenti 0
     */


    // restituisce un iteratore (senza remove) per la collezione
    public Iterator<E> getIterator(){
        return new MyIterator<E>(frequencies.keySet().iterator());
    }
   /*
    EFFECTS: restituisce un iteratore(senza remove), avvalendosi della creazione di un nuovo oggetto della classe
             MyIterator, per  invalidare  il metodo remove. Tale oggetto creato ha bisogno di prende come paramentro
             l'iteratore della  mappa trasformata in insieme, per poter simulare i metodi hasNext e next
     */
}
