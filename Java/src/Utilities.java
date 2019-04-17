import java.util.*;

public class Utilities{
    //Overview: classe di supporto della classe DataCounterTester, che provvede a:
    //          1) ripulire dalla punteggiatura e dalle lettere maiuscole, riga per riga di un file di testo che il
    //             main passa alla suddetta classe
    //          2) inserire ogni riga ripulita, dividendola in parole, in una HashTable e in un TreeMap, conteggiando
    //             la frequenza di ogni parola inserita
    //          3) ordinare le due strutture in cui sono state inserire le parole e le relative frequenze, in ordine
    //             decrescente di frequenza, ed a parita' di frequenza in ordine lessicografico di parola
    //          4) stampare l'ordinamento ottenuto

    private DataCounterHashTable<String> frequencies;
    private DataCounterTreeMap<String> frequencies2;

    public Utilities() {
        frequencies = new DataCounterHashTable<>();;
        frequencies2 = new DataCounterTreeMap<>();
    }
    /*
    costruisce un oggetto DataCounterHashTable e DataCounterTreeMap vuoti
     */

    /*
    inizializza gli oggetti DataCounterHashTable e DataCounterTreeMap, prendendo una riga da un file di testo,
    eliminandovi i simboli di punteggiatura, convertendovi le lettere maiuscole in minuscole e dividendo tale riga
    (ogni qualvolta si incontrano uno o piu' spazi) in parole da inserire nei due oggetti (a patto che ogni parola,
     dopo la pulizia effettuata, contenga almeno un carattere lecito)
     */
    public void insertLineInDataStructures(String sCurrentLine) {
        String[] r = sCurrentLine.replaceAll("[[^\\w\\d\\sאטילעש]]", " ").toLowerCase().split("\\s+");
        for (String str : r) {
            if (!str.isEmpty()) {
                frequencies.incCount(str);
                frequencies2.incCount(str);
            }
        }
    }
    /*
    EFFECTS: inizializza gli oggetti frequencies e frequencies2, prendendo una riga da un file di testo,  eliminandovi i
             simboli di punteggiatura, convertendovi le lettere maiuscole in minuscole e dividendo tale riga (ogni
             qualvolta si incontrano uno o piu' spazi) in parole da inserire nei due oggetti (a patto che ogni parola,
             dopo la pulizia effettuata, contenga almeno un carattere lecito)
     */

    /*
    EFFECTS: stampa il risultato atteso, dopo aver ordiato i due oggetti DataCounterHashTable e DataCounterTreeMap
             in ordine decrescente di valori ed a parita' di valore, in ordine lessicografico di chiavi
     */
    public void printResult() {
        SortMap sortedSet = new SortMap(frequencies);
        SortMap sortedSet2 = new SortMap(frequencies2);

        System.out.println("FREQUEZA PAROLE HASHTABLE");
        Iterator<Map.Entry<String, Integer>> it = sortedSet.getSortedSetIterator();
        while (it.hasNext()) {
            Map.Entry<String, Integer> x = it.next();
            System.out.println(x.getKey() + " " + x.getValue());
        }

        System.out.println();
        System.out.println("--------------------------");
        System.out.println();

        System.out.println("FREQUEZA PAROLE TREEMAP");
        Iterator<Map.Entry<String, Integer>> it2 = sortedSet2.getSortedSetIterator();
        while (it2.hasNext()) {
            Map.Entry<String, Integer> x = it2.next();
            System.out.println(x.getKey() + " " + x.getValue());
        }
    }
    /*
    EFFECTS: stampa il risultato atteso, dopo aver ordiato i due oggetti frequencies e frequencies2
             in ordine decrescente di valori ed a parita' di valore, in ordine lessicografico di chiavi.
             Tale ordinamento viene effettuato tramite l'ausilio della classe SortMap, che servendosi dei due
             oggetti sortedSet e sortedSet2, ordina rispettivamente, frequencies e frequencies2
     */
}
