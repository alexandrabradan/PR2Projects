import java.util.*;


public class SortMap {
    //Overview: classe che presa una classe che implementa l'interfaccia DataCounter, la ordina in ordine decrescente
    //          di valori, ed a parita' di valori per ordine lessicografico delle chiavi
    ;
    private SortedSet<Map.Entry<String, Integer>> sortedSet;

    public SortMap(DataCounter<String> f){
        sortedSet= new TreeSet<Map.Entry<String, Integer>>(
                new Comparator<Map.Entry<String, Integer>>() {
                    @Override
                    public int compare(Map.Entry<String, Integer> o1, Map.Entry<String, Integer> o2) {
                        int res = o1.getValue().compareTo(o2.getValue());
                        return res != 0 ? -res : o1.getKey().compareTo(o2.getKey());

                    }
                }
        );

        Iterator<String> it = f.getIterator();
        while (it.hasNext()) {
            String str = it.next();
            sortedSet.add(new AbstractMap.SimpleEntry<String, Integer>(str, f.getCount(str)));
        }
    }
    /*
    EFFECTS: costruisce, a partire da una classe che implementa l'interfaccia DataCounter, un insime ordinato per valori
             decrescenti ed a parita' di valori  per ordine lessicografico delle chiavi
     */

    /*
        EFFECTS: restituisce un iteratore per un SortedSet
         */
    public Iterator<Map.Entry<String, Integer>> getSortedSetIterator(){
        return sortedSet.iterator();
    }
        /*
        EFFECTS: restituisce un iteratore per un SortedSet
         */
}
