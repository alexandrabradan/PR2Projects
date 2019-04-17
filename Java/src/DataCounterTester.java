import javax.swing.*;
import javax.swing.filechooser.FileNameExtensionFilter;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Scanner;

public class DataCounterTester{
    //Overview: classe che testa i metodi delle classi DataCounterHashTable e DataCounterTreeMap, avvalendosi di
    //          una classe di supporto (avente differenti mansioni) Utilities, il cui compito principale e'
    //          quello di leggere un file di testo, e se la lettura non procede a buon fine, di segnalarlo

    public static void main(String[] args){
        Utilities utilities = new Utilities();

        /*
        legge un file riga per riga e ciascuna riga viene passata all'oggetto utilities, che provvedera' a ripulirla
        ed inserirla, divisa in parole, in una HashTable ed in un TreeMap
         */
        try{
            String currentDir = System.getProperty("user.dir");
            System.out.println("Al momento ti trovi nella cartella:" + currentDir);
            System.out.println();
            System.out.println("Dalla cartella Inputs scegli un file ed inserisci qui il numero di file da testare (0 - 10)");
            Scanner input = new Scanner(System.in);
            int n = input.nextInt();
            while (n < 0 || n > 10){
                System.out.println("Errore nell'inserimento");
                System.out.println("Inserisci un numero lecito di file da testare (0 - 10) dalla cartella Inputs");
                n = input.nextInt();
            }
            String fileName = "input" + n + ".txt";
            FileReader fr = new FileReader(System.getProperty("user.dir") + "/Inputs/" +  fileName);
            BufferedReader br = new BufferedReader(fr);

            String sCurrentLine;

            while ((sCurrentLine = br.readLine()) != null){
                utilities.insertLineInDataStructures(sCurrentLine);
            }

            utilities.printResult();

        } catch(IOException e){
            System.out.println(e.getMessage());
        }
    }
}
