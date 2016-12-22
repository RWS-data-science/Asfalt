import weka.core.Instances;
import weka.core.converters.ArffSaver;
import weka.core.converters.CSVLoader;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.Remove;
import weka.filters.unsupervised.attribute.Reorder;
import weka.filters.unsupervised.attribute.NumericToNominal;
 
import java.io.File;
 
public class DataConverter
{
 /**
 * takes 2 arguments:
 * - CSV input file
 * - ARFF output file
 * @param sourcepath
 * @param destpath
 * @throws java.lang.Exception
 */
 public static void Convert(String sourcepath,String destpath) throws Exception
 {
 // load CSV
 CSVLoader loader = new CSVLoader();
 loader.setSource(new File(sourcepath));
 Instances data = loader.getDataSet();
 
 Remove remove = new Remove();                         // new instance of filter
 remove.setOptions(weka.core.Utils.splitOptions("-R 1")); //set the options
 remove.setInputFormat(data);                          // inform filter about dataset **AFTER** setting options
 data = Filter.useFilter(data, remove);   // apply filter

 Reorder reorder = new Reorder();
 reorder.setOptions(weka.core.Utils.splitOptions("-R first-29,31-last,30"));
 reorder.setInputFormat(data);                          
 data = Filter.useFilter(data, reorder);   

 NumericToNominal ntn = new NumericToNominal();
 ntn.setOptions(weka.core.Utils.splitOptions("-R first,last"));
 ntn.setInputFormat(data);                          
 data = Filter.useFilter(data, ntn); 
 
 // save ARFF
 ArffSaver saver = new ArffSaver();
 saver.setInstances(data);
 saver.setFile(new File(destpath));
 //saver.setDestination(new File(destpath));
 saver.writeBatch();
 }
 public static void main(String args[]) throws Exception
 {
    File folder = new File("..\\..\\data\\output\\learning\\csv\\");
    File[] listOfFiles = folder.listFiles();

    for (int i = 0; i < listOfFiles.length; i++) {
    	if (listOfFiles[i].isFile()) {
    	  String target = listOfFiles[i].getName();
    	  target = target.substring(0, target.lastIndexOf("."));
    	  System.out.println("converting file " + (i + 1) + "/" + listOfFiles.length);
    	  Convert("..\\..\\data\\output\\learning\\csv\\" + listOfFiles[i].getName(), "..\\..\\data\\output\\learning\\arff\\" + target + ".arff");
    	}
    }
 
 }
}