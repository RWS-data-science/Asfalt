import weka.core.Instances;
import weka.core.converters.ConverterUtils.DataSource;
import weka.classifiers.Classifier;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.EuclideanDistance;
import weka.core.neighboursearch.LinearNNSearch;
import weka.core.neighboursearch.LinearNNSearch;
import weka.classifiers.evaluation.Evaluation;
import weka.classifiers.rules.ZeroR;
import weka.classifiers.trees.J48;
import weka.classifiers.trees.LMT;
import weka.classifiers.lazy.IBk;
import java.io.PrintWriter;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.Remove;
 
public class ExperimentRunner
{
 public static String getDataSet(String path,String concept, String variant, String year, Boolean test) throws Exception {
	 String str = path + concept;
	 if(test){
		 str = str + ".test";
	 } else {
		 str = str + ".train";
	 }
	 str = str + ".h" + year;
	 if(variant == "resampled"){
		 str = str + ".resampled";
	 }
	 str = str + ".arff";
	 return str;
 }
 public static String resultLine(String model, String concept,String variant, String year, Double a,Double b,Double c,Double d){
	 return model + ";" + concept + ";" + variant + ";" + year + ";" + a.toString() + ";" + b.toString() + ";" + c.toString() + ";" + d.toString();
 }
 public static void main(String args[]) throws Exception
 {
	 String path = "..\\..\\data\\output\\learning\\arff\\";
	 String[] concepts = {"sch","ver"};
	 String[] years = {"2012","2013","2011", "2014", "2015"};
	 String[] variants = {"normal", "resampled"};
	 
	 DataSource trainDataSource;
	 DataSource testDataSource;
	 
	 Instances trainData;
	 Instances testData;
	 Instances knnTrainData;
	 Instances knnTestData;
	 Instances SimpleTreeTrainData;
	 Instances SimpleTreeTestData;
	 Instances ComplexTreeTrainData;
	 Instances ComplexTreeTestData;
	 Instances NaiveBayesTestData;
	 Instances NaiveBayesTrainData;
	 
	 NaiveBayes Bayes = new NaiveBayes();
	 LMT complexTree = new LMT(); 
	 J48 simpleTree = new J48();
	 ZeroR zeror = new ZeroR();
	 IBk kNN = new IBk();
	 Evaluation Test;
	 
	 Remove remove = new Remove();
	 
	 PrintWriter  writer = new PrintWriter("..\\..\\data\\output\\learning\\WekaResults.csv");
	 writer.println("Model;Concept;Variant;Hold Out Year;Percentage Correct; Kappa;Mean Absolute Error; Mean Squared Error");
	 
	 
	 //THE MAIN LOOP
	 //We execute experiments with each year as a holdout
	 //For the normal and resampled sets
	 //Both for schade en verloop
	 //20 variants for each model
	 int tot = concepts.length * variants.length * years.length;
	 int progress = 1;
	 for (int i = 0; i < concepts.length; i++) {
		 for (int j = 0; j < variants.length; j++) {
			 for (int k = 0; k < years.length; k++) {
				 trainDataSource = new DataSource(getDataSet(path, concepts[i], variants[j], years[k], false));
				 testDataSource = new DataSource(getDataSet(path, concepts[i], variants[j], years[k], true));
				 trainData = trainDataSource.getDataSet();
				 trainData.setClassIndex(trainData.numAttributes() -1);
				 
				 testData = testDataSource.getDataSet();
				 testData.setClassIndex(testData.numAttributes() -1);
				 
				 //ZEROR 
				 zeror.buildClassifier(trainData);
				 Test = new Evaluation(trainData);
				 Test.evaluateModel(zeror, testData);
				 
				 writer.println(resultLine("ZeroR", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 //NAIVE BAYES
				 //Trained on the entire feature space
				 if(years[k] != "2014"){ //TODO Strange error
					 NaiveBayesTestData = testData;
					 NaiveBayesTrainData = trainData;
					 Bayes.buildClassifier(NaiveBayesTrainData);
					 Test = new Evaluation(NaiveBayesTrainData);
					 Test.evaluateModel(Bayes, NaiveBayesTestData);
					 
					 writer.println(resultLine("Bayes on all features", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 }
				 //KNN
				 //Trained on x,y
				 //Evaluated on 1, 3 & 5 Neighbours
				 //Evaluated on testdata & 10 fold cross validation over a single year
				 remove.setOptions(weka.core.Utils.splitOptions("-R 1-6,9-53")); 
				 remove.setInputFormat(trainData);                          
				 knnTrainData = Filter.useFilter(trainData, remove);  
				 knnTestData = Filter.useFilter(testData, remove);  
				 
				 kNN.setOptions(weka.core.Utils.splitOptions("-K 1 -W 0"));
				 kNN.buildClassifier(knnTrainData);
				 Test = new Evaluation(knnTrainData);
				 Test.evaluateModel(kNN, knnTestData);
				 writer.println(resultLine("1NN xy testdata", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 Test = new Evaluation(knnTrainData);
				 for (int l = 0; l < 10; l++) {
					  Instances curtrain = knnTestData.trainCV(10, l);
				      Instances curtest = knnTestData.testCV(10, l);
				      
				      //Classifier clsCopy = Classifier.makeCopy(kNN);
				      kNN.buildClassifier(curtrain);
				      Test.evaluateModel(kNN, curtest);
				 }
				 writer.println(resultLine("1NN xy 10 fold cross", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 kNN.setOptions(weka.core.Utils.splitOptions("-K 3 -W 0"));
				 kNN.buildClassifier(knnTrainData);
				 Test = new Evaluation(knnTrainData);
				 Test.evaluateModel(kNN, knnTestData);
				 writer.println(resultLine("3NN xy testdata", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 Test = new Evaluation(knnTrainData);
				 for (int l = 0; l < 10; l++) {
					  Instances curtrain = knnTestData.trainCV(10, l);
				      Instances curtest = knnTestData.testCV(10, l);
				      
				      //Classifier clsCopy = Classifier.makeCopy(kNN);
				      kNN.buildClassifier(curtrain);
				      Test.evaluateModel(kNN, curtest);
				 }
				 writer.println(resultLine("3NN xy 10 fold cross", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 kNN.setOptions(weka.core.Utils.splitOptions("-K 5 -W 0"));
				 kNN.buildClassifier(knnTrainData);
				 Test = new Evaluation(knnTrainData);
				 Test.evaluateModel(kNN, knnTestData);
				 writer.println(resultLine("5NN xy testdata", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 Test = new Evaluation(knnTrainData);
				 for (int l = 0; l < 10; l++) {
					  Instances curtrain = knnTestData.trainCV(10, l);
				      Instances curtest = knnTestData.testCV(10, l);
				      
				      //Classifier clsCopy = Classifier.makeCopy(kNN);
				      kNN.buildClassifier(curtrain);
				      Test.evaluateModel(kNN, curtest);
				 }
				 writer.println(resultLine("5NN xy 10 fold cross", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 //J48
				 //Trained on leeftijd and deklaagsoort
				 //The GPO Rule of Thumb
				 //Two variants of pruning
				 remove.setOptions(weka.core.Utils.splitOptions("-R 1-3,5,7-53")); 
				 remove.setInputFormat(trainData);                          
				 SimpleTreeTrainData = Filter.useFilter(trainData, remove);  
				 SimpleTreeTestData = Filter.useFilter(testData, remove);  
				 
				 simpleTree.setOptions(weka.core.Utils.splitOptions("-C 0.25"));
				 simpleTree.buildClassifier(SimpleTreeTrainData);
				 Test = new Evaluation(SimpleTreeTrainData);
				 Test.evaluateModel(simpleTree, SimpleTreeTestData);
				 writer.println(resultLine("J48 GPO baseline C 0.25", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 simpleTree.setOptions(weka.core.Utils.splitOptions("-C 0.05"));
				 simpleTree.buildClassifier(SimpleTreeTrainData);
				 Test = new Evaluation(SimpleTreeTrainData);
				 Test.evaluateModel(simpleTree, SimpleTreeTestData);
				 
				 writer.println(resultLine("J48 GPO baseline C 0.05", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 //LMT
				 //Trained on the top features as found by extensive studying (studies on same year though
				 //This is the best educated guess for now
				 remove.setOptions(weka.core.Utils.splitOptions("-R 2-3,5,11,13-20,22-24,26-34,36-44,46-53")); 
				 remove.setInputFormat(trainData);                          
				 ComplexTreeTrainData = Filter.useFilter(trainData, remove);  
				 ComplexTreeTestData = Filter.useFilter(testData, remove);  
				 
				 complexTree.buildClassifier(ComplexTreeTrainData);
				 Test = new Evaluation(ComplexTreeTrainData);
				 Test.evaluateModel(complexTree, ComplexTreeTestData);
				 
				 writer.println(resultLine("ComplexTree", concepts[i], variants[j], years[k],Test.pctCorrect(), Test.kappa(), Test.meanAbsoluteError(), Test.rootMeanSquaredError() ));
				 
				 //Prompting progress
				 System.out.println(progress++ + "/" + tot);
			 }
		 }
	 }
	 writer.close();
 }
}