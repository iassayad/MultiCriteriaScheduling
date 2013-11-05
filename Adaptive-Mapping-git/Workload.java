// javac Workload.java; java -ea Workload


import java.util.*;
import java.io.*;




/*
Simualte a workload variation of the current set of applications 
in order to test the adaptive mapping and measure
the global consumed energy and global number of user processors.
*/
/*
Workload 1  Period-mode :
An application G1 enters with a period constraint p1
Map G1 
An application G2 enters with a period constraint p2 
Map G1 and G2
An application G3 then G4 enter (before flushing G1 and G2 pipelines) with a period constraints p3 and p4 
Map G1, G2, G3 and G4
G2 quits then G5 enters (before flushing ...) with a period constraint p5
Map G1, G3, G4 and G5
*/
/*
Workload 2  Period-mode then power-mode :
An application G1 enters with a period constraint p1
Map G1 
An application G2 enters with a period constraint p2 
Map G1 and G2
Power-constrained mode due to an unplug of the phone from the car or a decrease of battery level 
Map G1 and G2
An application G3 then G4 enter (before flushing G1 and G2 pipelines) with a period constraints p3 and p4 
Map G1, G2, G3 and G4
G2 quits then G5 enters (before flushing ...) with a period constraint p5
Map G1, G3, G4 and G5
*/
class Workload
{  

private static double getAverage(Vector<Integer> nprocs){
    double sum = 0.0;
    for (int i = 0; i < nprocs.size(); i++){ 
        sum +=  nprocs.get(i); 
    } 
    return (sum / nprocs.size()); 	
}



private static double getSum(Vector<Double> cenergies){
    double sum = 0.0;
    for (int i = 0; i < cenergies.size(); i++){ 
        sum += cenergies.get(i); 
    } 
    return sum; 	
}


	private static void addGraph(GraphPareto G, Adaptive adaptiveMapping, Vector<Double> cenergies, Vector<Integer> nprocs,  Vector<Double> cpowers, Vector<Integer> applinumber    ){
		adaptiveMapping.addApplicationGraphPareto(G);
		System.out.println("---------------------- Adaptive Mapping  --------------------");
		adaptiveMapping.adaptSchedule();
		

		if (!adaptiveMapping.isMappingFailure()) {
			/*System.out.println("---> Number of Used processors ? "+ adaptiveMapping.getNumberOfUsedProcessors());
			System.out.println("---> Number of Unused processors ? "+ adaptiveMapping.getNumberOfUnusedProcessors());
			System.out.println("---> Consumed Energy ? "+ adaptiveMapping.getConsumedEnergy());*/
			cenergies.add(adaptiveMapping.getConsumedEnergy());
			//cpowers.add(adaptiveMapping.getConsumedPower());
			nprocs.add(adaptiveMapping.getNumberOfUsedProcessors());
			applinumber.add(adaptiveMapping.getApplinumber());
		} else {
			cenergies.add(0.0);
			//cpowers.add(0.0);
			nprocs.add(0);	
			applinumber.add(adaptiveMapping.getApplinumber());
			System.out.println("---> Number of Needed processors ? "+ adaptiveMapping.getNumberOfNeededProcessors());
			return;
		}	
	}

// ############## MILP ###############
 public static void main(String args[])
    {
	Vector<Double> cenergies = new Vector();
	Vector<Double> cpowers = new Vector();
	Vector<Integer> nprocs = new Vector();
	Vector<Integer> applinumber = new Vector();
	String output = "";
	double period = 100.0;
	int numberOftodaesOperations = 8;
	String graphbased = "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/";
	String resultsbased = "/home/popart/assayad/Desktop/Adaptive-milp-todaes-results/"+numberOftodaesOperations+"op/MILP-";
	GraphPareto G1 = new GraphPareto( period, graphbased+"8op-tuilescarre/G1bisperiod_rep1_nbop8.txt", graphbased+"8op-tuilescarre/G1bisnbproc_rep1_nbop8.txt",graphbased+"8op-tuilescarre/G1bispower_rep1_nbop8.txt" );
	GraphPareto G2 = new GraphPareto( period, graphbased+"8op/G2period_rep1_nbop8.txt", graphbased+"8op/G2nbproc_rep1_nbop8.txt",graphbased+"8op/G2power_rep1_nbop8.txt" );
	GraphPareto G3 =  new GraphPareto( period, graphbased+"8op/G3period_rep1_nbop8.txt", graphbased+"8op/G3nbproc_rep1_nbop8.txt",graphbased+"8op/G3power_rep1_nbop8.txt" );
	GraphPareto G4 =  new GraphPareto( period, graphbased+"8op/G4period_rep1_nbop8.txt", graphbased+"8op/G4nbproc_rep1_nbop8.txt",graphbased+"8op/G4power_rep1_nbop8.txt" );
	GraphPareto G5 =  new GraphPareto( period, graphbased+"8op/G5period_rep1_nbop8.txt", graphbased+"8op/G5nbproc_rep1_nbop8.txt",graphbased+"8op/G5power_rep1_nbop8.txt" );
	GraphPareto G6 =  new GraphPareto( period, graphbased+"8op/G2period_rep1_nbop8.txt", graphbased+"8op/G2nbproc_rep1_nbop8.txt",graphbased+"8op/G2power_rep1_nbop8.txt" );
	GraphPareto G7 =  new GraphPareto( period, graphbased+"8op/G3period_rep1_nbop8.txt", graphbased+"8op/G3nbproc_rep1_nbop8.txt",graphbased+"8op/G3power_rep1_nbop8.txt" );
	GraphPareto G8 =  new GraphPareto( period, graphbased+"8op/G4period_rep1_nbop8.txt", graphbased+"8op/G4nbproc_rep1_nbop8.txt",graphbased+"8op/G4power_rep1_nbop8.txt" );
	System.out.println("####################### Workload 1 BEGIN ##############################");
	Adaptive adaptiveMapping = new Adaptive();
	int archisize = 16;
	adaptiveMapping.setArchiSize(archisize);
	
	addGraph(G1, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);
	
	addGraph(G2, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G3, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G4, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G5, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);
	
	addGraph(G6, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G7, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G8, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);
	//addGraph(G6, adaptiveMapping, cenergies, nprocs);
	

	/*adaptiveMapping.removeApplicationGraphPareto(G2);
	adaptiveMapping.addApplicationGraphPareto(G5);
	System.out.println("---------------------- Adaptive Mapping (G1 -- G3 G4 G5) --------------------");
	adaptiveMapping.adaptSchedule();
	cenergies.add(adaptiveMapping.getConsumedEnergy());
	nprocs.add(adaptiveMapping.getNumberOfUsedProcessors());
	System.out.println("---> Success ? "+ !adaptiveMapping.isMappingFailure());
         */

	
     
	System.out.println("####################### Workload 1 END ##############################");

	// Compute the sum of the energy  over the 4  configurations above
	// Compute the sum (or the average ?) of number of used processor over the 4 configuration above
	// Compare the two preceeding measures with ones obtained using (A.K. Simgh in TODAES 2013 and Basten et al in Euromicro 2010)
	// Do the same for different types of scenaris 
	System.out.println("---> Archi size ? "+ adaptiveMapping.getArchiSize());
	System.out.println("---> Average Number of Used processors ? "+ Workload.getAverage(nprocs));
	System.out.println("---> Consumed Energy ? "+ Workload.getSum(cenergies));



	File out_channel1 = create_file (resultsbased+"usedproc-Archi"+archisize+"nbop"+numberOftodaesOperations+".txt");
	File out_channel2 = create_file (resultsbased+"energy-Archi"+archisize+"nbop"+numberOftodaesOperations+".txt");
	File out_channel3 = create_file (resultsbased+"applinumber-Archi"+archisize+"nbop"+numberOftodaesOperations+".txt");
	for (int i = 0; i < nprocs.size(); i++){ 
		write_file(out_channel1, ""+nprocs.elementAt(i));
		write_file(out_channel1, "\n");
   	 } 

	for (int i = 0; i < cenergies.size(); i++){ 
		write_file(out_channel2, ""+cenergies.elementAt(i));
		write_file(out_channel2, "\n");
   	 } 
	
	for (int i = 0; i < applinumber.size(); i++){ 
		write_file(out_channel3, ""+applinumber.elementAt(i));
		write_file(out_channel3, "\n");
   	 } 

	 mainTODAES(null);
   } // end main



// ############## TODAES ###############
 public static void mainTODAES(String args[])
    {
	Vector<Double> cenergies = new Vector();
	Vector<Double> cpowers = new Vector();
	Vector<Integer> nprocs = new Vector();
	Vector<Integer> applinumber = new Vector();
	String output = "";
	double period = 100.0;
	int numberOftodaesOperations = 8;

	String graphbased = "/home/popart/assayad/Desktop/Adaptive-todaes-results/";
	String resultsbased = "/home/popart/assayad/Desktop/Adaptive-milp-todaes-results/"+numberOftodaesOperations+"op/TODAES-";
	GraphPareto G1 = new GraphPareto( period, graphbased+"8op-tuilescarre/G1bisperiod_nbop8.txt", graphbased+"8op-tuilescarre/G1bisnbproc_nbop8.txt",graphbased+"8op-tuilescarre/G1bispower_nbop8.txt" );
	GraphPareto G2 = new GraphPareto( period, graphbased+"8op/G2period_nbop8.txt", graphbased+"8op/G2nbproc_nbop8.txt",graphbased+"8op/G2power_nbop8.txt" );
	GraphPareto G3 =  new GraphPareto( period, graphbased+"8op/G3-heterperiod_nbop8.txt", graphbased+"8op/G3-heternbproc_nbop8.txt",graphbased+"8op/G3-heterpower_nbop8.txt" );
	GraphPareto G4 =  new GraphPareto( period, graphbased+"8op/G4-heterperiod_nbop8.txt", graphbased+"8op/G4-heternbproc_nbop8.txt",graphbased+"8op/G4-heterpower_nbop8.txt" );
	GraphPareto G5 =  new GraphPareto( period, graphbased+"8op/G5-heterperiod_nbop8.txt", graphbased+"8op/G5-heternbproc_nbop8.txt",graphbased+"8op/G5-heterpower_nbop8.txt" );
	GraphPareto G6 =  new GraphPareto( period, graphbased+"8op/G2period_nbop8.txt", graphbased+"8op/G2nbproc_nbop8.txt",graphbased+"8op/G2power_nbop8.txt" );
	GraphPareto G7 =  new GraphPareto( period, graphbased+"8op/G3-heterperiod_nbop8.txt", graphbased+"8op/G3-heternbproc_nbop8.txt",graphbased+"8op/G3-heterpower_nbop8.txt" );
	GraphPareto G8 =  new GraphPareto( period, graphbased+"8op/G4-heterperiod_nbop8.txt", graphbased+"8op/G4-heternbproc_nbop8.txt",graphbased+"8op/G4-heterpower_nbop8.txt" );
	System.out.println("####################### Workload 1 BEGIN ##############################");
	Adaptive adaptiveMapping = new Adaptive();
	int archisize = 16;
	adaptiveMapping.setArchiSize(archisize);
	
	addGraph(G1, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);
	
	addGraph(G2, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G3, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G4, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G5, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);
	
	addGraph(G6, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G7, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	addGraph(G8, adaptiveMapping, cenergies, nprocs, cpowers, applinumber);

	//addGraph(G6, adaptiveMapping, cenergies, nprocs);
	

	/*adaptiveMapping.removeApplicationGraphPareto(G2);
	adaptiveMapping.addApplicationGraphPareto(G5);
	System.out.println("---------------------- Adaptive Mapping (G1 -- G3 G4 G5) --------------------");
	adaptiveMapping.adaptSchedule();
	cenergies.add(adaptiveMapping.getConsumedEnergy());
	nprocs.add(adaptiveMapping.getNumberOfUsedProcessors());
	System.out.println("---> Success ? "+ !adaptiveMapping.isMappingFailure());
         */

	
     
	System.out.println("####################### Workload 1 END ##############################");

	// Compute the sum of the energy  over the 4  configurations above
	// Compute the sum (or the average ?) of number of used processor over the 4 configuration above
	// Compare the two preceeding measures with ones obtained using (A.K. Simgh in TODAES 2013 and Basten et al in Euromicro 2010)
	// Do the same for different types of scenaris 
	System.out.println("---> Archi size ? "+ adaptiveMapping.getArchiSize());
	System.out.println("---> Average Number of Used processors ? "+ Workload.getAverage(nprocs));
	System.out.println("---> Consumed Energy ? "+ Workload.getSum(cenergies));



	File out_channel1 = create_file (resultsbased+"usedproc-Archi"+archisize+"nbop"+numberOftodaesOperations+".txt");
	File out_channel2 = create_file (resultsbased+"energy-Archi"+archisize+"nbop"+numberOftodaesOperations+".txt");
	File out_channel3 = create_file (resultsbased+"applinumber-Archi"+archisize+"nbop"+numberOftodaesOperations+".txt");
	for (int i = 0; i < nprocs.size(); i++){ 
		write_file(out_channel1, ""+nprocs.elementAt(i));
		write_file(out_channel1, "\n");
   	 } 

	for (int i = 0; i < cenergies.size(); i++){ 
		write_file(out_channel2, ""+cenergies.elementAt(i));
		write_file(out_channel2, "\n");
   	 } 

	for (int i = 0; i < applinumber.size(); i++){ 
		write_file(out_channel3, ""+applinumber.elementAt(i));
		write_file(out_channel3, "\n");
   	 } 
	
   } // end main




public static File create_file(String n){
		try {
        		File file = new File(n);
        		boolean success = file.createNewFile();
        		if (success) {
        		} else {
				// File already exists
        			if (file.delete())
        				assert(file.createNewFile());
        			else assert(false);
        		}
        		return file;
    		} catch (IOException e) {
    			System.out.println("n ="+n);
    			e.printStackTrace();
    			System.exit(0);
    			return null;
    		}
    
	}
    
	public static void write_file(File f, String n){
		try {
			FileWriter fw = new FileWriter(f, true);
			fw.write(n);
			fw.close();
		  }catch (Exception e){
			System.err.println("Error: " + e.getMessage());
	    		System.out.println("n ="+n);

	  		e.printStackTrace();
	  		System.exit(0);
		  }
	  
	}


	
} // end class

