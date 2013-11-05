import java.util.*;
import java.io.*;



public class TodaesMain {
	public static double en = 0;
	public static double len = 0;
	public static ArrayList<Lien> liens = new ArrayList<Lien>();
	//public static ArrayList<Lien> liensSiConnexe = new ArrayList<Lien>(); // l ensemble des liens si l archi etait completent connectee 
	public static ArrayList<Arc> Arcs = new ArrayList<Arc>();
	public static Operateur operateurs[];
	public static TodaesOperation todaesOperations[];
	public static int temps;
	public static Extraction extraction = new Extraction();
	public static TodaesAlgorithme todaesAlgo = new TodaesAlgorithme();
	public static ArrayList<TodaesOperation> opPrete = new ArrayList<TodaesOperation>();
	public static double POBJ = 1;
	public static String mode="";

			// contains numberOftodaesOperations mapping, each is a vector of tiles elements, each is a set of todaesOperations identifier
			// ex : < <<A>  <B>  <C>>  <<A> <B C>> <<A B C>> >
	public static Vector<Vector<Vector<String>>> todaesBestMappings; // todaes paretos 
	public static Vector<Double> consumedEnergy; // todaes paretos consumed energies, in decreasing order of procs number as above
	public static Vector<Double> periods; // todaes paretos periods, in decreasing order of procs number as above
	public static Vector<Integer> nprocs; //todqes paretos nprocs
			

	public static void main(String[] args) throws java.io.IOException,
	                                   InputDataReader.InputDataReaderException {
		//extraction.LireTodaesOperationFichier(); // On lit les todaesOperations ainsi que les liaisons entre elles
		//extraction.LirePostesFichier(); // On lit Les postes et leurs durees

		//String graphenumber="1bis";
		String graphenumber="6-heter";
		String filename = "/home/popart/assayad/workspace/ILPINRIA-2011-2012-batch-et-simple/data/FMDEP/tasks8-4freq-ok-G"+graphenumber+".txt";
		extraction.LireMILPfichierSaufDureeArcs(filename);
		int nb_operateur=operateurs.length;
		//Lien.InitLien(nb_operateur); //plus besoin de ca car extraction s en occupe. utilise uniquement pour le calucl des Minchemin, ensuite on enlve les chemins qui n appartiennent pas a l archi
		
		assert(nb_operateur >= 4 && nb_operateur <=8);
		//liens = Chemin.initialiser_archi_liens_noc(nb_operateur, "carre");
		liens = Chemin.initialiser_archi_liens_noc(nb_operateur, "ligne");
		// afficher les liens et les todaesOperations
		//extraction.LireDureesArc(); // On lit les durees des arcs sur les liens
		extraction.LireMILPfichierDureeArcs(filename);		
		AffichagePoste();
		AffichageArc();
		if(args.length >= 1) mode = args[0]; // prendre en compte la contrainte d energie dans la fonction de cout si mode ="-l"
		long start = System.currentTimeMillis();
		System.out.println("---> Todaes ...");

		
		int numberOftodaesOperations = TodaesMain.todaesOperations.length;
		int tiles= numberOftodaesOperations;
		todaesAlgo.initTodaesBestMappings(); // with as tiles as todaesOperations number
		assert(todaesBestMappings.size() == 1 + numberOftodaesOperations - tiles);
		while(tiles >=2){
		    tiles = tiles - 1;
                    todaesAlgo.findBestSolutionToPutInTodaesBestMappings(tiles); // Best solution using tiles proc
		    assert(todaesBestMappings.size() == 1 + numberOftodaesOperations - tiles);
			
                }
		assert(todaesBestMappings.size() == numberOftodaesOperations);

		System.out.println("<--- Todaes ...");
    		long end = System.currentTimeMillis();
    		Double time = (end - start)/1000. ;

		//String based = "/home/popart/assayad/Desktop/Adaptive-todaes-results/"+numberOftodaesOperations+"op-tuilescarre";
		String based = "/home/popart/assayad/Desktop/Adaptive-todaes-results/"+numberOftodaesOperations+"op";
		//String based = "/home/ismail/Desktop/tsh-results-sttt-optimal-5op-depardon/";
	       

		System.out.println("Todaes Adapt graph number G"+graphenumber);
	        File out_channel1 = create_file (based+"/"+"G"+graphenumber+"nbproc_nbop"+numberOftodaesOperations+".txt");
	        File out_channel2 = create_file (based+"/"+"G"+graphenumber+"power_nbop"+numberOftodaesOperations+".txt");
	        File out_channel3 = create_file (based+"/"+"G"+graphenumber+"period_nbop"+numberOftodaesOperations+".txt");
	        File out_channel5 = create_file (based+"/"+"G"+graphenumber+"period_obj_nbop"+numberOftodaesOperations+".txt");
	        File out_channel7 = create_file (based+"/"+"G"+graphenumber+"time_nbop"+numberOftodaesOperations+".txt");
		
	/*	for (int i=todaesBestMappings.size()-1; i >= 0  ; i--){
		 	write_file(out_channel1, ""+TodaesMain.todaesBestMappings.elementAt(i).size());
	         	write_file(out_channel2, ""+TodaesAlgorithme.computeExactTodaesConsumedEnergy(todaesBestMappings.elementAt(i))/computeMaxPeriods(todaesBestMappings));
	         	write_file(out_channel3, ""+TodaesAlgorithme.computeExactTodaesPeriod(todaesBestMappings.elementAt(i)));
	  	 	write_file(out_channel7, ""+time);

			write_file(out_channel1, "\n");
			write_file(out_channel2, "\n");
			write_file(out_channel3, "\n");
			write_file(out_channel7, "\n");
		} */

		// iterate on period objectives (same as MILP objectives for a fair comparison) 
		// then take the todaes best solution, i.e., with less number of processors
		// store the results in the trace files
	        Double Wo; // Period obj (in case of pipelined executions) : computed by program, not an input
		Boolean sol =false;
		for (int ind = 14; ind > 0; ind--){ // wmax = 50%wsup; 45%wsup, 40%wsup, 35%wsup, 30%wsup, ..., 5%wsup
			Wo = ind*0.025*compute_wsup_pipeline(); // 35%w 32,5%w 30%w 27.5%w 25%w 22,5%w 20%w 17,5%w 15%w ... 2,5%w
			sol =false;
			//for (int i=todaesBestMappings.size()-1; i >= 0  ; i--){ // ??? read values from files to separate the two daes algorithms
			for (int i=todaesBestMappings.size()-1; i >= todaesBestMappings.size()-1 - 4  ; i--){ // ??? read values from files to separate the two daes algorithms
				// 4 means we keep only up to 5 processors solutions in order to be in the same situation as milp for a fair comparison
				if (TodaesAlgorithme.computeExactTodaesPeriod(todaesBestMappings.elementAt(i)) <= Wo){
					write_file(out_channel1, ""+TodaesMain.todaesBestMappings.elementAt(i).size());
	         			write_file(out_channel2, ""+TodaesAlgorithme.computeExactTodaesConsumedEnergy(todaesBestMappings.elementAt(i))/Wo);
	         			write_file(out_channel3, ""+TodaesAlgorithme.computeExactTodaesPeriod(todaesBestMappings.elementAt(i)));
					write_file(out_channel5, Wo+" ");
	  	 			write_file(out_channel7, ""+time);

					write_file(out_channel1, "\n");
					write_file(out_channel2, "\n");
					write_file(out_channel3, "\n");
					write_file(out_channel5, "\n");
					write_file(out_channel7, "\n");
					sol = true;
					break;
				}	
			}
			
			if (!sol)  {
				System.out.println(" No solution");
				write_file(out_channel1, "NaN ");
				write_file(out_channel2, "NaN ");
				write_file(out_channel3, "Nan ");
				write_file(out_channel5, Wo+" ");
				write_file(out_channel7, "NaN" +" ");
				
				write_file(out_channel1, "\n");
				write_file(out_channel2, "\n");
				write_file(out_channel3, "\n");
				write_file(out_channel5, "\n");
				write_file(out_channel7, "\n");
				continue;
			}
					
		}


		
		//
		//AfficheLien();
		//AfficheTodaesOperation();
		//AfficheR();
		//Graphique graphe=new Graphique();
		//graphe.setVisible(true );




	} // end main

	public static double computeMaxPeriods(Vector<Vector<Vector<String>>> todaesBestMappings){
		int max = 0;

		double maxPeriod =  TodaesAlgorithme.computeTodaesPeriod(todaesBestMappings.elementAt(max));


		for (int i=1; i<todaesBestMappings.size(); i++){
			assert(todaesBestMappings.elementAt(i).size() != todaesBestMappings.elementAt(0).size());
			//double consumedEnergy = computeConsumedenergy(todaesMappings.elementAt(i)); // compute energy ???
			double period = TodaesAlgorithme.computeTodaesPeriod(todaesBestMappings.elementAt(i));
			if (period > maxPeriod) {
				maxPeriod = period;
				max = i;
			}
		}
		return maxPeriod;
	}


	 private static double compute_wsup_pipeline() {

		double Sup = 0;
		double pfreq0 = 0.25; // we have to compute dame period objectives as milp for a fair comparison
		for (int j = 0; j < todaesOperations.length; j++){
			double max = todaesOperations[j].getDurees()[0]/pfreq0;
			assert(max > 0);
			Sup = Sup + max;
		}
		for (int i = 0; i < Arcs.size(); i++)
						Sup = Sup + Arcs.get(i).getDurees()[0];
						
		assert(Sup > 0);
		return Sup;
	  }
  

	public static void AffichagePoste(){
		for(int i=0;i<operateurs.length;i++){
			System.out.println(operateurs[i]);
		}
	}
	public static void AfficheLien(){
		for(int i=0;i<liens.size();i++){
			System.out.println("Lien :  "+liens.get(i).getLibelle());
			System.out.println("Operateur 1 : "+liens.get(i).getOperateur1()+", Operateur 2 :"+liens.get(i).getOperateur2());
			System.out.println("Communication : "+liens.get(i).getComminication());
		}
	}
	public static void AffichageArc(){
		for(int i=0;i<TodaesMain.Arcs.size();i++)
			System.out.println(TodaesMain.Arcs.get(i));
	}
	public static void AfficheTodaesOperation(){
                for(int i=0;i<todaesOperations.length;i++)
                    System.out.println(todaesOperations[i]);
	}
	/*public static void AfficheR(){
		System.out.println(" L " + len);
		System.out.println(" P " + en/len);
	}*/


	public static TodaesOperation getOperation(String libelleTodaesOperation) {
		
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			if(TodaesMain.todaesOperations[i].getLibelle().equals(libelleTodaesOperation))
				return TodaesMain.todaesOperations[i];
		}
		assert(false);
		return null;
	}

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
