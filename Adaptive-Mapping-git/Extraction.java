

import java.io.FileNotFoundException;
import java.io.IOException;


import java.util.StringTokenizer;
import java.util.Arrays;

public class Extraction {
	private String fichier_postes= "todaesData/G1bis/operateur.csv";
	private String fichier_taches= "todaesData/G1bis/operation.csv";
	private String fichier_dureeArc= "todaesData/G1bis/Duree_arc.csv";
	public void LireTodaesOperationFichier(){ // Pour extraire les op�ration du fichier
		try {
			CsvReader fichier = new CsvReader(fichier_taches);
			fichier.readHeaders();
			int taille_durees=fichier.getHeaderCount()-4; // 4 : libelle,place...
			double Durees[] = new double[taille_durees]; 
			TodaesOperation op = new TodaesOperation();
			// pour compter le nombre d'operation
			int nombre_operation=0;
			while (fichier.readRecord())
			{
				if(fichier.get("Libelle").equals("--"))
					break;
				nombre_operation++;
			}
			fichier = new CsvReader(fichier_taches);
			fichier.readHeaders();
			TodaesMain.todaesOperations = new TodaesOperation[nombre_operation];
			int indice=0; // l'indice du tableau op�rations
			while (fichier.readRecord())
			{
				if(fichier.get("Libelle").equals("--"))
					break;
				
				op.setLibelle(fichier.get("Libelle"));
				op.setPlace(Integer.parseInt(fichier.get("Place")));
				op.setDebut(Integer.parseInt(fichier.get("Debut")));
				op.setFin(Integer.parseInt(fichier.get("Fin")));
				for(int i=0;i<taille_durees;i++)
					Durees[i]=Double.parseDouble(fichier.get("Duree"+i));
				op.setDurees(Durees);
				TodaesMain.todaesOperations[indice]=new TodaesOperation(op);
				indice++;
				
			}
			LireArcFichier(nombre_operation,TodaesMain.todaesOperations,fichier);
			fichier.close();


			
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	public void LirePostesFichier(){  
		try {
			CsvReader fichier = new CsvReader(fichier_postes);
			fichier.readHeaders();
			double Freq[] = new double[4]; 
			Operateur op = new Operateur();
			//double[] Frequences = {0.25,0.5,0.75,1};
			//op.setFrequences(Frequences);
			TodaesMain.operateurs = new Operateur[TodaesMain.todaesOperations[0].getDurees().length]; 
			int indice=0; // l'indice du tableau op�rations
			while (fichier.readRecord())
			{
				if(fichier.get("Libelle").equals("--"))
					break;
				
				op.setLibelle(fichier.get("Libelle")); 
				for(int i=0;i<4;i++)
					Freq[i]=Double.parseDouble(fichier.get("Freq"+i));  
				op.setFrequences(Freq);
				
				TodaesMain.operateurs[indice]=new Operateur(op);
				indice++;
			}
			assert(indice == TodaesMain.operateurs.length);
			//LireLienFichier(TodaesMain.operateurs.length,TodaesMain.operateurs,fichier);
			fichier.close();	
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void LireArcFichier(int nb_operation, TodaesOperation[] operations,CsvReader fichier) throws IOException{ 
		fichier.readRecord(); 
		for(int i=0;i<nb_operation && fichier.readRecord();i++)
			ChoixOperation(nb_operation, i,fichier.getRawRecord());
	}

	/*public void LireLienFichier(int nb_operateur, Operateur[] operateurs,CsvReader fichier) throws IOException{ 
		fichier.readRecord(); 

		for(int i=0;i<nb_operateur && fichier.readRecord();i++)
			ChoixOperateur(nb_operateur, i,fichier.getRawRecord());
	}*/
	
	public void LireDureesArc(){ 
		try {
			CsvReader fichier = new CsvReader(fichier_dureeArc);
			fichier.readHeaders();
			
			double Durees[] = new double[TodaesMain.liens.size()];  
			//TodaesMain.operateurs = new Operateur[TodaesMain.todaesOperations[0].getDurees().length]; 
			int indice=0; 
			while (fichier.readRecord())
			{
				if(fichier.get("Duree0").equals("--"))
					break;
				for(int i=0;i<TodaesMain.liens.size();i++){ // TodaesMain.liens.size() doit etre egal au nombre de durees
					Durees[i]=Double.parseDouble(fichier.get("Duree"+i));  // Extraire les durees des arcs
				}
				TodaesMain.Arcs.get(indice).setDurees(Durees);
				indice++;
				//System.out.println("Arc entre : "+TodaesMain.Arcs.get(indice).getOpSource().getLibelle()+" et "+TodaesMain.Arcs.get(indice).getOpDestination().getLibelle()+" Duree 3 : "+TodaesMain.Arcs.get(indice).getDurees()[2]);
			}
			fichier.close();	
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (Exception e){
			System.out.println(" Nombre de durees est-il egal au nombre de liens ? ");
			e.printStackTrace();
		}	
	}
      	public void ChoixOperation(int nb_operation, int positionOp,String record){ 
		try{
			String choix="";

			StringTokenizer token= new StringTokenizer(record,","); 
			Arc arc_test;//1,2

			while(token.hasMoreTokens()){
				choix=new String(token.nextToken());
				if(choix.equals("null")){
					arc_test=new Arc(TodaesMain.todaesOperations[positionOp],new TodaesOperation("null"));
					break;
				}
				else{
					arc_test=new Arc(TodaesMain.todaesOperations[positionOp],TodaesMain.todaesOperations[Integer.parseInt(choix)]);
				}
				TodaesMain.Arcs.add(arc_test); 
			}
		}
		catch(Exception e){
			System.out.println("Exception dans la fonction ChoixOperation : "+e.getMessage());
		}

	}


	/*public void ChoixOperateur(int nb_operateur, int positionOp,String record){ 
		try{
			String choix="";

			StringTokenizer token= new StringTokenizer(record,","); 
			Lien lien_test;//1,2

			while(token.hasMoreTokens()){
				choix=new String(token.nextToken());
				if(choix.equals("null")){
					lien_test=null;
					break;
				}
				else{
					assert(positionOp != Integer.parseInt(choix) ); // un lien lie forcement 2 operateurs differents ! 
					assert(Integer.parseInt(choix) < TodaesMain.operateurs.length);
					lien_test=new Lien(positionOp,Integer.parseInt(choix));
				}

				if (!TodaesMain.liens.contains(lien_test)) // tous les liens se repetent 2 fois dans le fichier qui les decrit
					TodaesMain.liens.add(lien_test); 
			}
		}
		catch(Exception e){
			System.out.println("Exception dans la fonction ChoixOperateur : "+e.getMessage());
		}
		System.out.println("TodaesMain.liens.size() "+ TodaesMain.liens.size());

	}
	*/



	public static void LireMILPfichierSaufDureeArcs(String filename) throws  java.io.IOException,
	                                   InputDataReader.InputDataReaderException{
  		double[] plambdas; // failure rates per proc
		double[] llambdas; // failure rates per link

	      	double[] pfreqs; // freq set
	      	double[][] texes; // tasks exec per proc
		double[][] tcoms; // com times per data dep

	      	int[][] succs; // data dep
	      


		  
	      	int ntasks;
	      	int nprocs;
	      	int nfreqs;
	      	int nlinks;
	      	int nch; 
	      	int rep;
 
	      
	      	Double Po; // Power obj
	      	Double lo; // GSFR  obj
	      	Double Wo; // Period obj (in case of pipelined executions) : computed by program, not an input
	     	Double Max;  // some upper bound on the objective 
	      	int limit;
		InputDataReader reader = new InputDataReader(filename);
	        plambdas = reader.readDoubleArray();
		llambdas = reader.readDoubleArray(); // NOT used in the pipelines optimization problem 
	        pfreqs = reader.readDoubleArray();
	        texes = reader.readDoubleArrayArray();
	        rep  = reader.readInt();
		tcoms = reader.readDoubleArrayArray();

	        succs = new int[tcoms.length][tcoms.length];
			 for (int i=0; i < tcoms.length; i++) {
					for (int j=0; j <  tcoms.length; j++) {
						succs[i][j]= (tcoms[i][j] != 0) ? 1 :  0;
					}
				
			 } 
	         
	         //ntasks = texes.length / plambdas.length;
		ntasks = texes.length;
	        //nprocs = plambdas.length;
		nprocs = texes[0].length;
	        nfreqs = pfreqs.length;
	         
			
	        Po = reader.readDouble();
		lo = reader.readDouble();
		Wo = -1.0;
	        limit = reader.readInt(); 

	        Max = -1.0;
	      
					 
		// Adaptative scheduling problem (energy, throughput, nb procs) => pas de replications de tâches => rep = 1;
		assert(rep == 1);

		//####################### OPERATIONS ##########################
	      { double Durees[] = new double[nprocs]; 
		TodaesOperation op = new TodaesOperation();
		
		TodaesMain.todaesOperations = new TodaesOperation[ntasks];
		int indice=0; 
		while (indice< ntasks)
		{
			op.setLibelle("T"+indice);
			op.setPlace(0);
			op.setDebut(0);
			op.setFin(0);
			for(int i=0;i<nprocs;i++)
				Durees[i]=texes[indice][i];
			op.setDurees(Durees);
			TodaesMain.todaesOperations[indice]=new TodaesOperation(op);
			indice++;
		}
             }
		
		//###################### ARCS ############################
		 for (int i=0; i < tcoms.length; i++) {
			for (int j=0; j <  tcoms.length; j++) {
				if (tcoms[i][j] != 0) {
					TodaesMain.Arcs.add(new Arc(TodaesMain.todaesOperations[i],TodaesMain.todaesOperations[j])); 
				}
			}
		} 

		//###################### PROCS ############################
		Operateur op = new Operateur();
		TodaesMain.operateurs = new Operateur[nprocs]; 
		int indice=0; 
		System.out.println("nprocs " + nprocs);
		while (indice < nprocs)
		{
			op.setLibelle("P"+indice); 
			op.setFrequences(pfreqs);
			TodaesMain.operateurs[indice]=new Operateur(op);
			indice++;
		}
		assert(indice == TodaesMain.operateurs.length);
	}

	public static void LireMILPfichierDureeArcs(String filename) throws java.io.IOException,
	                                   InputDataReader.InputDataReaderException{
		InputDataReader reader = new InputDataReader(filename);
	        reader.readDoubleArray();
		reader.readDoubleArray(); 
	        reader.readDoubleArray();
	        reader.readDoubleArrayArray();
	        reader.readInt();
		double[][] tcoms = reader.readDoubleArrayArray();
		int indice=0;
		assert(TodaesMain.liens.size() > 0 );
		double Durees[] = new double[TodaesMain.liens.size()];  
		for (int j=0; j<TodaesMain.Arcs.size();j++){ 
			for(int i=0;i<TodaesMain.liens.size();i++){ 
				Durees[i]=tcoms[Arrays.asList(TodaesMain.todaesOperations).indexOf(TodaesMain.Arcs.get(j).getOpSource())][Arrays.asList(TodaesMain.todaesOperations).indexOf(TodaesMain.Arcs.get(j).getOpDestination())];  
			}
			TodaesMain.Arcs.get(j).setDurees(Durees);
		}
	}
}
