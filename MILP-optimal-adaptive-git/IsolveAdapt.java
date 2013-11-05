import ilog.concert.*;
import ilog.cplex.*;
import java.io.*;
import java.util.*;

public class IsolveAdapt {
	static private class Data {
	     
	      
	      double[] plambdas; // failure rates per proc
		  double[] llambdas; // failure rates per link

	      double[] pfreqs; // freq set
	      double[][] texes; // tasks exec per proc
		  double[][] tcoms; // com times per data dep

	      int[][] succs; // data dep
	      
		  Vector < Vector < Vector < Vector <Integer> > >  > CH; // chemins de coms CH i j w n : identifier of link number n composing path number w 
		                                                        //                               between proc number i and proc number j 
		  
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
	   //   int PI, PO; // // Processor in charge of reading input/ouptut from shared memory (1<= PI <= 4)
	      
	      
	      Data(String filename) throws IloException, java.io.IOException,
	                                   InputDataReader.InputDataReaderException
	      {
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

	         nprocs = plambdas.length;
	         nfreqs = pfreqs.length;
	         
			
	         Po = reader.readDouble();
			 lo = reader.readDouble();
			 Wo = -1.0;
	         limit = reader.readInt(); 
			// PI =  reader.readInt(); // Processor in charge of reading input from shared memory (1<= PI <= 4)
			// PO = reader.readInt(); //  Processor in charge of writing output to shared memory (1<= PO <= 4)
			// assert(PI <= 4 && PI >= 1 && PI <= 4 && PI >= 1);
	         Max = -1.0;
	        /* for (int j = 0; j < texes.length; j++)
				for (int k = 0; k < texes[j].length; k++)
		           Max = Max + rep*texes[j][k]/pfreqs[0];
			   for (int i = 0; i < tcoms.length; i++)
				 for (int j = 0; j < tcoms[i].length; j++)
					Max = Max + rep*rep*nlinks*tcoms[i][j];
			   Max = Math.max(Max, 1);
			 */
					 
			 // Adaptative scheduling problem (energy, throughput, nb procs) => pas de replications de tâches => rep = 1;
			 assert(rep == 1);

	      }
		  
		  public void  initialize_chemins(ArrayList<Chemin.Lien> arch_liens, int nprocs){
				assert(CH == null);
				CH = new Vector();
				for (int i = 0; i < nprocs ; i++){
				   CH.add(new Vector());
					for  (int j = 0; j < nprocs; j++){
						//System.out.println("chemins 0 --> 3");
						CH.elementAt(i).add(new Vector());
						if (i != j){ // pas de chemins a stoker
							ArrayList<Chemin> chemins = Chemin.Chemins(i,j, arch_liens);
							for (int w = 0; w < chemins.size(); w++){
								CH.elementAt(i).elementAt(j).add(new Vector());
								for (int n = 0; n < chemins.get(w).getChemin().size(); n++){
									CH.elementAt(i).elementAt(j).elementAt(w).add(chemins.get(w).getChemin().get(n).getIdentifiant());
								}
							}
						}
					}
				}
					
				for (int i = 0; i < nprocs ; i++){
					assert (CH.elementAt(i).elementAt(i).isEmpty());
					for (int j = 0; j < nprocs ; j++){
						assert(CH.elementAt(i).elementAt(j).size() == CH.elementAt(j).elementAt(i).size());
					}
				}		 

		  }
		  
		  
		    
		  public int max_nb_chemins(){
				assert(CH != null);
				assert(!CH.isEmpty());
				int max =0;
				for (int i = 0; i < CH.size() ; i++){
					for  (int j = 0; j < CH.elementAt(i).size(); j++){
						max = Math.max(max, CH.elementAt(i).elementAt(j).size());
					}
				}
				return max;
		  }
		  
		  public void afficher_chemins(){
				assert(CH != null);
				assert(!CH.isEmpty());
				
				System.out.println("Affichage chemins ");

				for (int i = 0; i < CH.size() ; i++){
					for  (int j = 0; j < CH.elementAt(i).size(); j++){
						System.out.println("chemins " + i + " --> " + j);
						for (int w = 0; w < CH.elementAt(i).elementAt(j).size(); w++){
							System.out.println("	chemin " + w );
							for (int n = 0; n < CH.elementAt(i).elementAt(j).elementAt(w).size(); n++){
								System.out.println("	Lien " + n +" is "+  CH.elementAt(i).elementAt(j).elementAt(w).elementAt(n));
							}
						}
					}
				}
		  }
		  
		  //XXXX
			//CH[l][l'][w] empty if  l = l'
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
	   public static void main (String args[]) {
	      try {
	         String filename;
			 String graphenumber;
			 String archpattern;
	         if ( args.length >= 3)  { filename = args[0]; 
			                           graphenumber = args[1];
									   archpattern = args[2]; 
									   System.out.println("arch pattern = "+archpattern);
									   assert(archpattern.equals("carre") || archpattern.equals("ligne"));
			}
	         else  {  
									System.out.println("Usage example : java -ea IsolveAdapt ../data/FMDEP/tasks8-4freq-ok-G1.txt 1 carre ....");
	        	                    filename = "data/tasks5.txt";
									graphenumber = "0";
									archpattern = "ligne";
			}

	         Data     data  = new Data(filename);
	        
			
			Boolean pipelinedexec = true;
	         String based;
			if (archpattern.equals("carre"))
				based = "/home/popart/assayad/Desktop/ilp-results-adapt-optimal/"+data.ntasks+"op-tuilescarre";
			else if (archpattern.equals("ligne"))
				based = "/home/popart/assayad/Desktop/ilp-results-adapt-optimal/"+data.ntasks+"op";
			else{ based=""; assert(false);}

			 //String based = "/home/ismail/Desktop/tsh-results-sttt-optimal-5op-depardon/";
	       
	         File out_channel1 = create_file (based+"/"+"G"+graphenumber+"nbproc_rep"+data.rep+"_nbop"+data.ntasks+".txt");
	         File out_channel2 = create_file (based+"/"+"G"+graphenumber+"power_rep"+data.rep+"_nbop"+data.ntasks+".txt");
	         File out_channel3 = create_file (based+"/"+"G"+graphenumber+"period_rep"+data.rep+"_nbop"+data.ntasks+".txt");
	         File out_channel4 = create_file (based+"/"+"G"+graphenumber+"nbproc_obj"+data.rep+"_nbop"+data.ntasks+".txt");
	         File out_channel5 = create_file (based+"/"+"G"+graphenumber+"period_obj"+data.rep+"_nbop"+data.ntasks+".txt");
	         File out_channel6 = create_file (based+"/"+"G"+graphenumber+"status"+data.rep+"_nbop"+data.ntasks+".txt");
	         File out_channel7 = create_file (based+"/"+"G"+graphenumber+"time"+data.rep+"_nbop"+data.ntasks+".txt");

	         
			double[] bornesup_previousline = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
			assert(data.Wo == -1);
			assert(data.Max == -1);

	     //  for (int l2 = 15; l2 <= 20; l2++) { // 14 values  4e-6 .... 0.4 0.8
		//	for (int l2 = 15; l2 >= 14; l2--) { // 14 values  8e-6 4e-6 8e-7 4e-7
			//for (int l1 = 2; l1 >= 1; l1--) { 
			  int nbomax = data.ntasks;// more processors than this number are useless	
			  nbomax = nbomax - 2; // just to speedup simulation end Or : number of tasks except input and output ones 
			  nbomax = 5;
			  int nboinit = 1;
			//int nboinit = 4;
			   for (int nbpo = nboinit; nbpo <= nbomax; nbpo++) { // number of processors objective
				  double borneinf_thisline = 0;
				  data.nprocs = nbpo;	
				  data.CH=null;
				  ArrayList<Chemin.Lien> arch_liens = Chemin.initialiser_archi_liens_noc(data.nprocs,archpattern); // links
				  data.nlinks = arch_liens.size();
				  //assert(nlinks == data.llambdas.length); // SAME lambdas for all links otherwise we have to modify input file to precise which link has wich lambda
			 
				  data.initialize_chemins(arch_liens, data.nprocs); // intialiser the vector CH
			                                 // TODO : for a noc we have to determine the set arch_liens links diffently from :
				 							 //                                            1- the number of proc and the pattern of the target sub-arch to pick from the NOC
				  							//                                            2- the width and height of an iland of the NOC
											 //       Once arch_liens determined normally we don not need to modifiy initialize_chemins but may be the code using CH.elementAt().elementAt().size() if it was supposed
											 //       not varying over pairs of different processors in ILP prgogram; is it ?
			      // afficher_chemins();
				  assert (data.nprocs >= 1);
				
				
				 data.Max = compute_wsup_pipeline(data);
			     assert(data.Max > 0);
			     if (nbpo == nboinit)	for (int i=0; i<bornesup_previousline.length; i++)  bornesup_previousline[i] = data.Max; 
				//double bornesup_thisline = data.Max;

				// verifions que ce nombre est toujours le meme en faisant varier les procs extremites des chemins
				if (!archpattern.equals("carre")) {
					for (int l=0; l<  data.nprocs ; l++)
						for (int ll=0; ll<  data.nprocs  ; ll++)
							if (l!= ll) assert(data.CH.elementAt(l).elementAt(ll).size() == data.CH.elementAt(0).elementAt(1).size()); 
				}
			 
				if (!archpattern.equals("carre")) {
					if (data.nprocs >= 2) data.nch = data.CH.elementAt(0).elementAt(1).size();	// verifier que ce nombre est toujours le meme en faisant varier les extremites des chemins i et j
					else { 
						assert(data.CH.size()==1); data.nch = 0;
					}
					assert(data.nch == data.max_nb_chemins());
				}
				
				data.nch = data.max_nb_chemins();
				if (data.nprocs < 2) assert(data.nch == 0);
				 
	             // for (int p2 = 5; p2 <= 20; p2++){ // 16 values for Pobj 1 1.2 ....4
				// for (int wo = 14; wo > 0; wo--){ // wmax = 50%wsup; 45%wsup, 40%wsup, 35%wsup, 30%wsup, ..., 5%wsup
				   for (int wo = 14; wo > 0; wo--){ // wmax = 50%wsup; 45%wsup, 40%wsup, 35%wsup, 30%wsup, ..., 5%wsup

					
				//	data.lo = l1 * 4 * Math.pow(10,-(21-l2)) ;
	 	         // data.Po =  p2 * 0.2;
				//	data.Wo = wo*0.05*compute_wsup_pipeline(data);
					data.Wo = wo*0.025*compute_wsup_pipeline(data); // 35%w 32,5%w 30%w 27.5%w 25%w 22,5%w 20%w 17,5%w 15%w ... 2,5%w
			     	// data.Wo = wo*0.015*compute_wsup_pipeline(data); // 21%w 19,5%w 18%w  ... 1,5%w
					if (data.Wo < compute_winf_pipeline(data,nbpo,arch_liens.size()))  {
						System.out.println(" No solution  : Wo < Winf");
						write_file(out_channel1, "NaN ");
						write_file(out_channel2, "NaN ");
						write_file(out_channel3, "Nan ");
						write_file(out_channel4, data.nprocs+" ");
						write_file(out_channel5, data.Wo+" ");
						write_file(out_channel6, "NaN "+" ");
						write_file(out_channel7, "NaN" +" ");
						continue;
					}
					
					System.out.println("Proc obj = "+data.nprocs);
					System.out.println("Period obj = "+data.Wo);
					System.out.println("--->DECLARATIONS");
					IloCplex cplex = new IloCplex(); 
					cplex.setParam( IloCplex.IntParam.MIPEmphasis, 2); //http://pic.dhe.ibm.com/infocenter/cosinfoc/v12r2/index.jsp?topic=%2Filog.odms.cplex.help%2FContent%2FOptimization%2FDocumentation%2FCPLEX%2F_pubskel%2FCPLEX998.html
					cplex.setParam( IloCplex.IntParam.ParallelMode, -1); //opportunistic http://pic.dhe.ibm.com/infocenter/cosinfoc/v12r2/topic/ilog.odms.cplex.help/Content/Optimization/Documentation/CPLEX/_pubskel/CPLEX1022.html
					cplex.setParam( IloCplex.IntParam.Threads, 0);  //http://pic.dhe.ibm.com/infocenter/cosinfoc/v12r2/index.jsp?topic=%2Filog.odms.cplex.help%2FContent%2FOptimization%2FDocumentation%2FCPLEX%2F_pubskel%2FCPLEX1022.html
			 

	         // Create start variables
	         IloNumVar[][] t = new IloNumVar[data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
	            t[j] = cplex.numVarArray(data.rep, 0.0, data.Max);
	         
			 // x3[j][k][l] => l means processor number (l+1)
	         IloIntVar[][][] x3 = new IloIntVar[data.ntasks][data.rep][data.nprocs];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		 	            x3[j][k] = cplex.boolVarArray(data.nprocs);

	         IloIntVar[][][][] x4 = new IloIntVar[data.ntasks][data.rep][data.nprocs][data.nfreqs];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.nprocs; l++)
		        			 x4[j][k][l] = cplex.boolVarArray(data.nfreqs);
            
			// unlike l, processors number starts with 1
			// P[j][k] is zero makes sens if replica k does not exists
	         IloIntVar[][] P = new IloIntVar[data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
	            P[j] = cplex.intVarArray(data.rep, 0, data.nprocs);
	         
	         IloIntVar[][][][] g = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.ntasks; l++)
		        			 g[j][k][l] = cplex.boolVarArray(data.rep);
	         
			// v
	         IloIntVar[][][][] v = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.ntasks; l++)
		        			 v[j][k][l] = cplex.boolVarArray(data.rep);
	         
	         //X j K a b
	         IloIntVar[][][][] X = new IloIntVar[data.ntasks][data.rep+1][(int)Math.pow(data.nprocs, data.rep)][(int)Math.pow(data.nfreqs, data.rep)];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int K = 0; K <= data.rep; K++)
		        	 for (int a = 0; a < (int)Math.pow(data.nprocs, data.rep); a++)
			           //for (int b = 0; b < (int)Math.pow(data.nfreqs, data.rep); b++)
		        			 X[j][K][a] = cplex.boolVarArray((int)Math.pow(data.nfreqs, data.rep));
	         
	         // unlike k,  K starts with 0
	         // Y j K means K-1 replication of original task j
	         IloIntVar[][] Y = new IloIntVar[data.ntasks][data.rep+1];
	         for (int j = 0; j < data.ntasks; j++){
		        			 Y[j] = cplex.boolVarArray(data.rep+1);
			}
	         
	         
	         
	         //XY j K a b
	         IloIntVar[][][][] XY = new IloIntVar[data.ntasks][data.rep+1][(int)Math.pow(data.nprocs, data.rep)][(int)Math.pow(data.nfreqs, data.rep)];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int K = 0; K <= data.rep; K++)
		        	 for (int a = 0; a < (int)Math.pow(data.nprocs, data.rep); a++)
			          // for (int b = 0; b < (int)Math.pow(data.nfreqs, data.rep); b++)
		        			 XY[j][K][a] = cplex.boolVarArray((int)Math.pow(data.nfreqs, data.rep));
	         
	         //S1 a k
			 //a set of processors combinations and proc indices composing each combination
			 // voir CONDITION pour comment l'utiliser (le nombre des "a"  a lire)
             // en fait elle contient meme les combinaisons (a,0) (a,1) .... (a,rep)  mais
			 // stocke de telle sorte qu'un prefixe de l'ensemble des combinaison est aussi un ensemble
			 // de combinaisons (le nombre est plus petit) A CONDITION de LIRE le nombre EXACT des sous-combinaisons (le nombre des "a" a lire)
			 // Exemple pour les combin de K repli il faut prendre que les a allant de 0 a nbproc^K 
			 // (l'interet est de ne pas rajouter une dimension K au tabaleau avant a, l'inconvenient est : voir suite )
			 // => donc ne jamais utiliser S1.size pour iterer sur a  

	         int[][] S1 = new int[(int)Math.pow(data.nprocs, data.rep)][data.rep];
		       for (int a = 0; a < (int)Math.pow(data.nprocs, data.rep); a++){
		    	   	int tmpa = a;
			        for (int k = 0; k < data.rep; k++){
		        			// S1[a][k] = keme_bit_base_nprocs(a);
	        			S1[a][k] = tmpa%data.nprocs;
	        			tmpa = tmpa/data.nprocs;
			        }
		       }
	         
		     //S2 a k
			 //a set of frequencies combinations and freq indices cmposing each combination
			 // voir CONDITION pour comment l'utiliser (le nombre des "b"  a lire)
			 int[][] S2 = new int[(int)Math.pow(data.nfreqs, data.rep)][data.rep];
				   for (int b = 0; b < (int)Math.pow(data.nfreqs, data.rep); b++){
			    	   	int tmpb = b;
					    for (int k = 0; k < data.rep; k++){
				        	//S2[b][k] = keme_bit_base_nfreqs(a);
					    		S2[b][k] = tmpb%data.nfreqs;
			        			tmpb = tmpb/data.nfreqs;
					     }
				   }
	   
		
		
		//	try{ Thread.sleep(10000);} catch(InterruptedException e){};

	          // coms
			  //------------------------
			
		 

			// tc i k n j k'  between i k  and j k'
			IloNumVar[][][][][] tc = new IloNumVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
					for (int n = 0; n < data.nlinks; n++)
						for (int j = 0; j < data.ntasks; j++)
							for (int kk = 0; kk < data.rep; kk++){
								tc[i][k][n][j][kk] = cplex.numVar(0.0, data.Max);
							}
							
		
											
			
							
					
		
			 
			//XXXX
			// xc i k n j k'  between i k  and j k'
			IloIntVar[][][][][] xc = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
					for (int n = 0; n < data.nlinks; n++)
						for (int j = 0; j < data.ntasks; j++)
							for (int kk = 0; kk < data.rep; kk++){
								xc[i][k][n][j][kk] = cplex.boolVar();
							 //	if (data.succs[i][j] == 0) xc[i][k][n][j][kk] = 0; (done later ) 
							}
							
									
					

			
			
			
			//NOTE : il y a des ch en trop du a l'utilisation de nch au lieu du nombre exact
			//NOTE : faut-il les mettre a zero, ou est ce non important ? 
			//ch[i][k][j][kk][l][ll][w]
			IloIntVar[][][][][][][] ch = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.rep][data.nprocs][data.nprocs][data.nch];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
						for (int j = 0; j < data.ntasks; j++)
							for (int kk = 0; kk < data.rep; kk++)
								for (int l = 0; l < data.nprocs; l++)
									for (int ll = 0; ll < data.nprocs; ll++)
										ch[i][k][j][kk][l][ll] = cplex.boolVarArray(data.nch);
			
										
													 
			//gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk]
			IloIntVar[][][][][][][][][][]  gcc = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep][data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk] = cplex.boolVar();
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}	
			
             //-----------------------------				
	   
	         IloNumVar W = cplex.numVar(0.0, data.Max);
	         IloNumVar En = cplex.numVar(0.0, data.Max);
			 IloNumVar Enexe = cplex.numVar(0.0, data.Max);
	         IloNumVar Encom = cplex.numVar(0.0, data.Max);
	     
			 IloNumVar Sum = cplex.numVar(0.0, data.Max);
	
			System.out.println("<---DECLARATIONS");



	         for (int j = 0; j < data.ntasks; j++) {
	        	 for (int k = 0; k < data.rep; k++){
	        		 IloLinearNumExpr expr = cplex.linearNumExpr(); 
	        		 expr.addTerm(1,t[j][k]);
		        	 for (int l = 0; l < data.nprocs; l++){
			        	 for (int m = 0; m < data.nfreqs; m++){
			        		 expr.addTerm(+data.texes[j][l]/data.pfreqs[m],x4[j][k][l][m]);
			        	 }
		        	 }
		        	 expr.addTerm(-1,W);
				     //System.out.println("EXPR = "+expr);
		        	 cplex.addLe(expr,0);
	        	 }
	         }
			 // g[j][k][jj][kk] =1 si jj kk débute après j k
	         // t[jj][kk] - t[j][k] - M*g[j][k][jj][kk]>= sum_lm (Djl/fm)*x4[j][k][l][m] - M 
	         // for each tasks j
	         for (int j = 0; j < data.ntasks; j++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// j´ successor of jk	
	        	 	for (int jj = 0; jj < data.succs[j].length; jj++) {
							    // for each replica j´ḱ´ successor of jk
	        	 				for (int kk = 0; kk < data.rep; kk++){
	        	 				  if ((jj != j) || (kk != k)){
	        	 					IloLinearNumExpr expr = cplex.linearNumExpr(); 
	        	 					expr.addTerm(1,t[jj][kk]); 	 				
	        	 					expr.addTerm(-1,t[j][k]);
	        	 					for (int l = 0; l < data.nprocs; l++){
	        	 						for (int m = 0; m < data.nfreqs; m++){
	        	 							expr.addTerm(-data.texes[j][l]/data.pfreqs[m],x4[j][k][l][m]);
	        	 						}
	        	 					}

	        	 				//	if (data.succs[j][jj]!=0){ No : probleme si jjkk n'existe pas
	        	 				//		 assert(j != jj);
	        	 				//		 assert( data.succs[j][jj] == 1);
	    	        	 		//		cplex.addGe(expr,0);
	        	 				//	}else{	
									
	        	 						expr.addTerm(-data.Max,g[j][k][jj][kk]);
	    	        	 				cplex.addGe(expr,-data.Max);
										
	        	 				  //  }
	        	 				  }
	        	 			     }
	        	 			 
	        		    }	        		 
	        	   }
	           }
	         // v[jj][kk][j][k] :  P[j][k] -  P[jj][kk]  - (nprocs+1)*v[jj][kk][j][k] >= -nprocs
			//XXXX
			 //deactivate constraints if jk or jjkk does not exist : no need deduced form zeroing v when the former happens
	         // for each tasks j
	         for (int j = 0; j < data.ntasks; j++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int jj = 0; jj < data.succs[j].length; jj++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
        	 					if ((jj != j) || (kk != k)){
        	 						IloLinearNumExpr expr = cplex.linearNumExpr(); 
        	 						expr.addTerm(1, P[j][k]); 	 				
        	 						expr.addTerm(-1, P[jj][kk]);
        	 						expr.addTerm(-data.nprocs -1 , v[jj][kk][j][k]);
        	 					    //System.out.println("EXPR 3 = "+expr);
        	 						cplex.addGe(expr,-data.nprocs);
        	 					}
	        	 	    }
	        	     }
	               }
	           }
	       
			  
	         // g[j][k][jj][kk] + g[jj][kk][j][k] + v[j][k][jj][kk] + v[jj][kk][j][k] -M* sum_l x[j][k][l] -M* sum_l x[jj][kk][l] >= 1 - 2*M
	         // for each tasks j
	         for (int j = 0; j < data.ntasks; j++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int jj = 0; jj < data.succs[j].length; jj++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
        	 					if ((jj != j) || (kk != k)){
        	 						IloLinearNumExpr expr = cplex.linearNumExpr(); 
									// si j et jj sur meme proc et j --> jj dans DAG  alors j ---> jj dans l'execution puisqu'elles sont dans le meme etage, ie, g[j][k][jj][kk] = 1
									// si j et jj sur meme proc et jj --> j dans DAG  alors jj ---> j dans l'execution puisqu'elles sont dans le meme etage, ie,  g[jj][kk][j][k] = 1
									if (data.succs[j][jj] == 1 &&  data.succs[jj][j] == 1) assert(false);
									else if  (data.succs[j][jj] == 1) {
										expr.addTerm(1, g[j][k][jj][kk]); 	 				
										expr.addTerm(1, v[j][k][jj][kk]); 	 				
										expr.addTerm(1, v[jj][kk][j][k]);
									}
									else if (data.succs[jj][j] == 1){
										expr.addTerm(1, g[jj][kk][j][k]);
										expr.addTerm(1, v[j][k][jj][kk]); 	 				
										expr.addTerm(1, v[jj][kk][j][k]);
									}
									else {
										expr.addTerm(1, g[j][k][jj][kk]); 	 				
										expr.addTerm(1, g[jj][kk][j][k]);
										expr.addTerm(1, v[j][k][jj][kk]); 	 				
										expr.addTerm(1, v[jj][kk][j][k]);
									}
									for (int l = 0; l< data.nprocs; l++){
										expr.addTerm(-data.Max, x3[j][k][l]);
										expr.addTerm(-data.Max, x3[jj][kk][l]);
									}

        	 					    // System.out.println("EXPR 4 = "+expr);
        	 						cplex.addGe(expr,1 - 2*data.Max);
        	 					}
	        	 	    }
	        	     }
	               }
	           }
	          	 	
	         //g[j][k][jj][kk] + 2*M >= 1 + M*sum_l x[j][k][l] + M*sum_l x[jj][kk][l]  for all jjkk successor of jk
	        //XXXX
			 //deacivate constraints if i k or j kk does not exist
	         // for each tasks j
			 
		if (!pipelinedexec) { // in case of piplined executions no dependencide between stages in the same period
	         for (int j = 0; j < data.ntasks; j++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// j´ successor of jk	
	        	 	for (int jj = 0; jj < data.succs[j].length; jj++) {
	        	 			if (jj != j) {
		        			          // for each replica j´ḱ´ successor of jk
	        	 				for (int kk = 0; kk < data.rep; kk++){
	        	 					if (data.succs[j][jj]!=0){
	        	 						assert( data.succs[j][jj] == 1);
	        	 						IloLinearNumExpr expr = cplex.linearNumExpr(); 
	        	 						expr.addTerm(1, g[j][k][jj][kk]); 
										for (int l = 0; l< data.nprocs; l++){
											expr.addTerm(-data.Max, x3[j][k][l]);
											expr.addTerm(-data.Max, x3[jj][kk][l]);
										}	
		        					   //  System.out.println("EXPR 5 = "+expr);
	        	 						cplex.addGe(expr,1 - 2*data.Max);
									}
								}
							}	        		 
					}
				}
	         }
		 }	 	
	         //P[j][k]	 
	         // for each tasks j
		     for (int j = 0; j < data.ntasks; j++) {
		      	 // for each replica jk 
		         for (int k = 0; k < data.rep; k++){		
		      		     IloLinearNumExpr expr = cplex.linearNumExpr(); 
		       			 for (int l = 0; l < data.nprocs; l++){
		   	 						expr.addTerm(l+1, x3[j][k][l]);
		   	 			 }
		   	 			 expr.addTerm(-1, P[j][k]);
					     //System.out.println("EXPR 6 = "+expr);
		   	 			 cplex.addEq(expr,0);
		       	   }
	          }
		         
		     //suml x3[j][0][l]
		     // for each tasks j
		     for (int j = 0; j < data.ntasks; j++) {
		      	 // for each replica jk 
		      		     IloLinearNumExpr expr = cplex.linearNumExpr(); 
		       			 for (int l = 0; l < data.nprocs; l++){
		   	 						expr.addTerm(1, x3[j][0][l]);
		   	 			 }
					     //System.out.println("EXPR 7 = "+expr);
		   	 			 cplex.addEq(expr,1);
	          }
		     
			 //sum l x3[j][k][l] k>=1
		     // for each tasks j
		     for (int j = 0; j < data.ntasks; j++) {
		      	 // for each replica jk 
		         for (int k = 1; k < data.rep; k++){		
		      		     IloLinearNumExpr expr = cplex.linearNumExpr(); 
		       			 for (int l = 0; l < data.nprocs; l++){
		   	 						expr.addTerm(1, x3[j][k][l]);
		   	 			 }
					     //System.out.println("EXPR 8 = "+expr);
		   	 			 cplex.addRange(0,expr,1);
		       	   }
	          }
		     
			 // x3[j][k][l] -  sum_m x4[j][k][l][m]
		     // for each tasks j
		     for (int j = 0; j < data.ntasks; j++) {
		      	 // for each replica jk 
		         for (int k = 0; k < data.rep; k++){		
		       			 for (int l = 0; l < data.nprocs; l++){
		       				 IloLinearNumExpr expr = cplex.linearNumExpr(); 
		   	 				 expr.addTerm(1, x3[j][k][l]);
    					    // System.out.println("j k l= "+j+k+l);
		   	 				 for (int m = 0; m < data.nfreqs; m++){
					             expr.addTerm(-1,x4[j][k][l][m]);
	    					    //System.out.println("j k l m= "+j+k+l+m);

					         }
    					    // System.out.println("EXPR 9 = "+expr);
			   	 			 cplex.addEq(expr,0);
		   	 			 }
		       	   }
	          }
		     
			 // P[j][0]
		     // for each tasks j
		     for (int j = 0; j < data.ntasks; j++) {
		      		IloLinearNumExpr expr = cplex.linearNumExpr(); 
		   	 		expr.addTerm(1, P[j][0]);
				    // System.out.println("EXPR 10 = "+expr);
		   	 		cplex.addRange(1,expr,data.nprocs);  
	          }
		     
		     {
			
			 // Enexe
		    IloLinearNumExpr expr = cplex.linearNumExpr(); 
    		expr.addTerm(-1,Enexe);
		    for (int j = 0; j < data.ntasks; j++) {
				IloLinearNumExpr exprT = cplex.linearNumExpr(); 
			//	exprT.addTerm(-1,EnexeT[j]);
	        	for (int k = 0; k < data.rep; k++){
		        	 for (int l = 0; l < data.nprocs; l++){
			        	 for (int m = 0; m < data.nfreqs; m++){
			        		 expr.addTerm(+data.texes[j][l]*data.pfreqs[m]*data.pfreqs[m],x4[j][k][l][m]);
			  //      		 exprT.addTerm(+data.texes[data.nprocs*j+l]*data.pfreqs[m]*data.pfreqs[m],x4[j][k][l][m]);
			        	 }
		        	 }

	        	 }
			//	cplex.addEq(exprT,0);
	         }
		    // System.out.println("EXPR 11 = "+expr);
       	    cplex.addEq(expr,0);
		     }
			 
			    
			 //XXXXX
			 // Encom : comm energy
		  {
        		IloLinearNumExpr expr = cplex.linearNumExpr(); 
        		expr.addTerm(-1,Encom);
				for (int i = 0; i < data.ntasks; i++) {
					//IloLinearNumExpr exprT = cplex.linearNumExpr(); 
					//exprT.addTerm(-1,EncomT[i]);
					for (int j = 0; j < data.ntasks; j++) {
						if (data.succs[i][j] != 0) {
							for (int k = 0; k < data.rep; k++){
								for (int kk = 0; kk < data.rep; kk++){
									//for (int l= 0; l < data.nprocs; l++){
									//	for (int ll= 0; ll < data.nprocs; ll++){
									//		for (int w= 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++){
									//			for (int n= 0; n <data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++){
													for (int n= 0; n <data.nlinks; n++){
														expr.addTerm(+data.tcoms[i][j],xc[i][k][n][j][kk]);
														//exprT.addTerm(+data.tcoms[i][j],xc[i][k][n][j][kk]);
													}
										//	}
									//	}
									//}
								}
							}
						}
					}
					//cplex.addEq(exprT,0);
				}
    		    // System.out.println("EXPR 13  = "+expr);
    		     cplex.addEq(expr,0);
		     }


			// W >= end of coms (we have to add this constraint in case of piplined executions only)
			if (pipelinedexec)
			 {
				for (int n= 0; n <data.nlinks; n++){
					IloLinearNumExpr expr = cplex.linearNumExpr(); 
					for (int i = 0; i < data.ntasks; i++) {
						for (int j = 0; j < data.ntasks; j++) {
							if (data.succs[i][j] != 0) {
								for (int k = 0; k < data.rep; k++){
									for (int kk = 0; kk < data.rep; kk++){
										expr.addTerm(1,tc[i][k][n][j][kk]);
										expr.addTerm(+data.tcoms[i][j],xc[i][k][n][j][kk]);									
									}
								}
							}
						}
					}
					expr.addTerm(-1, W);
					cplex.addLe(expr,0);
				}
		     }

	    
		     {
			 // En/W
			IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
			expr1.addTerm(1,En);
			expr1.addTerm(-1,Enexe);
			expr1.addTerm(-1,Encom);
    		cplex.addEq(expr1, 0);
			
		//  IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
    	//	expr2.addTerm(1,En);
    	//	expr2.addTerm(-data.Po,W);
    	//	cplex.addLe(expr2, 0);
		
			// W must be <= wo (the period)
			IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
			expr2.addTerm(1,W);
			cplex.addLe(expr2, data.Wo);
			
		/*	Todo : 	W is bigger than any com time end (in addition to the fact that it is bigger than any exe time end : already done)
					W must be <= wo // in fact the period will be the period objective wo if we want to mimiize energy
					minimize En (instead of minimize W)
					it is useless to minimize En/minimal_Period because a corresponding sol (En, period_min, nproc) will always be dominated by a sol (En, period=wo, nproc) obtained by
					minimizing En/wo that is by minimizing En (since wo is a constant) // in other words the period will be the period objective wo if we want to mimiize energy
					*/
		     }
		     
    		{
			//Sum = sum_D(f) 
			IloLinearNumExpr expr = cplex.linearNumExpr(); 
    		expr.addTerm(-1,Sum);
			
		    for (int j = 0; j < data.ntasks; j++) {
			//	IloLinearNumExpr exprT = cplex.linearNumExpr(); 
			//	exprT.addTerm(-1,SumT[j]);
				for (int k = 0; k < data.rep; k++){
		        	 for (int l = 0; l < data.nprocs; l++){
			        	 for (int m = 0; m < data.nfreqs; m++){
			        		 expr.addTerm(+data.texes[j][l]/data.pfreqs[m],x4[j][k][l][m]);
			  //      		 exprT.addTerm(+data.texes[data.nprocs*j+l]/data.pfreqs[m],x4[j][k][l][m]);
			        	 }
		        	 }
	        	 }
			//	cplex.addEq(exprT,0);

	         }
		    // System.out.println("EXPR 13 = "+expr);
		    cplex.addEq(expr,0);
    		}
    		
			
			
     
		      		
			// XjKab means that j is replicated AT LEAST K times and has combinaisons a for processors and b for frequencies 
			//                                                           for THESE K replicas
    		//product lin with K terms
    		//XjKab <= x4jklm  for all 0<=k<K
    		//XjKab + K > sum_k x4jklm 0<=k<K
    		{        		
    		    for (int j = 0; j < data.ntasks; j++) {
    	        	for (int K = 1; K <= data.rep; K++){
    	        		for (int a=0; a < Math.pow(data.nprocs, K); a++){
        	        		for (int b=0; b < Math.pow(data.nfreqs, K); b++){
        	       		        IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
     	        			    expr2.addTerm(1,X[j][K][a][b]);
        	        			for (int k= 0; k < K; k++){
        	        				IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
            	        			expr1.addTerm(1,X[j][K][a][b]);
            	        			expr1.addTerm(-1,x4[j][k][S1[a][k]][S2[b][k]]);
            	       		        cplex.addLe(expr1,0);
            	       		        //System.out.println("EXPR 15_1 = "+expr1);
         	        			    expr2.addTerm(-1,x4[j][k][S1[a][k]][S2[b][k]]);
        	        			}
     	       		            cplex.addGe(expr2,-K+1);
     	       		            //System.out.println("EXPR 15_2 = "+expr2);
        	        		}
    	        		}
    	        	 }
    	         }
             }
    		
			// # replicas : c est important car c est la seule facn pour pouvoir ecrire une contrainte pour logR
    		//sum_K YjK = 1  1<=K<=Rep
    		//sum_K K*YjK = sum_kl x3jkl     1<=K<=Rep  0<=k<Rep 0<=l<nproc
			//formulas like XjKab doesn't work for YjK
    		{        		
    		    for (int j = 0; j < data.ntasks; j++) {
    		    	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
    		    	IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
    	        	for (int K = 1; K <= data.rep; K++){
    	        		expr1.addTerm(1,Y[j][K]);
    	        		expr2.addTerm(K,Y[j][K]);

    	        	}
    	        	cplex.addEq(expr1,1);
	       		   // System.out.println("EXPR 16_1 = "+expr1);
    	        	for (int k = 0; k < data.rep; k++){
    	        		for (int l = 0; l < data.nprocs; l++){
			        		 expr2.addTerm(-1, x3[j][k][l]);
    	        		}
    	        	}
    	        	cplex.addEq(expr2,0);
	       		   // System.out.println("EXPR 16_2 = "+expr2);
    		    }
    	   }
    		
    		
    		// YjK <= sum_l x3jkl  all  0<=k<K all 1<=K<=Rep
			// Yj0 = 0
    		{        		
    		    for (int j = 0; j < data.ntasks; j++) {
    	        	for (int K = 1; K <= data.rep; K++){
        	        	for (int k = 0; k < K; k++){
        	        		IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
        	        		expr1.addTerm(1,Y[j][K]);
        	        		for (int l = 0; l < data.nprocs; l++){
   			        		 	expr1.addTerm(-1, x3[j][k][l]);
        	        		}
            	        	cplex.addLe(expr1,0);
        	       		  //  System.out.println("EXPR 17 = "+expr);
        	        	}
    	        	}
					IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
					expr2.addTerm(1,Y[j][0]);
					cplex.addEq(expr2,0);
    		    }
    		}
			  // product lin with two terms
    		//XYjKab <= XjKab all j K a b
    		//XYjKab <= YjK all j K a b
    		//XYjKab +1 >= XjKab + YjK all j K a b
    		{        		
    		    for (int j = 0; j < data.ntasks; j++) {
    	        	for (int K = 1; K <= data.rep; K++){
    	        		for (int a=0; a < Math.pow(data.nprocs, K); a++){
        	        		for (int b=0; b < Math.pow(data.nfreqs, K); b++){
        	        				IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
            	        			expr1.addTerm(1,XY[j][K][a][b]);
            	        			expr1.addTerm(-1,X[j][K][a][b]);
            	       		        cplex.addLe(expr1,0);
            	       		     //    System.out.println("EXPR 14_1 = "+expr1);
            	       		        IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
         	        			    expr2.addTerm(1,XY[j][K][a][b]);
         	        			    expr2.addTerm(-1,Y[j][K]);
         	       		            cplex.addLe(expr2,0);
           	       		         //   System.out.println("EXPR 14_2 = "+expr2);
         	       		            IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
         	       		            expr3.addTerm(1,XY[j][K][a][b]);
         	       		            expr3.addTerm(-1,X[j][K][a][b]);
         	       		            expr3.addTerm(-1,Y[j][K]);
         	       		            cplex.addGe(expr3,-1);
           	       		        //    System.out.println("EXPR 14_3 = "+expr3);
        	        		}
    	        		}
    	        	 }
    	         }
    		     
             }


			
			//XXXX
			//correct reliability : proc lambdas plambda depends on used frequence fm
			
			 //XXXX
			 // v put to zero if 	 jk or jkk does not exist
//					v[i][k][j][kk] <= sum_l x3[i][k][l]
//					v[i][k][j][kk] <= sum_l x3[j][kk][l]
					
			for (int i = 0; i < data.ntasks; i++) {
				// for each replica jk 
				for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int j = 0; j < data.ntasks; j++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(1, v[i][k][j][kk]);
							for (int l = 0; l < data.nprocs; l++){
								expr.addTerm(-1, x3[i][k][l]);
							}
							//System.out.println("EXPR 3 = "+expr);
							cplex.addLe(expr,0);
	        	 	    }
	        	     }
	               }
			}
		   for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int j = 0; j < data.ntasks; j++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(1, v[i][k][j][kk]);
							for (int l = 0; l < data.nprocs; l++){
								expr.addTerm(-1, x3[j][kk][l]);
							}
							//System.out.println("EXPR 3 = "+expr);
							cplex.addLe(expr,0);
	        	 	    }
	        	     }
				}
			}

	
			 //XXXX
			  // g put to ero if  jk or jjkk does not exist
			//    g[i][k][j][kk] <= sum_l x3[i][k][l]
			//	g[i][k][j][kk] <= sum_l x3[j][kk][l]
		 for (int i = 0; i < data.ntasks; i++) {
				// for each replica jk 
				for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int j = 0; j < data.ntasks; j++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(1, g[i][k][j][kk]);
							for (int l = 0; l < data.nprocs; l++){
								expr.addTerm(-1, x3[i][k][l]);
							}
							//System.out.println("EXPR 3 = "+expr);
							cplex.addLe(expr,0);
	        	 	    }
	        	     }
	               }
			}
		   for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int j = 0; j < data.ntasks; j++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(1, g[i][k][j][kk]);
							for (int l = 0; l < data.nprocs; l++){
								expr.addTerm(-1, x3[j][kk][l]);
							}
							//System.out.println("EXPR 3 = "+expr);
							cplex.addLe(expr,0);
	        	 	    }
	        	     }
				}
			}

			//XXXX
			  // t i k   put to zero if i k does not exit
			  // t[i][k] <= M*sum_l x3[i][k][l]
			  for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(1, t[i][k]);
							for (int l = 0; l < data.nprocs; l++){
								expr.addTerm(-data.Max, x3[i][k][l]);
							}
							//System.out.println("EXPR 3 = "+expr);
							cplex.addLe(expr,0);
				}
			}


			//XXXX
			//v ik ik  =0 
			//g ik ik = 0
			for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
						IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
						IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
						expr1.addTerm(1, v[i][k][i][k]);
						expr2.addTerm(1, g[i][k][i][k]);							
						cplex.addEq(expr1,0);
						cplex.addEq(expr2,0);
				}
			}
			//coms
			//-----------------------------------------------------------------------------------------------------
			
			System.out.println("EXISTENCE and ZEROING");
		
			/////////////////////////////////////////////////
			// EXISTENCE and ZEROING
			/////////////////////////////////////////////////
			
		
			
			// Existence de ch : un ch doit exister entre deux operations si 
			//																	1) existantes 
			//																	2) dependantes 
			//																	3) pas  sur meme processeur (deduite de 4 : non !)
			//																	4) sans routage
			//                  sinon zeroing  ch (already done for each of these cases : copied to here) 
			// if j succ i  &&  l /= l' : sum_w ch[i][k][j][kk][l][l'][w] = (x3[i][k][l])(x3[j][kk][l']) - R[i][j]*x3[i][k][l]*x3[j][kk][l']    
		    //                                                                XXXXX : faut-il mettre une egalite ? oui au lieu de >= et une autre contrainte disant qu'il y a au plus 1 w
			//																  XXXXX : en fait vaut mieux lineaariser : la reecrire en 4 contraintes comme ça :
			// 1 	if j succ i  &&  l /= l' : sum_w ch[i][k][j][kk][l][l'][w] <= 1 -  R[i][j] 
			// 2    if j succ i  &&  l /= l' : sum_w ch[i][k][j][kk][l][l'][w] + 1  >= (x3[i][k][l]) + (x3[j][kk][l']) - R[i][j]
			// 3 ch[i][k][j][kk][l][l'][w] <= x3[i][k][l]
			// 4 ch[i][k][j][kk][l][l'][w] <= x3[j][kk][l']
			// 5 if j not succ i ch[i][k][j][kk][l][l'][w] = 0
			// 5 all l : ch[i][k][j][kk][l][l][w] = 0
			// 1 ch[i][k][j][kk][l][l'][w]  + R[i][j]  <= 1 
			//
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int l = 0; l < data.nprocs; l++) {				
								for (int ll = 0; ll < data.nprocs; ll++) {		
									if ( data.succs[i][j] !=0 && l != ll) {
										IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
										IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
										//expr1.addTerm(1,R[i][j]);
										//expr2.addTerm(1,R[i][j]);
										expr2.addTerm(-1,x3[i][k][l]);
										expr2.addTerm(-1,x3[j][kk][ll]);									
										for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
											IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
											IloLinearNumExpr expr4 = cplex.linearNumExpr(); 
											expr1.addTerm(1, ch[i][k][j][kk][l][ll][w]);	
											expr2.addTerm(1, ch[i][k][j][kk][l][ll][w]);
											expr3.addTerm(1, ch[i][k][j][kk][l][ll][w]);	
											expr4.addTerm(1, ch[i][k][j][kk][l][ll][w]);
											expr3.addTerm(-1, x3[i][k][l]);	
											expr4.addTerm(-1, x3[j][kk][ll]);
											cplex.addLe(expr3, 0);	
											cplex.addLe(expr4, 0);		
										}
										cplex.addLe(expr1,1);
										cplex.addGe(expr2,-1);
									}else{
										for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {				
											IloLinearNumExpr expr5 = cplex.linearNumExpr(); 
											expr5.addTerm(1, ch[i][k][j][kk][l][ll][w]);
											cplex.addEq(expr5,0);
										}
									}
								}
							}
						}
					}
				}
			}	
			
		

			// Existence de xc[i][k][n][j][k'] : si 
			//																	1) operations existantes (deduite de 5)
			//																	2) dependentes (deduite de 5)
			//																	3) pas sur meme proc (deduite de 5)
			//																	4) sans routage  (deduite de 5 : oui)
			//																	5) ch i k j k' l l' w existe  
			//																	6) n appartient a CH[l][l'][W] (remplace n directement dans la notation de xc. 
			//																									en faisant varier l et l' on couvre tout les n possibles )
			//					sinon zeroing xc
			//
			// for all n xc[i][k][CH[l][l'][w][n]][j][k'] = ch[i][k][j][k'][l][l'][w]
			// MODIF for all n xc[i][k][m][j][k'] =  (sum_l_l' / CH[l][l'][w][index] == m) ch[i][k][j][k'][l][l'][w]
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int m = 0; m < data.nlinks; m++) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, xc[i][k][m][j][kk]);								
								for (int l = 0; l < data.nprocs; l++) {				
									for (int ll = 0; ll < data.nprocs; ll++) {		
										assert(data.CH.elementAt(l).elementAt(l).isEmpty());
										for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
											if (data.CH.elementAt(l).elementAt(ll).elementAt(w).contains(m)){
												expr1.addTerm(-1, ch[i][k][j][kk][l][ll][w]);
											}
										}
									}
								}
								cplex.addEq(expr1, 0);	
							}	
						}
					}
				}
			}
			
		
			// Existence de tc (deduite de l'existence des xc here + precedence tasks coms later)
			//	tc[i][k][n][j][k'] <= M*xc[i][k][n][j][k']
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int n = 0; n < data.nlinks; n++) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, tc[i][k][n][j][kk]);	
								expr1.addTerm(-data.Max, xc[i][k][n][j][kk]);
								cplex.addLe(expr1, 0);		
							}
						}
					}
				}
			}

			
			System.out.println("PRECEDENCE TASKS");
	
			/////////////////////////////////////////////////
			// PRECEDENCE TASKS
			/////////////////////////////////////////////////									
		
		            
		
			
			System.out.println("EXCLUSION TASKS");
			/////////////////////////////////////////////////
			// EXCLUSION TASKS
			/////////////////////////////////////////////////
		


		
			System.out.println("PRECEDENCE COMS");
			/////////////////////////////////////////////////
			// PRECEDENCE COMS
			/////////////////////////////////////////////////

				
			//XXXX 
			//in addition to above, there are other situation ( ) where we must put to zero  start of coms  and mapping af coms xc xcbr xcar and start of routing and mapping o routing
			// (ex : when i j are on same proc !!! and according to value of R[i][j] and existance of the k-th or kk-th replicas)
			// all these situations deduced form value of   R[i][j] or below
			
			// precedence com link -> com link'
			// si j succ i : 
			//         for all k  kk  l  l'  w  n  with  l /= l'    n+1 < CH[l][l'][w].size
			//               tc[i][k][CH[l][l'][w][n+1]][j][kk] - M*ch[i][k][j][kk][l][l'][w]  >= tc[i][k][CH[l][l'][w][n]][j][kk] - M
			//               tcbr[i][k][CH[l][l'][w][n+1]][j]   - M*chbr[i][k][j][l][l'][w]  >= tcbr[i][k][CH[l][l'][w][n]][j]  - M 
			//               tcar[i][CH[l][l'][w][n+1]][j][k']  - M*chaar[i][j][k'][l][l'][w] >= tcar[i][CH[l][l'][w][n]][j][k']  - M 
		
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int l = 0; l < data.nprocs; l++) {				
								for (int ll = 0; ll < data.nprocs; ll++) {		
									if ( data.succs[i][j] !=0 && l != ll) {
										for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
											for (int n = 0; n+1 < data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++) {
												IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
												expr1.addTerm(1, tc[i][k][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n+1)][j][kk]);	
												expr1.addTerm(-data.Max, ch[i][k][j][kk][l][ll][w]);
												expr1.addTerm(-1, tc[i][k][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n)][j][kk]);	
												cplex.addGe(expr1, -data.Max);	
											}
										}
									}
								}
							}
						}
					}
				}
			}	
		
			
			//XXXXXXXXXX : prbleme : i lfaut s assurer que pour i =ii et j =jj et n != nn alors gcc peut etre  0 : oui car  la forumule dexcl mutuelle
		//							qui imposer une valeur de 1 aux g ne concerne que les cas n = nn (see later gcc + gcc >= 1)
			// XXXXX is i = ii j=jj n =nn gcc doit etre egale a zero ce qui n est pas autorise dans une formule plus loin => ajouter la condition de desactivation de ce cas dans cette formule plus loin
			//gccar[i][k][n][j][k'][ii][nn][jj][kk'] gccbr[i][k][n][j][k'][ii][kk][nn][jj] gcbrc[i][k][n][j][ii][kk][nn][jj][kk']
			//gcbrcbr[i][k][n][j][ii][kk][nn][jj] gcbrcar[i][k][n][j][ii][nn][jj][kk'] gcarc[i][n][j][k'][ii][kk][nn][jj][kk']
			//gcarcbr[i][n][j][k'][ii][kk][nn][jj] gcarcar[i][n][j][k'][ii][nn][jj][kk']
			
			//g-based precedence com link -> com' link'  : link com starts with gcc gccar gccbr gcbrc gcbrcbr gcbrcar gcarc gcarcbr gcarcar
		//1	//tc[ii][kk][nn][jj][kk'] - tc[i][k][n][j][k'] - M*gcc[i][k][n][j][k'][ii][kk][nn][jj][kk'] >=   (Dijn)*xc[i][k][n][j][k'] - M
		//2	//tcar[ii][nn][jj][kk'] - tc[i][k][n][j][k'] - M*gccar[i][k][n][j][k'][ii][nn][jj][kk'] >=   (Dijn)*xc[i][k][n][j][k'] - M
		//3	//tcbr[ii][kk][nn][jj] - tc[i][k][n][j][k'] - M*gccbr[i][k][n][j][k'][ii][kk][nn][jj] >=   (Dijn)*xc[i][k][n][j][k'] - M
		//4	//tc[ii][kk][nn][jj][kk'] - tcbr[i][k][n][j] - M*gcbrc[i][k][n][j][ii][kk][nn][jj][kk'] >=   (Dijn)*xcbr[i][k][n][j] - M
		//5	//tcbr[ii][kk][nn][jj] - tcbr[i][k][n][j] - M*gcbrcbr[i][k][n][j][ii][kk][nn][jj] >=   (Dijn)*xcbr[i][k][n][j] - M
		//6	//tcar[ii][nn][jj][kk'] - tcbr[i][k][n][j] - M*gcbrcar[i][k][n][j][ii][nn][jj][kk'] >=   (Dijn)*xcbr[i][k][n][j] - M
		//7	//tc[ii][kk][nn][jj][kk'] - tcar[i][n][j][k'] - M*gcarc[i][n][j][k'][ii][kk][nn][jj][kk'] >=   (Dijn)*xcar[i][n][j][k'] - M
		//8	//tcbr[ii][kk][nn][jj] - tcar[i][n][j][k'] - M*gcarcbr[i][n][j][k'][ii][kk][nn][jj] >=   (Dijn)*xcar[i][n][j][k'] - M
		//9	//tcar[ii][nn][jj][kk'] - tcar[i][n][j][k'] - M*gcarcar[i][n][j][k'][ii][nn][jj][kk'] >=   (Dijn)*xcar[i][n][j][k'] - M
		
		//XXXX : dois je enleve les multiplication par les *xc ... pour forcer les gcc a zero dans les cas ou xc = 0 (non pas la peine le zeroing deja fait ailleurs plus tard)

			//1
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tc[ii][kkk][nn][jj][kkkk]);	
													expr1.addTerm(-1, tc[i][k][n][j][kk]);
													expr1.addTerm(-data.Max, gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk] );
													expr1.addTerm(-data.tcoms[i][j], xc[i][k][n][j][kk] );	
													cplex.addGe(expr1, -data.Max);	
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}	
					
			System.out.println("--->  EXCLUSION COMS ...");

			/////////////////////////////////////////////////
			// EXCLUSION COMS
			/////////////////////////////////////////////////

            //XXXXXXXXXXX : probleme si ii = i e jj=j => la formule ci-dessous force les xc a zero 
			//deactiver les cas ii = i e jj =j et k =kkk et kk = kkkk
			//XXXX
			// excl between com ^ com'   com ^ combr   com ^ comar    comar ^ comar'     combr ^ combr'    combr ^ comar : link gcc gccar ... with vcc vccar ...
		    // no need for vc's since we can express mutual exclusion easily by using the link number (n) attached to gc's
			//                           deactivate constraints when ik or jk' or ii k or jjk' not exists AND WHEN ik=iikk and jkkk :jjkkkk
		//1	// gcc[i][k][n][j][k'][ii][kk][n][jj][kk'] + gcc[ii][kk][n][jj][kk'][i][k][n][j][k'] + 2*M  >=  1 + M*xc[i][k][n][j][k'] + M*xc[ii][kk][n][jj][kk']
		//2	// gccar[i][k][n][j][k'][ii][n][jj][kk'] + gcarc[ii][n][jj][kk'][i][k][n][j][k'] + 2*M  >=  1 + M*xc[i][k][n][j][k'] + M*xcar[ii][n][jj][kk']
		//3	// gccbr[i][k][n][j][k'][ii][kk][n][jj] + gcbrc[ii][kk][n][jj][i][k][n][j][k'] + 2*M  >=  1 + M*xc[i][k][n][j][k'] + M*xcbr[ii][kk][n][jj]
			// case gcbrc + gccbr  : idem case of gccbr+gcbrc above
		//4	// gcbrcbr[i][k][n][j][ii][kk][n][jj] + gcbrcbr[ii][kk][n][jj][i][k][n][j] + 2*M  >=  1 + M*xcbr[i][k][n][j] + M*xcbr[ii][kk][n][jj]
		//5	// gcbrcar[i][k][n][j][ii][n][jj][kk'] + gcarcbr[ii][n][jj][kk'][i][k][n][j] + 2*M  >=  1 + M*xcbr[i][k][n][j] + M*xcar[ii][n][jj][kk']
			// case gcarc + gccar  : idem case of gccar+gcarc above
			// case gcarcbr + gcbrcar : idem case of gcbrcar+gcarcbr above
		//6	// gcarcar[i][n][j][k'][ii][n][jj][kk'] + gcarcar[ii][n][jj][kk'][i][n][j][k'] + 2*M  >=  1 + M*xcar[i][n][j][k'] + M*xcar[ii][n][jj][kk']
			
		//1
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {				
				for (int k = 0; k < data.rep; k++) {
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											if (i != ii || j != jj || k !=  kkk || kk != kkkk){  
											for (int n = 0; n < data.nlinks; n++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcc[i][k][n][j][kk][ii][kkk][n][jj][kkkk] );	
													expr1.addTerm(1, gcc[ii][kkk][n][jj][kkkk][i][k][n][j][kk] );
													expr1.addTerm(-data.Max,  xc[i][k][n][j][kk] );
													expr1.addTerm(-data.Max,  xc[ii][kkk][n][jj][kkkk] );
													cplex.addGe(expr1, 1-2*data.Max);	
												}	
											}
										}
									}
								}
							}
						}
					}
					}
				}
				
						
			//XXXX
			// gcc[i][k][n][j][k'][ii][kk][n][jj][kk']  is  zero when xc[i][k][n][j][k']  or xc[ii][kk][nn][jj][kk'] is zero 
			// idem gccar ...
			// tc[i][k][n][j][k'] are zero when xc[i][k][n][j][k'] is zero 
			// idem tcar
			// idem tcbr
			// idem tr

		//1	//gcc[i][k][n][j][k'][ii][kk][nn][jj][kk'] <= xc[i][k][n][j][k']
		//2	//gcc[i][k][n][j][k'][ii][kk][nn][jj][kk'] <= xc[ii][kk][nn][jj][kk']
		//3	//gccar[i][k][n][j][k'][ii][nn][jj][kk'] <= xc[i][k][n][j][k']
		//4	//gccar[i][k][n][j][k'][ii][nn][jj][kk'] <= xcar[ii][nn][jj][kk']
		//5	//gccbr[i][k][n][j][k'][ii][kk][nn][jj] <= xc[i][k][n][j][k']
		//6	//gccbr[i][k][n][j][k'][ii][kk][nn][jj] <= xcbr[ii][kk][nn][jj]
		//7	//gcbrc[i][k][n][j][ii][kk][nn][jj][kk']  <= xcbr[i][k][n][j]
		//8	//gcbrc[i][k][n][j][ii][kk][nn][jj][kk']  <= xc[ii][kk][nn][jj][kk']
		//9	//gcbrcbr[i][k][n][j][ii][kk][nn][jj] <= xcbr[i][k][n][j]
		//10	//gcbrcbr[i][k][n][j][ii][kk][nn][jj] <= xcbr[ii][kk][nn][jj]
		//11	//gcbrcar[i][k][n][j][ii][nn][jj][kk'] <= xcbr[i][k][n][j]
		//12	//gcbrcar[i][k][n][j][ii][nn][jj][kk'] <= xcar[ii][nn][jj][kk']
		//13	//gcarc[i][n][j][k'][ii][kk][nn][jj][kk'] <= xcar[i][n][j][k']
		//14	//gcarc[i][n][j][k'][ii][kk][nn][jj][kk'] <= xc[ii][kk][nn][jj][kk']
		//15	//gcarcbr[i][n][j][k'][ii][kk][nn][jj] <= xcar[i][n][j][k']
		//16	//gcarcbr[i][n][j][k'][ii][kk][nn][jj] <= xcbr[ii][kk][nn][jj]
		//17	//gcarcar[i][n][j][k'][ii][nn][jj][kk'] <= xcar[i][n][j][k']
		//18	//gcarcar[i][n][j][k'][ii][nn][jj][kk'] <= xcar[ii][nn][jj][kk']


			//1 2
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk] );
													expr2.addTerm(1, gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk] );	
													expr1.addTerm(-1,  xc[i][k][n][j][kk] );
													expr2.addTerm(-1,  xc[ii][kkk][nn][jj][kkkk] );
													cplex.addLe(expr1, 0);	
													cplex.addLe(expr2, 0);
												}	
											}	
										}
									}
								 }
							}
						}
					}
				}
			}
		
			System.out.println("---> PRECEDENCE TASKS COMS ...");

			/////////////////////////////////////////////////
			// PRECEDENCE TASKS COMS
			/////////////////////////////////////////////////

			//XXXX
			//precedences  tache -> com -> tache  tache -> com -> routing  routing -> com -> tache
			// note: 
			//      remove the formula t[j][kk] - t[i][k] - M*g[i][k][j][kk]>= sum_lm (Dil/fm)*x4[i][k][l][m] - M  earlier in the code ?
			//		no because we need it to define g-based precedences ( force a precedence by putting g to 1 :  
			//                                                            a task to run after end of another task (used earlier in the definition of mutual exclusion)
			//		            however graph precedence can be removed since they are deduced from tasks -> com -> tasks ... precedences below
			// si j succ i : 
			//               we use v vtr and vrt to deactivate constraints in case nom coms are used because 
		    //               tasks are in same proc (no need because you can use only xc's instead) 
			// XXXXXXXXXXXXX 
			// deactivate constraints if tc does not exist i.e. if xc[i][k][n][j][kk] = 0  
			// if same proc then xc is zero then no need for using v's to deactivate donctraints in the same proc case since already deactivated by xc's 
		//1	//               tc[i][k][n][j][kk] - t[i][k]   - M*xc[i][k][n][j][kk] >= sum_lm (Dil/fm)*x4[i][k][l][m]  -M
		//2	//               t[j][kk] - tc[i][k][n][j][kk]   - M*xc[i][k][n][j][kk]  >=  (Dijn)*xc[i][k][n][j][kk]  -M
		//3	//               tcbr[i][k][n][j] - t[i][k] - M*xcbr[i][k][n][j]  >= sum_lm (Dil/fm)*x4[i][k][l][m] - M 
	  //4   //               tr[i][j] - tcbr[i][k][n][j] - M*xcbr[i][k][n][j]   >=  (Dijn)*xcbr[i][k][n][j] - M 
	 	//5	//               tcar[i][n][j][kk] - tr[i][j] - M*xcar[i][n][j][kk]   >=  - M 
		//6	//               t[j][kk] - tcar[i][n][j][kk] - M*xcar[i][n][j][kk]   >=  (Dijn)*xcar[i][n][j][kk] - M
            // si j not succ i : 
			//               put to zero mapping of com's (xc's done in existence part ) and routing tasks (xr done in existence part)  
			//               put to zero start of no routing coms (zeroing xc's done in existence part) routing coms (zeroing xcbr xcar done in exitence part)  
			//				
			// for each tasks i

		//1 // in case of pipelined executions : yes we have depndencies  op -> om
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int n = 0; n < data.nlinks; n++) {
							  if (data.succs[i][j] !=0) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, tc[i][k][n][j][kk]);
								expr1.addTerm(-1, t[i][k]);
								expr1.addTerm(-data.Max, xc[i][k][n][j][kk] );	
								for (int l = 0; l < data.nprocs; l++){
									for (int m = 0; m < data.nfreqs; m++){
										expr1.addTerm(-data.texes[i][l]/data.pfreqs[m],x4[i][k][l][m]);
									}
								}
								cplex.addGe(expr1, -data.Max);
							 }		
							}
						}
					}
				}
			}
			//2
	if (!pipelinedexec) { // in case of pipelined executions : no we do not have depndencies com -> op
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int n = 0; n < data.nlinks; n++) {
								if (data.succs[i][j] !=0) {
									IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
									expr1.addTerm(-1, tc[i][k][n][j][kk]);
									expr1.addTerm(1, t[j][kk]);
									expr1.addTerm(-data.Max, xc[i][k][n][j][kk] );	
									expr1.addTerm(-data.tcoms[i][j],xc[i][k][n][j][kk]);
									cplex.addGe(expr1, -data.Max);
								}	
							}
						}
					}
				}
			}	
		}
			
	
	
        		
        /*  {
			for (int j = 0; j < data.ntasks; j++) {
    		    	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
					expr1.addTerm(1,Y[j][1]);
        		    cplex.addEq(expr1,1);
				}
			} */
			
			
			{
    		    	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
					expr1.addTerm(1,En);
					//cplex.addGe(expr1,  borneInfL4fois10emoins5[p2-5]);
					cplex.addLe(expr1,  bornesup_previousline[wo-1]); // En <= En' if En' with less processors
			}
			{
    		     	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
					expr1.addTerm(1,En);
					cplex.addGe(expr1,  borneinf_thisline); // En >= En' if  En' for more period
			}    		
			
			
     		/*	if (pipelinedexec) {
					IloLinearNumExpr exprPI = cplex.linearNumExpr(); 
					IloLinearNumExpr exprPO = cplex.linearNumExpr(); 

					exprPI.addTerm(1, P[0][0]);
					exprPO.addTerm(1, P[data.ntasks-1][0]);

					cplex.addLe(exprPI, 4);
					cplex.addLe(exprPO, 4);
					cplex.addGe(exprPI, 1);
					cplex.addGe(exprPO, 1);
					
				}
			*/
			
			
			// Proc number constraints
	        // sum Z[i] <= np  ou Z[i] = 1 is proc i utilisé
			//                    cad 1 - Z[i] =  min_j(1, abs (P[j] - i))
			//   
			cplex.addMinimize(En);
			//System.out.println("cplex = "+cplex);
			System.out.println("limit = "+data.limit);

    		int limit = data.limit; 
			if (limit != 0)
				cplex.setParam(IloCplex.DoubleParam.TiLim, limit);
			else System.out.println("limit ignored = ");

		// cplex.exportModel("model.lp");
			
    		long start = System.currentTimeMillis();
			System.out.println("---> Solve ...");
    		Boolean sol = cplex.solve();
			System.out.println("<--- Solve ...");

    		long end = System.currentTimeMillis();
    		Double time = (end - start)/1000. ;
    		System.out.println(" Schedule tackled in " + time + " seconds using currentTimeMillis");
    		//Boolean sol = false;
	         if ( sol ) {
	             System.out.println(" Status  = " + cplex.getStatus());
				 System.out.println(" Time  = " + time);
				 borneinf_thisline =  cplex.getObjValue(); // attention au passage d'un Lobj vers un autre ce n'est plus valable ce bornsup
				 bornesup_previousline[wo-1] = cplex.getObjValue();
				 
	             System.out.println(" E  = " + cplex.getObjValue());
	            // Double lam = -cplex.getValue(logR)/(cplex.getValue(Sum) + cplex.getValue(Encom) + cplex.getValue(Encombr) + cplex.getValue(Encomar));
	            // System.out.println(" L  = " + lam);
	             //Double power = cplex.getValue(En)/cplex.getValue(W);
				 Double power = cplex.getValue(En)/data.Wo;
	             System.out.println(" P  = " + power);
	           //  Double reliability = Math.exp(cplex.getValue(logR));
	            // System.out.println(" R  = " + reliability);
	             System.out.println(" W  = " + cplex.getValue(W));
				 int nbpu = compute_nbpu(data, cplex, P);
				 write_file(out_channel1, nbpu+" "); // nb proc utilisés
				 write_file(out_channel2, power+" "); 
				// write_file(out_channel3, cplex.getValue(W)+" ");
				 write_file(out_channel3, cplex.getValue(W)+" "); // ???? il faudrait la periode plutot car c'est la sol qui donne le moins d'energie
				 write_file(out_channel4, data.nprocs+" ");
				 write_file(out_channel5, data.Wo+" ");
				 write_file(out_channel6, cplex.getStatus()+" ");
				 write_file(out_channel7, time+" ");
	         } else {
	        	  System.out.println(" No solution found ");
				write_file(out_channel1, "NaN ");
	        	write_file(out_channel2, "NaN ");
	        	write_file(out_channel3, "Nan ");
	            write_file(out_channel4, data.nprocs+" ");
	        	write_file(out_channel5, data.Wo+" ");
	        	write_file(out_channel6, "NaN "+" ");
	  	        write_file(out_channel7, time +" ");

	         }
		  ////////////////////////////////////////////////////////////////////
		  // DEBUG
		  //////////////////////////////////////////////////////////////
			boolean skip = false; 
			 if (sol&& !skip) {	 
			 	 for (int j = 0; j < data.ntasks; j++) {
					for (int K = 1; K <= data.rep; K++){
					
						if (cplex.getValue(Y[j][K]) > 0.5 ){
							System.out.println("---> j, K "+j+" "+ K);
							System.out.println("Y[j][K]: "+cplex.getValue(Y[j][K]));            	        		
						}
						for (int a=0; a < Math.pow(data.nprocs, K); a++){
							for (int b=0; b < Math.pow(data.nfreqs, K); b++){
									if (cplex.getValue(XY[j][K][a][b]) > 0.5 ){
										System.out.println("---> j, K, a, b "+j+" "+ K+ " "+a +" "+b);
										System.out.println("XY[j][K][a][b]: "+cplex.getValue(XY[j][K][a][b]));     
										//System.out.println(" ET ............ Y[j][K]: "+cplex.getValue(Y[j][K]));
									}
            		        			
									
            		        			
							}
						}
					}
				}
            	      
				for (int j = 0; j < data.ntasks; j++) {
					for (int k = 0; k < data.rep; k++){
						for (int l=0; l < data.nprocs; l++){
												
							for (int m=0; m < data.nfreqs; m++){
								if (cplex.getValue(x4[j][k][l][m]) > 0.5){
									System.out.println("---> j, k, l, m "+j+" "+ k+ " "+l +" "+m);
									System.out.println("x4[j][k][l][m]: "+cplex.getValue(x4[j][k][l][m]));
									System.out.println("ET ................. x3[j][k][l]: "+cplex.getValue(x3[j][k][l]));
								}
							}
						}
					}
				}
				
							
				
				
					//1 2
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													//expr1.addTerm(1, gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk] );
													//expr1.addTerm(-1,  xc[i][k][n][j][kk] );
													//expr2.addTerm(-1,  xc[ii][kkk][nn][jj][kkkk] );
													if (cplex.getValue(gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk]) > 0.5){
															System.out.println("---> gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk] i "+i+" k "+ k+ " n "+n +" j "+j+ " kk "+ kk + " ii "+ ii+ " kkk " + kkk + " nn " +nn +" jj " + jj + " kkkk "+ kkkk +" = " + +cplex.getValue(gcc[i][k][n][j][kk][ii][kkk][nn][jj][kkkk]) );
													}
												}	
											}	
										}
									}
								 }
							}
						}
					}
				}
			}
		
		//ch		
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int l = 0; l < data.nprocs; l++) {				
								for (int ll = 0; ll < data.nprocs; ll++) {	
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										if (cplex.getValue(ch[i][k][j][kk][l][ll][w]) > 0.5){
											System.out.println("---> ch[i][k][j][kk][l][ll][w] i "+i+ " k "+k +" j "+j+ " kk "+ kk + " l "+ l+ " ll " +ll +" w " + w +" = "  +cplex.getValue(ch[i][k][j][kk][l][ll][w]) );
										}
									}
								}
							}
						}
					}
				}
			}	
		
		//xc
		for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int m = 0; m < data.nlinks; m++) {
								if (cplex.getValue( xc[i][k][m][j][kk] ) > 0.5){
										System.out.println("--->  xc[i][k][m][j][kk] i "+i+ " k "+k +" m "+m+ " j "+ j+ " kk " + kk+" = "  +cplex.getValue(xc[i][k][m][j][kk]) );
								}
							}	
						}
					}
				}
			}

		
						
             } 	//sol && !skip      
		  ////////////////////////////////////////////////////////////////////
		  // END DEBUG
		  //////////////////////////////////////////////////////////////
	         cplex.end();
		   } // for wo
	              
			write_file(out_channel1, "\n");
			write_file(out_channel2, "\n");
			write_file(out_channel3, "\n");
			write_file(out_channel4, "\n");
			write_file(out_channel5, "\n");
			write_file(out_channel6, "\n");
			write_file(out_channel7, "\n");

		} // for nbpo
	//} // for l2
     
	}
	catch (IloException e) {
		System.out.println("Concert exception caught: " + e);
	}    
	catch (InputDataReader.InputDataReaderException ex) {
		System.out.println("Data Error: " + ex);
	}
	catch (java.io.IOException ex) {
		System.out.println("IO Error: " + ex);
	}
  } //main
  
  
  private static void zerovar(IloCplex cplex, IloIntVar v ) throws IloException {
		IloLinearNumExpr expr = cplex.linearNumExpr(); 
		expr.addTerm(1,v); 
		cplex.addEq(expr,0);
  }
   
 

 private static int  compute_nbpu(Data data, IloCplex cplex, IloIntVar[][] P ) throws IloException {
		Vector<Double> distinct_values = new Vector();
		for (int i = 0; i< P.length; i++){
			for (int k= 0; k < data.rep; k++){
				assert (P[i].length == data.rep);
				assert(cplex.getValue(P[i][k]) > 0.1); // no replication thus all tasks are mapped somewhere
				Boolean exists = false;
				for (int  j = 0; j< distinct_values.size() ; j++){
					if (Math.abs(cplex.getValue(P[i][k]) - distinct_values.elementAt(j)) < 0.1 ){
						exists = true; break;
					}
				}
				if (!exists) 
					distinct_values.add(cplex.getValue(P[i][k]));
			}
		}
		return distinct_values.size();
  }
  
  private static double compute_wsup_pipeline(Data data) {
		assert(data.nlinks != -1);
		double Sup = 0;
		for (int j = 0; j < data.texes.length; j++){
			double max = data.texes[j][0];
			for (int k = 0; k < data.texes[j].length; k++){
					max = Math.max(max,data.texes[j][k]);
			}
			Sup = Sup + data.rep*max/data.pfreqs[0];
		}
		for (int i = 0; i < data.tcoms.length; i++)
			for (int j = 0; j < data.tcoms[i].length; j++)
					//	Sup = Sup + data.rep*data.rep*data.nlinks*data.tcoms[i][j];
						Sup = Sup + data.rep*data.rep*data.tcoms[i][j];
						
		assert(Sup > 0);
		return Sup;
  }
  
  
    private static double compute_winf_pipeline(Data data, int nbpo, int nlinks) {
		assert(data.nprocs == nbpo);
		assert(data.nlinks == nlinks);
		double Infexe = 0;
		for (int j = 0; j < data.texes.length; j++){
			double min = data.texes[j][0];
			for (int k = 0; k < data.texes[j].length; k++){
					min = Math.min(min,data.texes[j][k]);
			}
			Infexe = Infexe + 1*min/data.pfreqs[data.pfreqs.length-1];
		}
		double Infcom = 0;
		/*for (int i = 0; i < data.tcoms.length; i++)
			for (int j = 0; j < data.tcoms[i].length; j++)
						Infcom = Infcom + data.tcoms[i][j];
		*/				
		assert(Infcom/data.nprocs+Infexe/data.nlinks > 0);
		return Infexe/data.nprocs;

	}
	
 private static boolean has_succs(Data data,int i){
	for (int j = 0; j< data.succs[i].length; j++){
		if (data.succs[i][j] != 0) return true;
	}
	return false;	
  }


 private static boolean has_preds(Data data,int i){
	assert(i< data.ntasks);
	for (int jj=0; jj< data.ntasks; jj++){
		if (data.succs[jj][i] == 1) 
			return true;
	}
	return false;
  }

}

