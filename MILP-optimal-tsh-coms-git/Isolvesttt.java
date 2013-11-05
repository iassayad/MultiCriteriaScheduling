import ilog.concert.*;
import ilog.cplex.*;
import java.io.*;
import java.util.*;

public class Isolvesttt {
	static private class Data {
	     
	      
	      double[] plambdas;
		  double[] llambdas;

	      double[] pfreqs;
	      double[] texes;
		  double[][] tcoms;

	      int[][] succs;
	      
		 // Vector<Vector<Vector<Vector<int>>>> CH;
		  Vector < Vector < Vector < Vector <Integer> > >  > CH;
		  
	      int ntasks;
	      int nprocs;
	      int nfreqs;
	      int nlinks;
		   int nch;
	      int rep;
 
	      
	      Double Po;
	      Double lo;
	      
	      Double Max;
	      int limit;
	      
	      
	      
	      Data(String filename) throws IloException, java.io.IOException,
	                                   InputDataReader.InputDataReaderException
	      {
	         InputDataReader reader = new InputDataReader(filename);

	       

	         plambdas = reader.readDoubleArray();
			 llambdas = reader.readDoubleArray();
	         pfreqs = reader.readDoubleArray();
	         texes = reader.readDoubleArray();
	         rep  = reader.readInt();
			 tcoms = reader.readDoubleArrayArray();

	         succs = new int[tcoms.length][tcoms.length];
			 for (int i=0; i < tcoms.length; i++) {
					for (int j=0; j <  tcoms.length; j++) {
						succs[i][j]= (tcoms[i][j] != 0) ? 1 :  0;
					}
				
			 } 
	         
	         ntasks = texes.length / plambdas.length;
	         nprocs = plambdas.length;
	         nfreqs = pfreqs.length;
	         
			 ArrayList<Chemin.Lien> arch_liens = Chemin.initialiser_archi_liens(nprocs);
             nlinks = arch_liens.size();
			 assert(nlinks == llambdas.length);
			 
			 initialize_chemins(arch_liens);
			// afficher_chemins();
			 assert (nprocs >= 2);
			 nch = CH.elementAt(0).elementAt(1).size();	// verifier que ce nombre est toujours le meme meme en faisant varier les extremites des chemins i et j
	         Po = reader.readDouble();
	         lo = reader.readDouble();
	         limit = reader.readInt(); 
	      
	         Max = 0.0;
	         for (int j = 0; j < texes.length; j++)
		           Max = Max + rep*texes[j]/pfreqs[0];
			 for (int i = 0; i < tcoms.length; i++)
					for (int j = 0; j < tcoms[i].length; j++)
						Max = Max + rep*rep*nlinks*tcoms[i][j];
			 Max = Math.max(Max, 1);

	      }
		  
		  public void  initialize_chemins(ArrayList<Chemin.Lien> arch_liens){
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
	         if ( args.length >= 1)  filename = args[0];
	         else                 
	        	                    filename = "data/tasks5.txt";

	         Data     data  = new Data(filename);
	        
	         
			  String based = "/home/popart/assayad/Desktop/tsh-results-sttt-optimal-5op-depardon/";
			 //String based = "/home/ismail/Desktop/tsh-results-sttt-optimal-5op/";
	       
	         File out_channel1 = create_file (based+"optimal/big_lambda_rep"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");
	         File out_channel2 = create_file (based+"optimal/power_rep"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");
	         File out_channel3 = create_file (based+"optimal/length_rep"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");
	         File out_channel4 = create_file (based+"optimal/big_lambda_obj"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");
	         File out_channel5 = create_file (based+"optimal/power_obj"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");
	         File out_channel6 = create_file (based+"optimal/status"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");
	         File out_channel7 = create_file (based+"optimal/time"+data.rep+"_nbproc"+data.nprocs+"_nbop"+data.ntasks+".txt");

	         
			//double[] borneInfL4fois10emoins5 = {85.00000000000001, 77.74000000000001 , 55.96000000000001, 53.999999999999986, 52.33000000018591, 52.33000000000001, 52.33, 52.33, 52.33000000000001, 52.329999999999984, 52.329999999999984, 52.33, 52.33, 52.33, 52.33, 52.33};
			double[] borneinf_previousline = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
			double bornesup_thisline = data.Max;
			 // valeur obtenu bicritere sans Robj ni contrainte S8S2 : 85.0 77.74 58.00000000000076 54.0 52.33000000018591 52.33 52.33 52.33 52.33000000000001 52.329999999999984 52.329999999999984 52.33 52.33 52.33 52.33 

			   //double bornesup = data.Max;
			 //double bornesup = 180.4833; 
			 //double bornesup_previousline = bornesup; 
	     //  for (int l2 = 15; l2 <= 20; l2++) { // 14 values  4e-6 .... 0.4 0.8
			 for (int l2 = 15; l2 >= 14; l2--) { // 14 values  8e-6 4e-6 8e-7 4e-7
	             for (int l1 = 2; l1 >= 1; l1--) { 
					//bornesupinf = bornesup_previousline;
				  bornesup_thisline = data.Max;
	              for (int p2 = 5; p2 <= 20; p2++){ // 16 values 1 1.2 ....4
					data.lo = l1 * 4 * Math.pow(10,-(21-l2)) ;
	            	 
	 	            data.Po =  p2 * 0.2;
					
	 	             System.out.println("Lambda obj = "+data.lo);
	 	             System.out.println("Power obj = "+data.Po);
					 
			 System.out.println("--->DECLARATIONS");
			
		
			// For better perf as in (http://www-01.ibm.com/support/docview.wss?uid=swg21400034) :
//			 IloCplex.DoubleParam.EpRHS = Math.exp(-1); //feasibility tolerance  
//			 IloCplex.DoubleParam.EpOpt = Math.exp(-1); //simplex optimality tolerance
//			 IloCplex.DoubleParam.ScaInd = 1; http://www.ieor.berkeley.edu/Labs/ilog_docs/html/usrcplex/solveLP14.html
//			 IloCplex.DoubleParam.EpMrk= .9999;
//			 IloCplex.BooleanParam.NumericalEmphasis = true; http://pic.dhe.ibm.com/infocenter/cosinfoc/v12r2/index.jsp?topic=%2Filog.odms.cplex.help%2FContent%2FOptimization%2FDocumentation%2FCPLEX%2F_pubskel%2FCPLEX429.html
//			 EpGap // Relative optimality tolerance http://pic.dhe.ibm.com/infocenter/cplexzos/v12r4/index.jsp?topic=%2Fcom.ibm.cplex.zos.help%2FUsrMan%2Ftopics%2Fdiscr_optim%2Fmip%2Ftroubleshoot%2F59_optim_criteria.html
//			 setParam RelObjDif = 0.1  // To speed up the proof of optimality, you can set objective difference parameters, /
								// setting the RelObjDif to 0.01 would mean that CPLEX would skip any potential new solution that is not at least 1% better than the incumbent solution.
								 //  http://pic.dhe.ibm.com/infocenter/cplexzos/v12r4/index.jsp?topic=%2Fcom.ibm.cplex.zos.help%2FUsrMan%2Ftopics%2Fdiscr_optim%2Fmip%2Ftroubleshoot%2F59_optim_criteria.html
//           VarSel = 3 // strong branching //http://pic.dhe.ibm.com/infocenter/cplexzos/v12r4/index.jsp?topic=%2Fcom.ibm.cplex.zos.help%2FUsrMan%2Ftopics%2Fdiscr_optim%2Fmip%2Ftroubleshoot%2F58_best_node_stuck.html
//	         StrongCandLim // http://pages.cs.wisc.edu/~ferris/cs635/cplex.pdf imit on the length of the candidate list for strong branching (varsel = 3). (default = 10) 
//           strongitlim (integer)  // http://pages.cs.wisc.edu/~ferris/cs635/cplex.pdfLimit on the number of iterations per branch in strong branching (varsel = 3). The default value of 0 causes 
                         //the limit to be chosen automatically which is normally satisfactory. Try reducing this value if the time per 
                         //node seems excessive. Try increasing this value if the time per node is reasonable but Cplex is making little 
                          //progress. 
//			 Probe and MIPEmphasis http://pic.dhe.ibm.com/infocenter/cplexzos/v12r4/index.jsp?topic=%2Fcom.ibm.cplex.zos.help%2FUsrMan%2Ftopics%2Fdiscr_optim%2Fmip%2Ftroubleshoot%2F59_optim_criteria.html					
// BtTol Setting that parameter to a value near 0.0 increases the likelihood that a backtrack will occur, 
			     IloCplex cplex = new IloCplex(); 
			 
			// cplex.setParam(IloCplex.DoubleParam.EpRHS, 1.0e-1);
		//	 cplex.setParam(IloCplex.DoubleParam.EpOpt , 1.0e-1);
			 
			// cplex.setParam( IloCplex.IntParam.ScaInd , 1);
		//	 cplex.setParam( IloCplex.DoubleParam.EpMrk, .9999);
		//	 cplex.setParam( IloCplex.BooleanParam.NumericalEmphasis, true); // includes two preceding perf optim
		//	 cplex.setParam(IloCplex.DoubleParam.EpGap, 7.5e-1);
		//	 cplex.setParam(IloCplex.DoubleParam.RelObjDif, 1.0e-1);
		//	 cplex.setParam( IloCplex.IntParam.VarSel , 3);
		//	 cplex.setParam( IloCplex.IntParam.StrongCandLim, 100);
		//	 cplex.setParam( IloCplex.IntParam.StrongItLim, 100);
		//	 cplex.setParam( IloCplex.IntParam.Probe, 3);
// cuts :
		/*	 cplex.setParam( IloCplex.IntParam.Cliques , 2);
			 cplex.setParam( IloCplex.IntParam.Covers , 3);
			 cplex.setParam( IloCplex.IntParam.DisjCuts , 3);
			 cplex.setParam( IloCplex.IntParam.FlowCovers , 2);
			 cplex.setParam( IloCplex.IntParam.FlowPaths , 2);
			 cplex.setParam( IloCplex.IntParam.FracCuts , 2);
			 cplex.setParam( IloCplex.IntParam.GUBCovers , 2);
			 cplex.setParam( IloCplex.IntParam.ImplBd , 2);
			 cplex.setParam( IloCplex.IntParam.MIRCuts  , 2);
			 */
			 cplex.setParam( IloCplex.IntParam.MIPEmphasis, 2); //http://pic.dhe.ibm.com/infocenter/cosinfoc/v12r2/index.jsp?topic=%2Filog.odms.cplex.help%2FContent%2FOptimization%2FDocumentation%2FCPLEX%2F_pubskel%2FCPLEX998.html
		//	 cplex.setParam( IloCplex.DoubleParam.RelObjDif, 1.0e-1);
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
            
			// unlike l processors number starts with 1
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
	   
			//S3 K a ll c k
			 //a set  of c's : indices of communiation chemins combinations w0 =(c,0) w1=(c,1) .... wk-1=(c,k-1) 
			 // between given proc combination p0=(a,0) p1 =(a,1) .... pk-1=(a,k-1) and a proc destination (ll) 
			 // contains the exact number of combinations => safe to use S3 K a ll size() unlike S1 and S2
			
			// S3 K a ll c k  
			//Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3 = new Vector();
			System.out.println("------> Intialisation");
			System.out.println("initialiserS3");
			Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3 = initialiserS3(data,S1);
			//afficherS3 (S3,data);
			System.out.println("initialiserS4");
			Vector<Vector<Vector<Integer>>>  S4 = initialiserS4(data);
			System.out.println("initialiserS5");
			Vector<Vector<Vector<Vector<Integer>>>> S5 = initialiserS5(data, S1, S4);
			//afficherS4andS5(S4,S5, data);
			try{ Thread.sleep(10000);} catch(InterruptedException e){};
			System.out.println("initialiserS6");
			Vector<Vector<Vector<Integer>>> S6 = initialiserS6(data);
			System.out.println("initialiserS7");
			Vector<Vector<Vector<Vector<Integer>>>> S7 = initialiserS7(data, S1,S6);
			//afficherS6andS7(S6,S7, data);
			System.out.println("initialiserS8");
			Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Integer>>>>>>>>>> S8 = initialiserS8(data, S1, S3, S4, S5, S6, S7);
			//afficherS8(S8, data);
			try{ Thread.sleep(10000);} catch(InterruptedException e){};

			System.out.println("<------ Intialisation");
	          // coms
			  //------------------------
			  // Existence of routing 
			  // R i j
			IloIntVar[][] R = new IloIntVar[data.ntasks][data.ntasks];
			for (int j = 0; j < data.ntasks; j++){
				R[j] = cplex.boolVarArray(data.ntasks);
			}
			
		/*	IloIntVar[][] SP = new IloIntVar[data.ntasks][data.ntasks];
			for (int j = 0; j < data.ntasks; j++){
				SP[j] = cplex.boolVarArray(data.ntasks);
			}
			*/
			//SP

			// tc i k n j k'  between i k  and j k'
			IloNumVar[][][][][] tc = new IloNumVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
					for (int n = 0; n < data.nlinks; n++)
						for (int j = 0; j < data.ntasks; j++)
							for (int kk = 0; kk < data.rep; kk++){
								tc[i][k][n][j][kk] = cplex.numVar(0.0, data.Max);
							}
							
			// tr i j
	         IloNumVar[][] tr = new IloNumVar[data.ntasks][data.ntasks];
	         for (int i = 0; i < data.ntasks; i++)
				for (int j = 0; j < data.ntasks; j++)
					tr[i][j] = cplex.numVar(0.0, data.Max);
											
			// tcbr i k n j between i k and r i j 
			IloNumVar[][][][] tcbr = new IloNumVar[data.ntasks][data.rep][data.nlinks][data.ntasks];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
					for (int n = 0; n < data.nlinks; n++)
						for (int j = 0; j < data.ntasks; j++){
							tcbr[i][k][n][j] = cplex.numVar(0.0, data.Max);
						}
							
			// tcar i n j k' between r i j and j k' 
			IloNumVar[][][][] tcar = new IloNumVar[data.ntasks][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++)
				for (int n = 0; n < data.nlinks; n++)
					for (int j = 0; j < data.ntasks; j++)
						for (int kk = 0; kk < data.rep; kk++){
								tcar[i][n][j][kk] = cplex.numVar(0.0, data.Max);
						}
			
							
			// vtr
	         IloIntVar[][][][] vtr = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.ntasks];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l <  data.ntasks; l++)
						for (int m = 0; m < data.ntasks; m++)
							vtr[j][k][l][m] = cplex.boolVar();

			// vrt
	         IloIntVar[][][][] vrt = new IloIntVar[data.ntasks][data.ntasks][data.ntasks][data.rep];
				for (int l = 0; l < data.ntasks; l++)
				   for (int m = 0; m < data.ntasks; m++)
					   for (int j = 0; j < data.ntasks; j++)
					     for (int k = 0; k < data.rep; k++)
		        			 vrt[l][m][j][k] = cplex.boolVar();
							 
							 
			 // gtr[q][k][i][j]
			//XXXX
			IloIntVar[][][][] gtr = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.ntasks];
			for (int j = 0; j < data.ntasks; j++)
				for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.ntasks; l++)
						for (int m = 0; m < data.ntasks; m++)
							gtr[j][k][l][m] = cplex.boolVar();

			 // grt
			 //XXXX
			 IloIntVar[][][][] grt = new IloIntVar[data.ntasks][data.ntasks][data.ntasks][data.rep];
			 for (int l = 0; l < data.ntasks; l++)
				for (int m = 0; m < data.ntasks; m++)
					for (int j = 0; j < data.ntasks; j++)
						for (int k = 0; k < data.rep; k++)
		        			 grt[l][m][j][k] = cplex.boolVar();

			 // Pr
			 //XXXX
			 IloIntVar[][] Pr = new IloIntVar[data.ntasks][data.ntasks];
	         for (int j = 0; j < data.ntasks; j++)
	            Pr[j] = cplex.intVarArray(data.ntasks, 0, data.nprocs);
			 //xr
			 //XXXX
			  // xr[j][k][l] => l means processor number (l+1)
	         IloIntVar[][][] xr = new IloIntVar[data.ntasks][data.ntasks][data.nprocs];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.ntasks; k++)
		 	            xr[j][k] = cplex.boolVarArray(data.nprocs);
			
			 
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
							
			// xcbr i k n j between i k and r i j 
			IloIntVar[][][][] xcbr = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
					for (int n = 0; n < data.nlinks; n++)
						for (int j = 0; j < data.ntasks; j++){
							xcbr[i][k][n][j] = cplex.boolVar();
							//if (data.succs[i][j] == 0) xcbrc[i][k][n][j] = 0; (done later ) 
						}
							
			// xcar i n j k' between r i j and j k' 
			IloIntVar[][][][] xcar = new IloIntVar[data.ntasks][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++)
				for (int n = 0; n < data.nlinks; n++)
					for (int j = 0; j < data.ntasks; j++)
						for (int kk = 0; kk < data.rep; kk++){
								xcar[i][n][j][kk] = cplex.boolVar();
								//if (data.succs[i][j] == 0) xcar[i][n][j][kk] = 0; (done later ) 
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
			//chbr[i][k][j][l][ll][w]
			IloIntVar[][][][][][] chbr = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.nprocs][data.nprocs][data.nch];
			for (int i = 0; i < data.ntasks; i++)
				for (int k = 0; k < data.rep; k++)
						for (int j = 0; j < data.ntasks; j++)
								for (int l = 0; l < data.nprocs; l++)
									for (int ll = 0; ll < data.nprocs; ll++)
										chbr[i][k][j][l][ll] = cplex.boolVarArray(data.nch);
										
			//char[i][j][kk][l][ll][w]
			IloIntVar[][][][][][] chaar = new IloIntVar[data.ntasks][data.ntasks][data.rep][data.nprocs][data.nprocs][data.nch];
			for (int i = 0; i < data.ntasks; i++)
						for (int j = 0; j < data.ntasks; j++)
							for (int kk = 0; kk < data.rep; kk++)
								for (int l = 0; l < data.nprocs; l++)
									for (int ll = 0; ll < data.nprocs; ll++)
										chaar[i][j][kk][l][ll] = cplex.boolVarArray(data.nch);
										
			System.out.println(" ----> S8S2 ");
			// S8S2[i][e][f][K][a][b][g][h][o]							
		/*	IloIntVar[][][][][][][][][] S8S2 = new IloIntVar[data.ntasks][(int)Math.pow(2,data.ntasks-1)][(int)Math.pow(data.nprocs,data.ntasks-1)][data.rep+1][(int)Math.pow(data.nprocs,data.rep)][(int)Math.pow(data.nfreqs,data.rep)][(int)Math.pow(2,data.ntasks-1)][(int)Math.pow(data.nprocs,data.ntasks-1)][(int)Math.pow((int)Math.pow(data.nch,data.ntasks-1),data.rep)] ;						
			for (int i=0; i<data.ntasks; i++)
				for (int e =0; e< Math.pow(2+1,data.ntasks-1); e++)
					for (int f=0; f < Math.pow(data.nprocs+1,data.ntasks-1); f++)
						for (int K=0; K<= data.rep; K++)
							for (int a=0; a< Math.pow(data.nprocs,data.rep); a++)
								for (int b=0; b< Math.pow(data.nfreqs,data.rep);b++)
									for (int x =0; x< Math.pow(2+1,data.ntasks-1); x++)
										for (int h =0; h<Math.pow(data.nprocs+1,data.ntasks-1); h++)
											for (int o=0; o <Math.pow(Math.pow(data.nch,data.ntasks-1),data.rep); o++)
												S8S2[i][e][f][K][a][b][x][h][o] = cplex.boolVar();
		*/										
			IloIntVar[][][][][][][][][] S8S2 = new IloIntVar[data.ntasks][2][3][data.rep+1][(int)Math.pow(data.nprocs,data.rep)][(int)Math.pow(data.nfreqs,data.rep)][4][9][16] ;						
			for (int i=0; i<data.ntasks; i++)
				for (int e =0; e< 2; e++)
					for (int f=0; f < 3; f++)
						for (int K=0; K<= data.rep; K++)
							for (int a=0; a< Math.pow(data.nprocs,data.rep); a++)
								for (int b=0; b< Math.pow(data.nfreqs,data.rep);b++)
									for (int x =0; x< 4; x++)
										for (int h =0; h<9; h++)
											for (int o=0; o < 16; o++)
												S8S2[i][e][f][K][a][b][x][h][o] = cplex.boolVar();

			System.out.println(" <---- S8S2 ");
										 
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

            //gccar[i][k][n][j][k'][ii][nn][jj][kk']
			IloIntVar[][][][][][][][][]  gccar = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep][data.ntasks][data.nlinks][data.ntasks][data.rep];
				for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gccar[i][k][n][j][kk][ii][nn][jj][kkkk] = cplex.boolVar();												
													}
											}
										}
									}
							}
						}
					}
				}
			}	
			// gccbr[i][k][n][j][k'][ii][kk][nn][jj]
		IloIntVar[][][][][][][][][]  gccbr = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep][data.ntasks][data.rep][data.nlinks][data.ntasks];
		for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gccbr[i][k][n][j][kk][ii][kkk][nn][jj] = cplex.boolVar();
												}	
											}
									}
								}
							}
						}
					}
				}
			}	
			 // gcbrc[i][k][n][j][ii][kk][nn][jj][kk']
			IloIntVar[][][][][][][][][]  gcbrc = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] = cplex.boolVar();
												}		
											}
										}
									}
								}
							}
					}
				}
			}	
			//gcbrcbr[i][k][n][j][ii][kk][nn][jj] 
			IloIntVar[][][][][][][][]  gcbrcbr = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.ntasks][data.rep][data.nlinks][data.ntasks];
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcbrcbr[i][k][n][j][ii][kkk][nn][jj] = cplex.boolVar();
												}	
											}
									}
								}
							}
					}
				}
			}	
			// gcbrcar[i][k][n][j][ii][nn][jj][kk'] 
			IloIntVar[][][][][][][][]  gcbrcar = new IloIntVar[data.ntasks][data.rep][data.nlinks][data.ntasks][data.ntasks][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcbrcar[i][k][n][j][ii][nn][jj][kkkk] = cplex.boolVar();
												}	
											}
										}
									}
							}
					}
				}
			}	
			// gcarc[i][n][j][k'][ii][kk][nn][jj][kk']
			IloIntVar[][][][][][][][][] gcarc = new IloIntVar[data.ntasks][data.nlinks][data.ntasks][data.rep][data.ntasks][data.rep][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] = cplex.boolVar();
												}		
											}
										}
									}
								}
							}
						}
					}
			}	
			
			//gcarcbr[i][n][j][k'][ii][kk][nn][jj] 
			IloIntVar[][][][][][][][]  gcarcbr = new IloIntVar[data.ntasks][data.nlinks][data.ntasks][data.rep][data.ntasks][data.rep][data.nlinks][data.ntasks];
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcarcbr[i][n][j][kk][ii][kkk][nn][jj] = cplex.boolVar();
												}	
											}
									}
								}
							}
						}
					}
			}	
			
		// gcarcar[i][n][j][k'][ii][nn][jj][kk']
			IloIntVar[][][][][][][][]  gcarcar = new IloIntVar[data.ntasks][data.nlinks][data.ntasks][data.rep][data.ntasks][data.nlinks][data.ntasks][data.rep];
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													gcarcar[i][n][j][kk][ii][nn][jj][kkkk] = cplex.boolVar();
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
	         IloNumVar Encombr = cplex.numVar(0.0, data.Max);
	         IloNumVar Encomar = cplex.numVar(0.0, data.Max);


		/*	IloNumVar[] EnexeT = new IloNumVar[data.ntasks];
			 for (int i = 0; i < data.ntasks; i++)  EnexeT[i] = cplex.numVar(0.0, data.Max);
			 IloNumVar[] EncomT = new IloNumVar[data.ntasks];
			 for (int i = 0; i < data.ntasks; i++)  EncomT[i] = cplex.numVar(0.0, data.Max);
			 IloNumVar[] EncombrT = new IloNumVar[data.ntasks];
			 for (int i = 0; i < data.ntasks; i++)  EncombrT[i] = cplex.numVar(0.0, data.Max);
			 IloNumVar[] EncomarT = new IloNumVar[data.ntasks];
			 for (int i = 0; i < data.ntasks; i++)  EncomarT[i] = cplex.numVar(0.0, data.Max);


			IloNumVar[] SumT =  new IloNumVar[data.ntasks];
			for (int i = 0; i < data.ntasks; i++)  SumT[i] = cplex.numVar(0.0, data.Max);
*/
	         IloNumVar Sum = cplex.numVar(0.0, data.Max);
	        // IloNumVar LSum = cplex.numVar(0.0, Double.MAX_VALUE);
	         IloNumVar logR = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
	      //   IloNumVar logRexe = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
	      //   IloNumVar logRcom = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
	       //  IloNumVar logRcombr = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
	        // IloNumVar logRcomar = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
			 
	         IloNumVar logRelPOS = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
			
			/* IloNumVar[] logRelPOST = new IloNumVar[data.ntasks];
			 for (int i = 0; i < data.ntasks; i++)  logRelPOST[i] = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);
*/
			System.out.println("<---DECLARATIONS");

	         // State disjunctive constraints for each resource
	         for (int j = 0; j < data.ntasks; j++) {
	        	 for (int k = 0; k < data.rep; k++){
	        		 IloLinearNumExpr expr = cplex.linearNumExpr(); 
	        		 expr.addTerm(1,t[j][k]);
		        	 for (int l = 0; l < data.nprocs; l++){
			        	 for (int m = 0; m < data.nfreqs; m++){
			        		 expr.addTerm(+data.texes[data.nprocs*j+l]/data.pfreqs[m],x4[j][k][l][m]);
			        	 }
		        	 }
		        	 expr.addTerm(-1,W);
				     //System.out.println("EXPR = "+expr);
		        	 cplex.addLe(expr,0);
	        	 }
	         }

	         //t[jj][kk] - t[j][k] - M*g[j][k][jj][kk]>= sum_lm (Djl/fm)*x4[j][k][l][m] - M 
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
	        	 							expr.addTerm(-data.texes[data.nprocs*j+l]/data.pfreqs[m],x4[j][k][l][m]);
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
        	 						expr.addTerm(1, g[j][k][jj][kk]); 	 				
        	 						expr.addTerm(1, g[jj][kk][j][k]);
        	 						expr.addTerm(1, v[j][k][jj][kk]); 	 				
        	 						expr.addTerm(1, v[jj][kk][j][k]);
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
		     
			 //sum l x3[j][k][l]
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
			        		 expr.addTerm(+data.texes[data.nprocs*j+l]*data.pfreqs[m]*data.pfreqs[m],x4[j][k][l][m]);
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
					IloLinearNumExpr exprT = cplex.linearNumExpr(); 
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


	    
				 //XXXXX
			 // Encombr : comm energy
		  {
        		IloLinearNumExpr expr = cplex.linearNumExpr(); 
        		expr.addTerm(-1,Encombr);
				for (int i = 0; i < data.ntasks; i++) {
					IloLinearNumExpr exprT = cplex.linearNumExpr(); 
					//exprT.addTerm(-1,EncombrT[i]);
					for (int j = 0; j < data.ntasks; j++) {
						if (data.succs[i][j] != 0) {
							for (int k = 0; k < data.rep; k++){
									//for (int l= 0; l < data.nprocs; l++){
									//	for (int ll= 0; ll < data.nprocs; ll++){
									//		for (int w= 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++){
									//			for (int n= 0; n <data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++){
													for (int n= 0; n <data.nlinks; n++){
														//  System.out.println("EXPR 13 : log(1-F)  vaut : "+Math.log(1-F));
														expr.addTerm(+data.tcoms[i][j],xcbr[i][k][n][j]);
													//	exprT.addTerm(+data.tcoms[i][j],xcbr[i][k][n][j]);
													}
									//		}
									//	}
									//}
							}
						}
					}
				//	cplex.addEq(exprT,0);

				}
    		    // System.out.println("EXPR 13  = "+expr);
    		     cplex.addEq(expr,0);
		     }

	    
			 //XXXXX
			 // Encomar : comm energy
		  {
        		IloLinearNumExpr expr = cplex.linearNumExpr(); 
        		expr.addTerm(-1,Encomar);
				for (int j = 0; j < data.ntasks; j++) {
					IloLinearNumExpr exprT = cplex.linearNumExpr(); 
					//exprT.addTerm(-1,EncomarT[j]);
					for (int i = 0; i < data.ntasks; i++) {
						if (data.succs[i][j] != 0) {
								for (int kk = 0; kk < data.rep; kk++){
									//for (int l= 0; l < data.nprocs; l++){
									//	for (int ll= 0; ll < data.nprocs; ll++){
									//		for (int w= 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++){
									//			for (int n= 0; n <data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++){
												for (int n= 0; n <data.nlinks; n++){
													//  System.out.println("EXPR 13 : log(1-F)  vaut : "+Math.log(1-F));
													expr.addTerm(+data.tcoms[i][j],xcar[i][n][j][kk]);
												//	exprT.addTerm(+data.tcoms[i][j],xcar[i][n][j][kk]);
												}
											//}
									//	}
									//}
								}
						}
					}
					//cplex.addEq(exprT,0);

				}
    		    // System.out.println("EXPR 13  = "+expr);
    		     cplex.addEq(expr,0);
		     }

		     {
			 // En/W
			IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
			expr1.addTerm(1,En);
			expr1.addTerm(-1,Enexe);
			expr1.addTerm(-1,Encom);
			expr1.addTerm(-1,Encombr);
			expr1.addTerm(-1,Encomar);
    		cplex.addEq(expr1, 0);
			
		    IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
    		expr2.addTerm(1,En);
    		expr2.addTerm(-data.Po,W);
    		cplex.addLe(expr2, 0);

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
			        		 expr.addTerm(+data.texes[data.nprocs*j+l]/data.pfreqs[m],x4[j][k][l][m]);
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
			
			System.out.println("DEFS");

			/////////////////////////////////////////////////
			// DEFS
			/////////////////////////////////////////////////
			//XXXX
			// SP[i][j] is 1 if all ik j kk on same proc 0 otherwise :  PROBLEME : no its  only SP[i][j] is 1 if all ik j kk on same proc
			// SP[i][j] <= 1 - v[i][k][j][kk]
			// SP[i][j] + sum_k_kk v[i][k][j][kk] + v[j][kk][i][k] >= 1
			
	/*		for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
	        	 	for (int j = 0; j < data.ntasks; j++) {
        	 			for (int kk = 0; kk < data.rep; kk++){
							IloLinearNumExpr expr = cplex.linearNumExpr();
							expr.addTerm(1, SP[i][j]);
							expr.addTerm(1, v[i][k][j][kk]);
							//System.out.println("EXPR  SP[i][j] ... = "+expr);
							cplex.addLe(expr,1);
	        	 	    }
	        	     }
				 }
			  }
		
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {
	        	 // for each replica jk 
				 IloLinearNumExpr expr = cplex.linearNumExpr();
				 expr.addTerm(1, SP[i][j]);
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
        	 			for (int kk = 0; kk < data.rep; kk++){
							expr.addTerm(1, v[i][k][j][kk]);
							expr.addTerm(1, v[j][kk][i][k]);
							//System.out.println("EXPR 3 = "+expr);
	        	 	    }
	        	     }
				cplex.addGe(expr,1);
				 }
			  }
			  
	*/		
			// si j succ i : lin Rij = sum YiK * sum YjK'
    		//               Rij <= sum_K YiK  all 2<=K<=Rep
			//               Rij <= sum_K YjK all 2<=K<=Rep
			//               Rij + 1  + M*SP[i][j] >=  sum_K YiK + sum_K YjK all 2<=K<=Rep all 2<=K<=Rep (deactivate formula if all i replicas AND all j replicas on SAME proc)
			//               R[i][j] <= M - M* SP[i][j] (f all i replicas AND all j replicas on SAME proc => R ij = 0 )

// MODIF : garder expr 1, 2 , 3 sans SP
			// si j not succ i : Rij = 0 s
    	/*	{        		
    		    for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {
						if (data.succs[i][j] != 0) {
							IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
						//	IloLinearNumExpr expr4 = cplex.linearNumExpr(); 

        	        		expr1.addTerm(1,R[i][j]);
							expr2.addTerm(1,R[i][j]);
							expr3.addTerm(1,R[i][j]);
							//expr3.addTerm(data.Max,SP[i][j]);
						//	expr4.addTerm(1,R[i][j]);
					//		expr4.addTerm(data.Max,SP[i][j]);
							for (int K = 2; K <= data.rep ; K++){
								expr1.addTerm(-1, Y[i][K]);	
								expr2.addTerm(-1, Y[j][K]);	
								expr3.addTerm(-1, Y[i][K]);	
								expr3.addTerm(-1, Y[j][K]);	
							}
							cplex.addLe(expr1,0);
							cplex.addLe(expr2,0);
							cplex.addGe(expr3,-1);
						//	cplex.addLe(expr4,data.Max);							
					
							}else {
							IloLinearNumExpr expr5 = cplex.linearNumExpr(); 
							expr5.addTerm(1,R[i][j]);
							cplex.addEq(expr5,0);
						}
					}
				}	
						
    		} */
			// si j succ i : lin Rij = max (sum YiK , sum YjK') over 2<=K<=Rep all 2<=K'<=Rep 	
    		//               Rij >= sum_K YiK  all 2<=K<=Rep
			//               Rij >= sum_K YjK all 2<=K<=Rep
			//               Rij <=   sum_K YiK + sum_K YjK all 2<=K<=Rep all 2<=K'<=Rep 			 
				{        		
    		    for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {
						if (data.succs[i][j] != 0) {
							IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr3 = cplex.linearNumExpr(); 

        	        		expr1.addTerm(1,R[i][j]);
							expr2.addTerm(1,R[i][j]);
							expr3.addTerm(1,R[i][j]);
							for (int K = 2; K <= data.rep ; K++){
								expr1.addTerm(-1, Y[i][K]);	
								expr2.addTerm(-1, Y[j][K]);	
								expr3.addTerm(-1, Y[i][K]);	
								expr3.addTerm(-1, Y[j][K]);	
							}
							cplex.addGe(expr1,0);
							cplex.addGe(expr2,0);
							cplex.addLe(expr3,0);
						//	cplex.addLe(expr4,data.Max);							
					
							}else {
							IloLinearNumExpr expr5 = cplex.linearNumExpr(); 
							expr5.addTerm(1,R[i][j]);
							cplex.addEq(expr5,0);
						}
					}
				}	
			} 
			System.out.println("EXISTENCE and ZEROING");
		
			/////////////////////////////////////////////////
			// EXISTENCE and ZEROING
			/////////////////////////////////////////////////
			
			//XXXX : regrouper les existences ci-dessous avec les zeroing correspondants mais eparpilles ici et la
			
			//XXXX
			// Existence de xr (peut etre deduit de l'existence de r ij) : une op de routage existe si  
			//																    1) rij = 1 (chech this : dependance, i exisite, j existe, replicas existent, pas tous replicas sur meme proc ) 
			//                  sinon zeroing xr 
			//
			// sum_l xr[i][j][l] = R[i][j]
			
			  for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {
							IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
							expr1.addTerm(1,R[i][j]);
							for (int l = 0; l < data.nprocs ; l++){
								expr1.addTerm(-1, xr[i][j][l]);	
							}
							cplex.addEq(expr1,0);
					}
				}	
			
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
										expr1.addTerm(1,R[i][j]);
										expr2.addTerm(1,R[i][j]);
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
			
				//
			// Existence de chbr : un ch doit exister entre une op et un routing si   
			//																	1) op existante (deduite de 3 : non !)
			//																	2) i, j dependantes (deduite de 3 )
			//																	3) xr existante
			//																	4) pas sur meme proc (deduite de 3 : non !)
			//																	5) avec routage (deduite de 3)
			//                  sinon zeroing  chbr (already done for each of these cases ?) 
			// 
			//  if j succ i  &&  l /= l' : sum_w chbr[i][k][j][l][l'][w]  = x3[i][k][l]*xr[i][j][l'] XXXXX : faut-il mettre une egalite ? oui, meme raison qu'avant
			//                                                                                       XXXXX : en fait c'est ieux de lineariser reecrire en 3 contraintes
			// 1 if j succ i  &&  l /= l' : sum_w chbr[i][k][j][l][l'][w]  <=  x3[i][k][l]       
			// 2 if j succ i  &&  l /= l' : sum_w chbr[i][k][j][l][l'][w]  <=  xr[i][j][l']       
			// 3 if j succ i  &&  l /= l' : sum_w chbr[i][k][j][l][l'][w] + 1 >=  x3[i][k][l] + xr[i][j][l']   
			//                   
			// chbr[i][k][j][l][l'][w] <=  x3[i][k][l] 
			// chbr[i][k][j][l][l'][w] <=  xr[i][j][l'] 
			// 4 chbr[i][k][j][l][l][w] = 0 
			// 4 if j not succ i : chbr[i][k][j][l][l'][w] = 0 
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int l = 0; l < data.nprocs; l++) {				
							for (int ll = 0; ll < data.nprocs; ll++) {		
								if ( data.succs[i][j] !=0 && l != ll) {
									IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
									IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
									IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
									expr1.addTerm(-1,x3[i][k][l]);
									expr2.addTerm(-1,xr[i][j][ll]);
									expr3.addTerm(-1,x3[i][k][l]);
									expr3.addTerm(-1,xr[i][j][ll]);									
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										expr1.addTerm(1, chbr[i][k][j][l][ll][w]);	
										expr2.addTerm(1, chbr[i][k][j][l][ll][w]);
										expr3.addTerm(1, chbr[i][k][j][l][ll][w]);	
									}
									cplex.addLe(expr1,0);
									cplex.addLe(expr2,0);
									cplex.addGe(expr3,-1);
								}else{
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {				
										IloLinearNumExpr expr4 = cplex.linearNumExpr(); 
										expr4.addTerm(1, chbr[i][k][j][l][ll][w]);
										cplex.addEq(expr4,0);
									}
								}
							}
						}
					}
				}
			}	

			// Existence de char : un char doit exister entre un routing et une op si   
			//																	1) op existante (deduite de 3 : non ! )
			//																	2) i, j dependantes (deduite de 3)
			//																	3) xr existante
			//																	4) pas sur meme proc (deduite de 3 : non !) 
			//																	5) avec routage (deduite de 3)
			//                  sinon zeroing  char (already done for each of these cases ?) 
			//
			// if j succ i  &&  l /= l' : sum_w chaar[i][j][k'][l][l'][w] = xr[i][j][l]*x3[j][k'][l'] XXXXX : faut-il mettre une egalite ? oui, meme raison qu'avant
			//                                                                                       XXXXX : lineariser en 3 contraintes comme avant
			//chaar[i][j][kk][l][l][w] = 0 
			//chaar[i][j][kk][l][l'][w] <= xr[i][j][l] 
			//chaar[i][j][kk][l][l'][w] <= x[j][kk][l'] 
			// if j not succ i : chaar[i][j][kk][l][l'][w] = 0  
			//
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {	
					for (int kk = 0; kk < data.rep; kk++) {
						for (int l = 0; l < data.nprocs; l++) {				
							for (int ll = 0; ll < data.nprocs; ll++) {		
								if ( data.succs[i][j] !=0 && l != ll) {
									IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
									IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
									IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
									expr1.addTerm(-1,xr[i][j][l]);
									expr2.addTerm(-1,x3[j][kk][ll]);
									expr3.addTerm(-1,xr[i][j][l]);
									expr3.addTerm(-1,x3[j][kk][ll]);									
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										expr1.addTerm(1, chaar[i][j][kk][l][ll][w]);	
										expr2.addTerm(1, chaar[i][j][kk][l][ll][w]);
										expr3.addTerm(1, chaar[i][j][kk][l][ll][w]);	
									}
									cplex.addLe(expr1,0);
									cplex.addLe(expr2,0);
									cplex.addGe(expr3,-1);
								}else{
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {				
										IloLinearNumExpr expr4 = cplex.linearNumExpr(); 
										expr4.addTerm(1, chaar[i][j][kk][l][ll][w]);
										cplex.addEq(expr4,0);
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
			
			// Existence de xcbr[i][k][n][j] : si 
			//																	1) operation existante (deduite de 5)
			//																	2) dependentes (deduite de 5)
			//																	3) pas sur meme proc (deduite de 5 : oui)
			//																	4) avec routage : existence de xr   (deduite de 5)
			//																	5) chbr i k j l l' w existe  avec l proc de operation et l' proc de routing
			//																	6) n appartient a CH[l][l'][W] (remplace n directement dans la notation de xcbr. 
			//																									en faisant varier l et l' on couvre tout les n possibles )	
			//					sinon zeroing xcbr
			// 
			// for all n xcbr[i][k][CH[l][l'][w][n]][j] = chbr[i][k][j][l][l'][w]
			// MODIF for all n xcbr[i][k][m][j] =  (sum_l_l' / CH[l][l'][w][index] == m) chbr[i][k][j][l][l'][w]
			
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {	
						for (int m = 0; m < data.nlinks; m++) {
							IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
							expr1.addTerm(1, xcbr[i][k][m][j]);	
							for (int l = 0; l < data.nprocs; l++) {				
								for (int ll = 0; ll < data.nprocs; ll++) {		
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										if (data.CH.elementAt(l).elementAt(ll).elementAt(w).contains(m)){
												expr1.addTerm(-1, chbr[i][k][j][l][ll][w]);
										}
									}
								}
							}
							cplex.addEq(expr1, 0);	
						}			
					}
				}
			}	

			
			// Existence de xcar[i][n][j][k] : si 
			//																	1) operation existante (deduite de 5)
			//																	2) dependentes (deduite de 5)
			//																	3) pas sur meme proc (deduite de 5 : oui)
			//																	4) avec routage : existence de xr   (deduite de 5)
			//																	5) char i j k  l l' w existe  avec l proc de routing et l' proc de l'operation
			//																	6) n appartient a CH[l][l'][W] (remplace n directement dans la notation de xcbr. 
			//																									en faisant varier l et l' on couvre tout les n possibles )			
			// 
			//  for all n xcar[i][CH[l][l'][w][n]][j][k'] = chaar[i][j][k'][l][l'][w]
			// MODIF for all n xcar[i][m][j][k'] = (sum_l_l' / CH[l][l'][w][index] == nm)  chaar[i][j][k'][l][l'][w]
				
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {				
					for (int kk = 0; kk < data.rep; kk++) {
						for (int m = 0; m < data.nlinks; m++) {
							IloLinearNumExpr expr1 = cplex.linearNumExpr();
							expr1.addTerm(1, xcar[i][m][j][kk]);	
							for (int l = 0; l < data.nprocs; l++) {				
								for (int ll = 0; ll < data.nprocs; ll++) {		
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										if (data.CH.elementAt(l).elementAt(ll).elementAt(w).contains(m)){
											expr1.addTerm(-1, chaar[i][j][kk][l][ll][w]);
											//for (int n = 0; n < data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++) {
												//IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
												//expr1.addTerm(1, xcar[i][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n)][j][kk]);	
												//expr1.addTerm(-1, chaar[i][j][kk][l][ll][w]);
												//cplex.addEq(expr1, 0);	
											//}
										}
									}
								}
							}
							cplex.addEq(expr1, 0);	
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

			
			// Existence de tcbr (deduite de l'existence de xcbr here + precedence tasks coms later))
			//               tcbr[i][k][n][j] <= M*xcbr[i][k][n][j]

			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int n = 0; n < data.nlinks; n++) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, tcbr[i][k][n][j]);	
								expr1.addTerm(-data.Max, xcbr[i][k][n][j]);
								cplex.addLe(expr1, 0);		
							}
					}
				}
			}
			// Existence de tcar (deduite de l'existence des xcar here + precedence tasks coms later))
			//               tcar[i][n][j][k'] <= M*xcar[i][n][j][k']
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int n = 0; n < data.nlinks; n++) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, tcar[i][n][j][kk]);	
								expr1.addTerm(-data.Max, xcar[i][n][j][kk]);
								cplex.addLe(expr1, 0);		
							}
						}
					}
			}
			// Existence de Pr (deduite de l'existence xr)
			 // Pr[i][j] = sum_l  (l+1)*xr[i][j][l]
			  for (int j = 0; j < data.ntasks; j++) {
		      	 // for each replica jk 
		         for (int i = 0; i < data.ntasks; i++){		
		      		     IloLinearNumExpr expr = cplex.linearNumExpr(); 
		       			 for (int l = 0; l < data.nprocs; l++){
		   	 						expr.addTerm(l+1, xr[i][j][l]);
		   	 			 }
		   	 			 expr.addTerm(-1, Pr[i][j]);
					     //System.out.println("EXPR 6 = "+expr);
		   	 			 cplex.addEq(expr,0);
		       	   }
	          }

			// Existence de tr (deduite de l'existence de xcbr)
			//               tr[i][j] <= M*sum_l xr[i][j][l] 

			  for (int j = 0; j < data.ntasks; j++) {
		      	 // for each replica jk 
		         for (int i = 0; i < data.ntasks; i++){		
		      		     IloLinearNumExpr expr = cplex.linearNumExpr(); 
		       			 for (int l = 0; l < data.nprocs; l++){
		   	 						expr.addTerm(-data.Max, xr[i][j][l]);
		   	 			 }
		   	 			 expr.addTerm(1, tr[i][j]);
					     //System.out.println("EXPR 6 = "+expr);
		   	 			 cplex.addLe(expr,0);
		       	   }
	          }

			System.out.println("PRECEDENCE TASKS");
	
			/////////////////////////////////////////////////
			// PRECEDENCE TASKS
			/////////////////////////////////////////////////									
		
			//g-based precedence tache -> routing routing -> tache  : link  task starts with gtr grt
			//tr[i][j] - t[q][k] - M*gtr[q][k][i][j]>= sum_lm (Dql/fm)*x4[q][k][l][m] - M  (deactivate in case xrij or xqk does not exist : dedcued from zeroing gtr and grt)
			//t[q][k] - tr[i][j]  - M*grt[i][j][q][k]>=  - M 

			// for each tasks j
	         for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int j = 0; j < data.ntasks; j++){
	        	 	// j´ successor of jk	
	        	 	for (int q = 0; q < data.ntasks; q++) {
						// for each replica j´ḱ´ successor of jk
						for (int k = 0; k < data.rep; k++){
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(1,tr[i][j]); 	 				
							expr.addTerm(-1,t[q][k]);
							for (int l = 0; l < data.nprocs; l++){
								for (int m = 0; m < data.nfreqs; m++){
									expr.addTerm(-data.texes[data.nprocs*q+l]/data.pfreqs[m],x4[q][k][l][m]);
								}
							}
							expr.addTerm(-data.Max,gtr[q][k][i][j]);
							//  System.out.println("EXPR 2 = "+expr);
							cplex.addGe(expr,-data.Max);
						}
					}
				}	        		 
			}

			// for each tasks j
	         for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int j = 0; j < data.ntasks; j++){
	        	 	// j´ successor of jk	
	        	 	for (int q = 0; q < data.ntasks; q++) {
						// for each replica j´ḱ´ successor of jk
						for (int k = 0; k < data.rep; k++){
							IloLinearNumExpr expr = cplex.linearNumExpr(); 
							expr.addTerm(-1,tr[i][j]); 	 				
							expr.addTerm(1,t[q][k]);
							expr.addTerm(-data.Max,grt[i][j][q][k]);
							//  System.out.println("EXPR 2 = "+expr);
							cplex.addGe(expr,-data.Max);
						}
					}
				}	        		 
			}

			
			// a task preceeds its routing if any (gtr = 1 and grt = 1)
			// if j succ i : gtr[i][k][i][j]  + 2*M >= 1 + M*sum_l xr[i][j][l] + M*sum_l x3[i][k][l] (deactivated in no routing : deduced from deactivation if xrl does not exist)
		    // if j succ i : grt[i][j][j][k] + 2*M >= 1 + M*sum_l xr[i][j][l] + M*sum_l x3[j][k][l] (deactivated in no routing : deduced from deactivation if xrl does not exist)  
	
			// for each tasks j
	         for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int j = 0; j < data.ntasks; j++){
	        	 	// j´ successor of jk	
						// for each replica j´ḱ´ successor of jk
						if (data.succs[i][j] != 0) {
							for (int k = 0; k < data.rep; k++){
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
								expr1.addTerm(1,gtr[i][k][i][j]); 
								expr2.addTerm(1,grt[i][j][j][k]); 	 				
								for (int l = 0; l < data.nprocs; l++){
									expr1.addTerm(-data.Max,xr[i][j][l]);
									expr1.addTerm(-data.Max,x3[i][k][l]);
									expr2.addTerm(-data.Max,xr[i][j][l]);
									expr2.addTerm(-data.Max,x3[j][k][l]);
								}
								//  System.out.println("EXPR 2 = "+expr);
								cplex.addGe(expr1,1-2*data.Max);
								cplex.addGe(expr2,1-2*data.Max);
							}
						}
				}	        		 
			}
            
			//XXXXXXXXXXXX
			// daectivateformulas above if  xrl ij or x ikl does not exist 
			//XXXXXXXXXXX
			//  if  xrl ij or x ikl does not exist gtr and grt are zero
			//gtr[q][k][i][j] <= M*sum_l x[q][k][l]
			//gtr[q][k][i][j] <= M*sum_l xr[i][j][l]
			//grt[i][j][q][k] <= M*sum_l x[q][k][l]
			//grt[i][j][q][k] <= M*sum_l xr[i][j][l]

			// for each tasks j
	         for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int j = 0; j < data.ntasks; j++){
	        	 	// j´ successor of jk	
	        	 	for (int q = 0; q < data.ntasks; q++) {
						// for each replica j´ḱ´ successor of jk
						for (int k = 0; k < data.rep; k++){
							IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr4 = cplex.linearNumExpr(); 
							expr1.addTerm(1,gtr[q][k][i][j]);
							expr2.addTerm(1,gtr[q][k][i][j]);
							expr3.addTerm(1,grt[i][j][q][k]);
							expr4.addTerm(1,grt[i][j][q][k]);
							for (int l = 0; l < data.nprocs; l++){
									expr1.addTerm(-1,xr[i][j][l]);
									expr2.addTerm(-1,x3[q][k][l]);
									expr3.addTerm(-1,xr[i][j][l]);
									expr4.addTerm(-1,x3[q][k][l]);
							}

							//  System.out.println("EXPR 2 = "+expr);
							cplex.addLe(expr1,0);
							cplex.addLe(expr2,0);
							cplex.addLe(expr3,0);
							cplex.addLe(expr4,0);
						}
					}
				}	        		 
			}
			
			System.out.println("EXCLUSION TASKS");
			/////////////////////////////////////////////////
			// EXCLUSION TASKS
			/////////////////////////////////////////////////
			
			//vtr vrt (vrr is useless, don't need to declare it at all, because we don't have to define exl for zero exec time tasks)
			//             Pr[i][j] -  P[q][k]  - (nprocs+1)*vtr[q][k][i][j] >= -nprocs
			//             P[q][k] -  Pr[i][j]  - (nprocs+1)*vrt[i][j][q][k] >= -nprocs
			
			for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int j = 0; j < data.ntasks; j++){
	        	 	// for each task j´
	        	 	for (int q = 0; q < data.ntasks; q++) {
        	 			for (int k = 0; k < data.rep; k++){
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
								expr1.addTerm(1, Pr[i][j]); 	 				
								expr1.addTerm(-1, P[q][k]);
								expr1.addTerm(-data.nprocs -1 , vtr[q][k][i][j]);
								expr2.addTerm(-1, Pr[i][j]); 	 				
								expr2.addTerm(1, P[q][k]);
								expr2.addTerm(-data.nprocs -1 , vrt[i][j][q][k]);
								//System.out.println("EXPR 3 = "+expr);
								cplex.addGe(expr1,-data.nprocs);
								cplex.addGe(expr2,-data.nprocs);
	        	 	    }
	        	     }
				}
			}

			
			//XXXX
			//vtr vrt if ik or jkk  or ij does not exist
			//vtr[q][k][i][j] <= sum_l x[q][k][l]
			//vtr[q][k][i][j] <= sum_l xr[i][j][l]
			//vrt[i][j][q][k] <= sum_l x[q][k][l]
			//vrt[i][j][q][k] <= sum_l xr[i][j][l]
			
			for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int j = 0; j < data.ntasks; j++){
	        	 	// j´ successor of jk	
	        	 	for (int q = 0; q < data.ntasks; q++) {
						// for each replica j´ḱ´ successor of jk
						for (int k = 0; k < data.rep; k++){
							IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
							IloLinearNumExpr expr4 = cplex.linearNumExpr(); 
							expr1.addTerm(1,vtr[q][k][i][j]);
							expr2.addTerm(1,vtr[q][k][i][j]);
							expr3.addTerm(1,vrt[i][j][q][k]);
							expr4.addTerm(1,vrt[i][j][q][k]);
							for (int l = 0; l < data.nprocs; l++){
									expr1.addTerm(-1,xr[i][j][l]);
									expr2.addTerm(-1,x3[q][k][l]);
									expr3.addTerm(-1,xr[i][j][l]);
									expr4.addTerm(-1,x3[q][k][l]);
							}

							//  System.out.println("EXPR 2 = "+expr);
							cplex.addLe(expr1,0);
							cplex.addLe(expr2,0);
							cplex.addLe(expr3,0);
							cplex.addLe(expr4,0);
						}
					}
				}	        		 
			}


			//XXXX
			// excl between  tache ^ routing : link gtr grt vtr vrt
			// deactivate constraints in case ik or ii jj do not exists 
			//gtr[i][k][ii][jj] + grt[ii][jj][i][k]  + vtr[i][k][ii][jj] + vrt[ii][jj][i][k] -M* sum_l x[i][k][l] -M* sum_l xr[ii][jj][l] >= 1 - 2*M

			 for (int i = 0; i < data.ntasks; i++) {
	        	 // for each replica jk 
	        	 for (int k = 0; k < data.rep; k++){
	        	 	// for each task j´
					for (int ii = 0; ii < data.ntasks; ii++){
						for (int jj = 0; jj < data.ntasks; jj++) {
        	 						IloLinearNumExpr expr = cplex.linearNumExpr(); 
        	 						expr.addTerm(1, gtr[i][k][ii][jj]); 	 				
        	 						expr.addTerm(1, grt[ii][jj][i][k]);
        	 						expr.addTerm(1, vtr[i][k][ii][jj]); 	 				
        	 						expr.addTerm(1, vrt[ii][jj][i][k]);
									for (int l = 0; l< data.nprocs; l++){
										expr.addTerm(-data.Max, x3[i][k][l]);
										expr.addTerm(-data.Max, xr[ii][jj][l]);
									}

        	 					    // System.out.println("EXPR 4 = "+expr);
        	 						cplex.addGe(expr,1 - 2*data.Max);
	        	 	    }
	        	     }
	               }
	           }

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
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int l = 0; l < data.nprocs; l++) {				
							for (int ll = 0; ll < data.nprocs; ll++) {		
								if ( data.succs[i][j] !=0 && l != ll) {
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										for (int n = 0; n+1 <data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++) {
											IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
											expr1.addTerm(1, tcbr[i][k][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n+1)][j]);	
											expr1.addTerm(-data.Max, chbr[i][k][j][l][ll][w]);
											expr1.addTerm(-1, tcbr[i][k][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n)][j]);	
											cplex.addGe(expr1, -data.Max);	
										}
									}
								}
							}
						}
					}
				}
			}	
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {				
					for (int kk = 0; kk < data.rep; kk++) {
						for (int l = 0; l < data.nprocs; l++) {				
							for (int ll = 0; ll < data.nprocs; ll++) {		
								if ( data.succs[i][j] !=0 && l != ll) {
									for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
										for (int n = 0; n+1 < data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++) {
											IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
											expr1.addTerm(1, tcar[i][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n+1)][j][kk]);	
											expr1.addTerm(-data.Max, chaar[i][j][kk][l][ll][w]);
											expr1.addTerm(-1, tcar[i][data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n)][j][kk]);	
											cplex.addGe(expr1, -data.Max);	
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
		//2	
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tcar[ii][nn][jj][kkkk]);	
													expr1.addTerm(-1, tc[i][k][n][j][kk]);
													expr1.addTerm(-data.Max, gccar[i][k][n][j][kk][ii][nn][jj][kkkk] );
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
		//3
		for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tcbr[ii][kkk][nn][jj]);	
													expr1.addTerm(-1, tc[i][k][n][j][kk]);
													expr1.addTerm(-data.Max, gccbr[i][k][n][j][kk][ii][kkk][nn][jj] );
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
			//4
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tc[ii][kkk][nn][jj][kkkk]);	
													expr1.addTerm(-1, tcbr[i][k][n][j]);
													expr1.addTerm(-data.Max, gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] );
													expr1.addTerm(-data.tcoms[i][j], xcbr[i][k][n][j] );	
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
			
			//5
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tcbr[ii][kkk][nn][jj]);	
													expr1.addTerm(-1, tcbr[i][k][n][j]);
													expr1.addTerm(-data.Max, gcbrcbr[i][k][n][j][ii][kkk][nn][jj] );
													expr1.addTerm(-data.tcoms[i][j], xcbr[i][k][n][j] );	
													cplex.addGe(expr1, -data.Max);	
												}	
											}
									}
								}
							}
					}
				}
			}	
			
			//6
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tcar[ii][nn][jj][kkkk]);	
													expr1.addTerm(-1, tcbr[i][k][n][j]);
													expr1.addTerm(-data.Max, gcbrcar[i][k][n][j][ii][nn][jj][kkkk] );
													expr1.addTerm(-data.tcoms[i][j], xcbr[i][k][n][j] );	
													cplex.addGe(expr1, -data.Max);	
												}	
											}
										}
									}
							}
					}
				}
			}	

			//7
			for (int i = 0; i < data.ntasks; i++) {
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
													expr1.addTerm(-1, tcar[i][n][j][kk]);
													expr1.addTerm(-data.Max, gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] );
													expr1.addTerm(-data.tcoms[i][j], xcar[i][n][j][kk] );	
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
			
			//8
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tcbr[ii][kkk][nn][jj]);	
													expr1.addTerm(-1, tcar[i][n][j][kk]);
													expr1.addTerm(-data.Max, gcarcbr[i][n][j][kk][ii][kkk][nn][jj] );
													expr1.addTerm(-data.tcoms[i][j], xcar[i][n][j][kk] );	
													cplex.addGe(expr1, -data.Max);	
												}	
											}
									}
								}
							}
						}
					}
			}	
			
			//9
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, tcar[ii][nn][jj][kkkk]);	
													expr1.addTerm(-1, tcar[i][n][j][kk]);
													expr1.addTerm(-data.Max, gcarcar[i][n][j][kk][ii][nn][jj][kkkk] );
													expr1.addTerm(-data.tcoms[i][j], xcar[i][n][j][kk] );	
													cplex.addGe(expr1, -data.Max);	
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
				
				//2
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gccar[i][k][n][j][kk][ii][n][jj][kkkk] );	
													expr1.addTerm(1, gcarc[ii][n][jj][kkkk][i][k][n][j][kk] );
													expr1.addTerm(-data.Max,  xc[i][k][n][j][kk] );
													expr1.addTerm(-data.Max,  xcar[ii][n][jj][kkkk] );
													cplex.addGe(expr1, 1-2*data.Max);	
												}	
											}
										}
									}
							}
						}
					}
				}
					
				//3
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gccbr[i][k][n][j][kk][ii][kkk][n][jj] );	
													expr1.addTerm(1, gcbrc[ii][kkk][n][jj][i][k][n][j][kk] );
													expr1.addTerm(-data.Max,  xc[i][k][n][j][kk] );
													expr1.addTerm(-data.Max,  xcbr[ii][kkk][n][jj] );
													cplex.addGe(expr1, 1-2*data.Max);	
												}	
										}
									}
								}
							}
						}
					}
				}	
				
				//4
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {	
										if (i != ii || j != jj || k !=  kkk){  
											for (int n = 0; n < data.nlinks; n++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcbrcbr[i][k][n][j][ii][kkk][n][jj] );	
													expr1.addTerm(1, gcbrcbr[ii][kkk][n][jj][i][k][n][j] );
													expr1.addTerm(-data.Max,  xcbr[i][k][n][j] );
													expr1.addTerm(-data.Max,  xcbr[ii][kkk][n][jj] );
													cplex.addGe(expr1, 1-2*data.Max);	
												}	
											}
										}
									}
								}
						}
					}
				}
				//5
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcbrcar[i][k][n][j][ii][n][jj][kkkk] );	
													expr1.addTerm(1, gcarcbr[ii][n][jj][kkkk][i][k][n][j] );
													expr1.addTerm(-data.Max,  xcbr[i][k][n][j] );
													expr1.addTerm(-data.Max,  xcar[ii][n][jj][kkkk] );
													cplex.addGe(expr1, 1-2*data.Max);	
												}	
											}
										}
								}
						}
					}
				}
				//6
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											if (i != ii || j != jj  || kk != kkkk){  
											for (int n = 0; n < data.nlinks; n++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcarcar[i][n][j][kk][ii][n][jj][kkkk] );	
													expr1.addTerm(1, gcarcar[ii][n][jj][kkkk][i][n][j][kk] );
													expr1.addTerm(-data.Max,  xcar[i][n][j][kk] );
													expr1.addTerm(-data.Max,  xcar[ii][n][jj][kkkk] );
													cplex.addGe(expr1, 1-2*data.Max);	
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
			
				//3 4
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gccar[i][k][n][j][kk][ii][nn][jj][kkkk] );
													expr2.addTerm(1, gccar[i][k][n][j][kk][ii][nn][jj][kkkk] );	
													expr1.addTerm(-1,  xc[i][k][n][j][kk] );
													expr2.addTerm(-1,  xcar[ii][nn][jj][kkkk] );
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

	//5 6
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gccbr[i][k][n][j][kk][ii][kkk][nn][jj] );
													expr2.addTerm(1, gccbr[i][k][n][j][kk][ii][kkk][nn][jj] );	
													expr1.addTerm(-1,  xc[i][k][n][j][kk] );
													expr2.addTerm(-1,  xcbr[ii][kkk][nn][jj] );
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

	//7 8
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] );
													expr2.addTerm(1, gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] );	
													expr1.addTerm(-1,  xcbr[i][k][n][j] );
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
	//9 10
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcbrcbr[i][k][n][j][ii][kkk][nn][jj] );
													expr2.addTerm(1, gcbrcbr[i][k][n][j][ii][kkk][nn][jj] );	
													expr1.addTerm(-1,  xcbr[i][k][n][j] );
													expr2.addTerm(-1,  xcbr[ii][kkk][nn][jj] );
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

	//11 12
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcbrcar[i][k][n][j][ii][nn][jj][kkkk] );
													expr2.addTerm(1, gcbrcar[i][k][n][j][ii][nn][jj][kkkk] );	
													expr1.addTerm(-1,  xcbr[i][k][n][j] );
													expr2.addTerm(-1,  xcar[ii][nn][jj][kkkk] );
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

		//13 14
			for (int i = 0; i < data.ntasks; i++) {
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
													expr1.addTerm(1, gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] );
													expr2.addTerm(1, gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] );	
													expr1.addTerm(-1,  xcar[i][n][j][kk] );
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

	//15 16
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcarcbr[i][n][j][kk][ii][kkk][nn][jj] );
													expr2.addTerm(1, gcarcbr[i][n][j][kk][ii][kkk][nn][jj] );	
													expr1.addTerm(-1,  xcar[i][n][j][kk] );
													expr2.addTerm(-1,  xcbr[ii][kkk][nn][jj] );
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

	//17 18
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
													IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
													expr1.addTerm(1, gcarcar[i][n][j][kk][ii][nn][jj][kkkk] );
													expr2.addTerm(1, gcarcar[i][n][j][kk][ii][nn][jj][kkkk] );	
													expr1.addTerm(-1,  xcar[i][n][j][kk] );
													expr2.addTerm(-1,  xcar[ii][nn][jj][kkkk] );
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

			System.out.println("---> PRECEDENCE TASKS COMS ...");

			/////////////////////////////////////////////////
			// PRECEDENCE TASKS COMS
			/////////////////////////////////////////////////

			//XXXX
			//precedences  tache -> com -> tache  tache -> com -> routing  routing -> com -> tache
			// note: t[j][kk] - t[i][k] - M*g[i][k][j][kk]>= sum_lm (Dil/fm)*x4[i][k][l][m] - M 
			//      remove the formula above earlier in the code ?
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

			//1
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
										expr1.addTerm(-data.texes[data.nprocs*i+l]/data.pfreqs[m],x4[i][k][l][m]);
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
			
			//3
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int n = 0; n < data.nlinks; n++) {
							  if (data.succs[i][j] !=0) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, tcbr[i][k][n][j]);
								expr1.addTerm(-1, t[i][k]);
								expr1.addTerm(-data.Max, xcbr[i][k][n][j] );	
								for (int l = 0; l < data.nprocs; l++){
									for (int m = 0; m < data.nfreqs; m++){
										expr1.addTerm(-data.texes[data.nprocs*i+l]/data.pfreqs[m],x4[i][k][l][m]);
									}
								}
								cplex.addGe(expr1, -data.Max);
							 }		
							}
					}
				}
			}

			//4
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int n = 0; n < data.nlinks; n++) {
								if (data.succs[i][j] !=0) {
									IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
									expr1.addTerm(-1, tcbr[i][k][n][j]);
									expr1.addTerm(1, tr[i][j]);
									expr1.addTerm(-data.Max, xcbr[i][k][n][j] );	
									expr1.addTerm(-data.tcoms[i][j],xcbr[i][k][n][j]);
									cplex.addGe(expr1, -data.Max);
								}	
							}
					}
				}
			}	
			
				//5
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int n = 0; n < data.nlinks; n++) {
							  if (data.succs[i][j] !=0) {
								IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
								expr1.addTerm(1, tcar[i][n][j][kk]);
								expr1.addTerm(-1, tr[i][j]);
								expr1.addTerm(-data.Max, xcar[i][n][j][kk] );	
								cplex.addGe(expr1, -data.Max);
							 }		
							}
						}
					}
			}
			
			//6
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int n = 0; n < data.nlinks; n++) {
								if (data.succs[i][j] !=0) {
									IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
									expr1.addTerm(-1, tcar[i][n][j][kk]);
									expr1.addTerm(1, t[j][kk]);
									expr1.addTerm(-data.Max, xcar[i][n][j][kk] );	
									expr1.addTerm(-data.tcoms[i][j],xcar[i][n][j][kk]);
									cplex.addGe(expr1, -data.Max);
								}	
							}
						}
					}
			}	
			
	
	
		
		/*	NXXXX : logRelCOC : (ii) routing1 -> com -> task i -> com (j)   (ii) routing -> com -> task i -> com (j) ... (ii) routingn -> com -> task i -> com (j) 
                               or  task i -> com (j)   (ii) routing1 -> com -> task i -> com (j) ... (ii) routingn -> com -> task i -> com (j) ... idem en variant les succs j
							   or (ii) routing1 -> com -> task i -> com (j)   task i -> com (j) ... (ii) routingn -> com -> task i -> com (j) ... idem en variant les succs j
							   ...
							   ... idem en variant les succs j
							   ... idem en variant les succs routing
			 => il faut des variables S4 S5 S6 S7 i K a d énumérant les differentes combinaisons possibles de predecesseurs routants ou non de successeurs routant ou non et leurs mappings  	
			 
			  Vector<Vector<Vector<int>>> S4,S6
			  Vector<Vector<Vector<Vector<int>>>> S5,S7
			  
			  S4 i e  = (1,-1,-1,0,-1,0) : O est pred routant et -1 signifie n est pas pred  k<=K-1. non pas de k car meme predecesseurs pour tout replica (idem succs)
			  S5 i f = (2,-1,-1,-1,-1,-1) : les procs des pred = S1 1 a
			  S6 i g = (-1,1,-1,-1,0,-1) : O et 1 sont succs routants et -1 signifie n est pas succ  k<=K-1
			  S7 i h = (-1,4,-1,-1,2,-1) : les procs des succs = S1 2 a
			  NXXXX : pour k>= 1 S4 i K f k ne contient que des routants et pareil pour S6. non pas de k car meme predecesseurs pour tout replica (idem succs)
			  
			  +logRelPOS : obligatoire pour faire la somme sur les fiabilite des operations 
			 Pour toute tache i 
				Pour toute combinaison de predecesseurs routants (routants uniquement) S8 i e
					Pour toute combinaison de proc des predecesseurs (routants uniquement) S8 i e f 
						Pour tout nombre des replicas de i  S8 i e f K
							Pour toute combinaison de proc des replicas  S8 i e f K a
								Pour toute combinaison de frequences des replicas   S2 K b
									Pour toute combinaison des successeurs (routants ou non) S8 i e f K a g
										Pour toute combinaison de proc des successeurs (routants ou non) S8 i e f K a  g h
											pour toute combinaison de chemins o entre les procs des predecesseurs routants et les procs des K replica et les procs des successeurs S8 i e f K a g h o  
												F = 1;
												pour tout replica k de i
													extraire la sous-combinaison ok de chemins entre les procs des predecesseurs routants et le proc du replica k de i et les procs des successeurs S8 i e f K a g h o k  
													Rch = 1
													pour tout chemin w de la sous-combinaison ok (taille de ok = nb des procs de f + nb des procs h)
														Pour tout lien n de w
															calculer Rch = Rch*Rn
														fin
													fin	
													calculer R = Rexe*Rch ou Rexe tient compte de la frequence S2[b][k]
													F = F*(1 - R) 
												fin
												-Var*(1-F)	// Var active (vaut 1)  si (1) la combinaison S8 effective et (2) la combinations S2 K b effective càd Var = VarefKabgho (y compris b)
											fin	
										fin
									fin			
								fin			
							fin				
						fin		
					fin
				fin //replica
			fin
			= 0
		*/	
		//	logRelPOS 
		/*		XXXX : MODIF 1 : logRelOS = logRcom valable uniquement pour les taches sans predecesseurs et avec sucesseurs !!! NO
				XXXX :	MODIF 2 : logRelO = logRexe valable uniquement pour les taches sans predecesseurs et sans sucesseurs !!! NO	 
						RAISON  : logRelPOS = COC ci-dessous gere deja les taches avec predecesseurs (meme sans routants) et avec successeurs  !!! NO
				XXXX : MODIF3 :  logRelPO devrait gerer les taches avec predecesseurs (meme sans routants) et SANS successeurs !!! NO
				XXXX : NO NO NO NO logRelPOS pourra gerer touts les cas en inserant 4 vecteurs e f g h =(-1 ... -1) dans le cas sans preds, sans preds, sans succs, sans succs respectivement dans S4 S5 S6 S7
						=> modifier S4 S5 S6 S7
		*/
			{ //ismail : removed temporarely
				IloLinearNumExpr expr = cplex.linearNumExpr(); 
				expr.addTerm(-1,logRelPOS);

			for (int i=0; i<data.ntasks; i++){
				//	IloLinearNumExpr exprT = cplex.linearNumExpr(); 
				//	exprT.addTerm(-1,logRelPOST[i]);
					for (int e =0; e< S8.elementAt(i).size(); e++){
						for (int f=0; f < S8.elementAt(i).elementAt(e).size(); f++){
							for (int K=1; K<= data.rep; K++){ 
								for (int a=0; a< Math.pow(data.nprocs, K); a++){
									for (int b=0; b< Math.pow(data.nfreqs,K);b++){	
										for (int x =0; x< S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).size(); x++){
											for (int h =0; h<S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).size(); h++){
												for (int o=0; o <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size(); o++){
												    assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).size() == K);
													double  F = 1;
													for (int k=0; k< K; k++){
														double Rch = 1;
														Vector<Integer> ok = S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).elementAt(k);
														int K2 = trouver_nb_routants_procs(data,i,S5,e,f);	assert(K2 == trouver_nb_routants(data,i,S4,e));
														int K3 = trouver_nb_successeurs_procs(data,i,S7,x,h);	assert(K3 == trouver_nb_successeurs(data,i)); assert(K3 == trouver_nb_successeurs(data,i,S6,x));
														assert(ok.size() == K2+K3);
														for (int c=0 ; c < ok.size(); c++){
															int w = ok.elementAt(c);
															if (w != -1) {
																int l = -1;
																int ll = -1;
																double tcom = -1;
																if (c < K2){
																	l = trouver_routant_proc(data, i, S5, e, f, c+1); // le "c+1" eme proc dans la combinaison f
																	int jj = trouver_routant(data, i, S4, e, c+1);// le "c+1" eme predecesseur routant dans la combinaison e
																	ll = S1[a][k];
																	tcom = data.tcoms[jj][i];
																} else if (c-K2 < K3 ){
																	l = S1[a][k];
																	ll = trouver_successeur_proc(data, i, S7, x, h, c-K2+1); // le "c-K1+1" eme proc dans la combinaison h
																	int j = trouver_successeur(data, i, S6, x, c-K2+1); // le "c-K1+1" eme successeur  dans la combinaison h
																	assert(j==trouver_successeur(data, i, c-K2+1));// le "c-K1+1" eme successeur dans la data.succs[i]
																	tcom = data.tcoms[i][j];
																} else 
																	assert(false);
																//System.out.println("K K2  K3  c " + K + " " +K2 + " " + K3+ " " +  c );	
															//	System.out.println("l ll  data.CH.elementAt(l).elementAt(ll).size() w" + " " +l + " " + ll+ " " +  data.CH.elementAt(l).elementAt(ll).size() + " " + w);	
																assert(	w < data.CH.elementAt(l).elementAt(ll).size() );
																for (int n= 0; n <data.CH.elementAt(l).elementAt(ll).elementAt(w).size(); n++){ 
																	Rch = Rch*Math.exp(-data.llambdas[data.CH.elementAt(l).elementAt(ll).elementAt(w).elementAt(n)]*tcom);
																}
															}
														}														
														F = F*(1-Rch*Math.exp(-data.plambdas[S1[a][k]]*Math.pow(10,(1 - data.pfreqs[S2[b][k]])/(1 - data.pfreqs[0]))*data.texes[data.nprocs*i+S1[a][k]]/data.pfreqs[S2[b][k]]));
													}
													//exprT.addTerm(Math.log(1-F),S8S2[i][e][f][K][a][b][x][h][o]);
													expr.addTerm(Math.log(1-F),S8S2[i][e][f][K][a][b][x][h][o]);
												}		
											}
										}
									}
								}
							}
						}
					}
				//cplex.addEq(exprT,0);
				}				
				cplex.addEq(expr,0);
			
			} // end
			
			
		//NXXXXX : declarer les variables S8S2[i][e][f][K][a][b][g][h][o] et leurs  contraintes de linearisation	
			/*	S8S2[i][e][f][K][a][b][g][h][o] =  (for all predecesseur routant jj : R[jj][i])* (for all predecesseur non routant jj : 1-R[jj][i]) * (for all routant proc l of jj : xr jj i l)* (XYiKab)
												  *(for all successeur routant j : R[i][j])* (for all successeur non routant j : 1-R[i][j])* (for all successeur routant proc ll xr i j ll)*(for all successeur non routant proc ll x j 0 ll)
												  *(for all replica i k for all chemin w entre l le proc du routant et ll le proc du replica : chaar[jj][i][k][l][ll][w])
												  *(for all replica i k for all chemin w entre l le proc du replica et ll le proc du successeur routant : chbr[i][k][j][l][ll][w])
												  *(for all replica i k for all chemin w entre l le proc du replica et ll le proc du successeur non routant : ch[i][k][j][0][l][ll][w])
			*/
			{ //??????????????????????????????? MANQUE : les contraintes disant que S8S2 est zero si les freq S2 b k ne sont pas utilisés pour les replicas <----------- DONE
			  //???????????????????????????????          ET les contraintes disant que S8S2 doit etre 1 si les freq S2 bk utilisés en plus des autres conditions deja prises en comptes ci-dessous <----------- DONE
			  //???????????????????????????????          ET idem pour les S1 a k donnant les processeurs sur lesquels sont mappés les k replicas  <----------- DONE
			  //??????????????????????????????? MANQUE : sum S8S2 i pour une op donnée est EGALE à 1 <------ DONE
			  //??????????????????????????????? MANQUE : sum S8S2 i e est INFEGALE 1 NON car DEDUIT de la contrainte ci-dessus
			  //??????????????????????????????? MANQUE : sum S8S2 i e f est INFEGALE 1 NON car DEDUIT
				for (int i=0; i<data.ntasks; i++){
					// sum S8S2 = 1
					// ismail : tempo  ???????? POURQUOI LA SOMME N'EST PAS EGALE A 1 ??????
					IloLinearNumExpr expr = cplex.linearNumExpr(); 
					for (int e =0; e< S8.elementAt(i).size(); e++){
						for (int f=0; f < S8.elementAt(i).elementAt(e).size(); f++){
							for (int K=1; K<= data.rep; K++){ 
								for (int a=0; a< Math.pow(data.nprocs, K); a++){
									for (int b=0; b< Math.pow(data.nfreqs,K);b++){	
										for (int x =0; x< S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).size(); x++){
											for (int h =0; h<S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).size(); h++){
												for (int o=0; o <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size(); o++){ 
													//for (int o=0; o <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size() && o<1; o++){ //?????????????? c est temporaire
													// ismail tempo : 
													expr.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
													IloLinearNumExpr expr12 = cplex.linearNumExpr(); 
													expr12.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
													int TOTAL = 0;
													assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).size() == K);
													for (int jj = 0; jj < S4.elementAt(i).elementAt(e).size(); jj++){ 
														if (S4.elementAt(i).elementAt(e).elementAt(jj) == 1){ //pred routant
															IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
															expr1.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr1.addTerm(-1,R[jj][i]); 
															cplex.addLe(expr1,0);
															int l =S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj);
															assert(l >= 0 && l < data.nprocs); //pred routant proc
															IloLinearNumExpr expr3 = cplex.linearNumExpr(); 
															expr3.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr3.addTerm(-1,xr[jj][i][l]); 
															cplex.addLe(expr3,0);
															expr12.addTerm(-1,R[jj][i]); 
															expr12.addTerm(-1,xr[jj][i][l]); 
															TOTAL = TOTAL+1;
															TOTAL = TOTAL+1;
														}else if (S4.elementAt(i).elementAt(e).elementAt(jj) == 0){ //pred pas routant  // ????????????????? optimize : in that case S8S2 is zero for all K>= 2
															IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
															expr2.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr2.addTerm(1,R[jj][i]); 
															cplex.addLe(expr2,1);
															int l =S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj);
															assert(l == -1); 
															expr12.addTerm(1,R[jj][i]); 
															TOTAL = TOTAL+1;
															TOTAL = TOTAL-1;
															if (K>=2) zerovar(cplex,S8S2[i][e][f][K][a][b][x][h][o]); //assert(false) ; // pas de K>= 2  et pred pas routant car ce cas de combinaison n'arrivera jamais
														}else {
															assert(S4.elementAt(i).elementAt(e).elementAt(jj) == -1);
															int l =S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj);
															assert(l == -1); 
															assert(data.succs[jj][i] == 0);
														}
													}
													
													if (!has_preds(data,i)){
														assert(S4.elementAt(i).size() == 1);
													    for (int jj = 0; jj < S4.elementAt(i).elementAt(0).size(); jj++) 
															assert(S4.elementAt(i).elementAt(0).elementAt(jj) == -1);
													}

													IloLinearNumExpr expr4 = cplex.linearNumExpr(); 
													expr4.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
													expr4.addTerm(-1,XY[i][K][a][b]); 
													cplex.addLe(expr4,0);
													expr12.addTerm(-1,XY[i][K][a][b]); 
													TOTAL = TOTAL+1;
													
													for (int j = 0; j < S6.elementAt(i).elementAt(x).size(); j++){ 
														if (S6.elementAt(i).elementAt(x).elementAt(j) == 1){ //succ routant
															IloLinearNumExpr expr5 = cplex.linearNumExpr(); 
															expr5.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr5.addTerm(-1,R[i][j]); 
															cplex.addLe(expr5,0);
															int ll =S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j);
															assert(ll >= 0 && ll < data.nprocs); //pred routant proc
															IloLinearNumExpr expr7 = cplex.linearNumExpr(); 
															expr7.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr7.addTerm(-1,xr[i][j][ll]); 
															cplex.addLe(expr7,0);
															expr12.addTerm(-1,R[i][j]); 
															expr12.addTerm(-1,xr[i][j][ll]); 
															TOTAL = TOTAL+1;
															TOTAL = TOTAL+1;
														}else if (S6.elementAt(i).elementAt(x).elementAt(j) == 0){ //succ pas routant // ???????????? optimize : in that case S8S2 is zero for all K>= 2
															IloLinearNumExpr expr6 = cplex.linearNumExpr(); 
															expr6.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr6.addTerm(1,R[i][j]); 
															cplex.addLe(expr6,1);
															int ll =S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j);
															assert(ll >= 0 && ll < data.nprocs); //succ pas routant proc
															IloLinearNumExpr expr8 = cplex.linearNumExpr(); 
															expr8.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
															expr8.addTerm(-1,x3[j][0][ll]); 
															cplex.addLe(expr8,0);
															expr12.addTerm(1,R[i][j]); 
															expr12.addTerm(-1,x3[j][0][ll]); 
															TOTAL = TOTAL+1;
															TOTAL = TOTAL-1;
															TOTAL = TOTAL+1;
															if (K>=2) zerovar(cplex,S8S2[i][e][f][K][a][b][x][h][o]); //assert(false) ; // pas de K>= 2  et succ pas routant car ce cas de combinaison n'arrivera jamais
														}else {
															
															assert(S6.elementAt(i).elementAt(x).elementAt(j) == -1);
															int ll =S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j);
															assert(ll == -1); 
															assert(data.succs[i][j] == 0);

														}
													}
													if (!has_succs(data,i)) {
														assert(S6.elementAt(i).size() == 1);
														for (int j = 0; j < S6.elementAt(i).elementAt(0).size(); j++) 
															assert(S6.elementAt(i).elementAt(0).elementAt(j) == -1);
													}													
													
													
																	
													for (int k=0; k< K; k++){
														//S8S2 <= x3 i k S1 a k  // si les replicas de i n'utilisent pas les procs s1 a alors S8S2 est 0
														//S8S2 <= x4 i k S1 a k S2 b k // si les replicas de u n'utilisent pas les freq S2 b alors S8S2 est 0
														//TOTAL ++ TOTAL ++
														
													
													//    ismail : tempo 
													// USELESS becasue w e already have S8S2 <= XYiKab and  S8S2 - XYKiab .. >= ...
													/*	IloLinearNumExpr exprS1S2 = cplex.linearNumExpr(); 
														exprS1S2.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
														exprS1S2.addTerm(-1,x4[i][k][S1[a][k]][S2[b][k]]); 
														cplex.addLe(exprS1S2, 0);
														expr12.addTerm(-1,x4[i][k][S1[a][k]][S2[b][k]]); 
														TOTAL=TOTAL+1;
														*/
														Vector<Integer> ok = S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).elementAt(k);
														int K2 = trouver_nb_routants_procs(data,i,S5,e,f);	assert(K2 == trouver_nb_routants(data,i,S4,e));
														int K3 = trouver_nb_successeurs_procs(data,i,S7,x,h);	assert(K3 == trouver_nb_successeurs(data,i)); assert(K3 == trouver_nb_successeurs(data,i,S6,x));
														assert(ok.size() == K2+K3);
														
														for (int c=0 ; c < ok.size(); c++){
															int w = ok.elementAt(c);
															if (w != -1) {
																int l = -1;
																int ll = -1;
																if (c < K2){
																	l = trouver_routant_proc(data, i, S5, e, f, c+1); // le "c+1" eme proc dans la combinaison f
																	int jj = trouver_routant(data, i, S4, e, c+1);// le "c+1" eme predecesseur routant dans la combinaison e
																	ll = S1[a][k];
																	//expr9 : chaar jj i k l ll w
																	IloLinearNumExpr expr9 = cplex.linearNumExpr(); 
																	expr9.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
																	expr9.addTerm(-1,chaar[jj][i][k][l][ll][w]); 
																	cplex.addLe(expr9,0);
																	expr12.addTerm(-1,chaar[jj][i][k][l][ll][w]); 
																	TOTAL = TOTAL+1;
																} else if (c-K2 < K3 ){
																
																	l = S1[a][k];
																	ll = trouver_successeur_proc(data, i, S7, x, h, c-K2+1); // le "c-K1+1" eme proc dans la combinaison h
																	int j = trouver_successeur(data, i, S6, x, c-K2+1); // le "c-K1+1" eme successeur  dans la combinaison h
																	assert(j==trouver_successeur(data, i, c-K2+1));// le "c-K1+1" eme successeur dans la data.succs[i]
																	if (S6.elementAt(i).elementAt(x).elementAt(j) == 1){ //succ routant  expr10 chbr i k j l ll w 
																		IloLinearNumExpr expr10 = cplex.linearNumExpr(); 
																		expr10.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
																		expr10.addTerm(-1,chbr[i][k][j][l][ll][w]); 
																		cplex.addLe(expr10,0);	
																		expr12.addTerm(-1,chbr[i][k][j][l][ll][w]); 
																		TOTAL = TOTAL+1;
																	}else if  (S6.elementAt(i).elementAt(x).elementAt(j) == 0){ // succ pas routant  expr11 ch ik j 0 l ll w
																		//assert(K >= 2);  ////////////// ????????????????? on ne peut avoir i k alors qu' il n y a de pas de routage entre i et j !!!!!!!!!!!!!!!!!!!
																		IloLinearNumExpr expr11 = cplex.linearNumExpr(); 
																		expr11.addTerm(1,S8S2[i][e][f][K][a][b][x][h][o]); 
																		expr11.addTerm(-1,ch[i][k][j][0][l][ll][w]); 
																		cplex.addLe(expr11,0);
																		expr12.addTerm(-1,ch[i][k][j][0][l][ll][w]); 
																		TOTAL = TOTAL+1;
																		if (K>=2) zerovar(cplex,S8S2[i][e][f][K][a][b][x][h][o]); //assert(false) ; // pas de K>= 2  et succ pas routant car ce cas de combinaison n'arrivera jamais
																								  // because we know already that S8S2[i][e][f][K][a][b][x][h][o] will ALWAYS be 0 and thus useless for all K>=2 if succ pas routant

																	}
																	else assert(false);
																
																} else 
																	assert(false);																																
															}
														}
													}
																									
		
																	

													//expr12 S8S2  >= 1 + sum (...) - TOTAL
												//	System.out.println("S8S2 : TOTAL = " + TOTAL);
												//	System.out.println("S8S2 : expr12 = " + expr12);
												cplex.addGe(expr12,1 - TOTAL);
													
												 }		
											}
										}
									}
								}
							}
						}
					}
					// ismail tempo : 
				cplex.addEq(expr,1); 
				}				
			} // end
			
	
			//----------------------------------------------------
			// logR
			
    		{
    		    IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
				IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
				expr1.addTerm(-1,logR);
			/*	expr1.addTerm(1,logRexe); no
				expr1.addTerm(1,logRcom); no
				expr1.addTerm(1,logRcombr); no
				expr1.addTerm(1,logRcomar); no */
				expr1.addTerm(1,logRelPOS);
			//	for (int i=0; i<data.ntasks; i++){
			//		expr1.addTerm(1,logRelPOST[i]);
			//	}

				cplex.addEq(expr1,0);
				
				
			  
			  //  ismail : tempo
				expr2.addTerm(-1,logR);
				expr2.addTerm(-data.lo,Sum);
				expr2.addTerm(-data.lo,Encom);
				expr2.addTerm(-data.lo,Encombr);
				expr2.addTerm(-data.lo,Encomar);
                cplex.addLe(expr2,0);
			   
			  // ismail tempraire :  
			/*   for (int i=0; i<data.ntasks; i++){
				IloLinearNumExpr expr2 = cplex.linearNumExpr(); 
				expr2.addTerm(-1,logRelPOST[i]);
				expr2.addTerm(-data.lo,SumT[i]);
				expr2.addTerm(-data.lo,EncomT[i]);
				expr2.addTerm(-data.lo,EncombrT[i]);
				expr2.addTerm(-data.lo,EncomarT[i]);
				cplex.addLe(expr2,0);
			   } */
			   
              
        	}
        		
        /*  {
			for (int j = 0; j < data.ntasks; j++) {
    		    	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
					expr1.addTerm(1,Y[j][1]);
        		    cplex.addEq(expr1,1);
				}
			} */
			
			{
    		    //	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
				//	expr1.addTerm(1,W);
        		  //  cplex.addGe(expr1,52.3300);
			} 
			{
    		    	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
					expr1.addTerm(1,W);
					//cplex.addGe(expr1,  borneInfL4fois10emoins5[p2-5]);
					cplex.addGe(expr1,  borneinf_previousline[p2-5]);
			}
			{
    		     	IloLinearNumExpr expr1 = cplex.linearNumExpr(); 
					expr1.addTerm(1,W);
					cplex.addLe(expr1,  bornesup_thisline);
			}    		
			cplex.addMinimize(W);
			//System.out.println("cplex = "+cplex);
    		//System.out.println("lo = "+data.lo);
    		//System.out.println("Po = "+data.Po);
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
				 bornesup_thisline =  cplex.getObjValue(); // attention au passage d'un Lobj vers un autre ce n'est plus valable ce bornsup
				 borneinf_previousline[p2-5] = cplex.getObjValue();
				// if (p2==5) borneinf = borneinf_previousline[p2-5];
				 
	             System.out.println(" W  = " + cplex.getObjValue());
	             Double lam = -cplex.getValue(logR)/(cplex.getValue(Sum) + cplex.getValue(Encom) + cplex.getValue(Encombr) + cplex.getValue(Encomar));
	             System.out.println(" L  = " + lam);
	             Double power = cplex.getValue(En)/cplex.getValue(W);
	             System.out.println(" P  = " + power);
	             Double reliability = Math.exp(cplex.getValue(logR));
	             System.out.println(" R  = " + reliability);
	             System.out.println(" E  = " + cplex.getValue(En));
				 write_file(out_channel1, lam+" "); 
				 write_file(out_channel2, power+" "); 
				 write_file(out_channel3, cplex.getValue(W)+" ");
				 write_file(out_channel4, data.lo+" ");
				 write_file(out_channel5, data.Po+" ");
				 write_file(out_channel6, cplex.getStatus()+" ");
				 write_file(out_channel7, time+" ");
	         } else {
	        	  System.out.println(" No solution found ");
				write_file(out_channel1, "NaN ");
	        	write_file(out_channel2, "NaN ");
	        	write_file(out_channel3, "Nan ");
	        	write_file(out_channel4, data.lo+" ");
	        	write_file(out_channel5, data.Po+" ");
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
				
				for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++){
								if (cplex.getValue(R[i][j]) > 0.5){
									System.out.println("R[i][j]  i "+ i + " j "+ j + " = " +cplex.getValue(R[i][j]));
								}
								//	if (cplex.getValue(SP[i][j]) > 0.5){
								//	System.out.println("SP[i][j]  i "+ i + " j "+ j + " = "+cplex.getValue(SP[i][j]));
								//}
								
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
			
				//3 4
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													//expr1.addTerm(1, gccar[i][k][n][j][kk][ii][nn][jj][kkkk] );
													//expr2.addTerm(1, gccar[i][k][n][j][kk][ii][nn][jj][kkkk] );	
													//expr1.addTerm(-1,  xc[i][k][n][j][kk] );
													//expr2.addTerm(-1,  xcar[ii][nn][jj][kkkk] );
													if (cplex.getValue(gccar[i][k][n][j][kk][ii][nn][jj][kkkk]) > 0.5){
															System.out.println("---> gccar[i][k][n][j][kk][ii][nn][jj][kkkk] i "+i+" k "+ k+ " n "+n +" j "+j+ " kk "+ kk + " ii "+ ii+  " nn " +nn +" jj " + jj + " kkkk "+ kkkk +" = " + +cplex.getValue(gccar[i][k][n][j][kk][ii][nn][jj][kkkk]) );
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

	//5 6
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
												//	expr1.addTerm(1, gccbr[i][k][n][j][kk][ii][kkk][nn][jj] );
												//	expr2.addTerm(1, gccbr[i][k][n][j][kk][ii][kkk][nn][jj] );	
												//	expr1.addTerm(-1,  xc[i][k][n][j][kk] );
												//	expr2.addTerm(-1,  xcbr[ii][kkk][nn][jj] );
													if (cplex.getValue(gccbr[i][k][n][j][kk][ii][kkk][nn][jj]) > 0.5){
															System.out.println("---> gccbr[i][k][n][j][kk][ii][kkk][nn][jj] i "+i+" k "+ k+ " n "+n +" j "+j+ " kk "+ kk + " ii "+ ii+ " kkk " + kkk + " nn " +nn +" jj " + jj +" = " + +cplex.getValue(gccbr[i][k][n][j][kk][ii][kkk][nn][jj]) );
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

	//7 8
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
												//	expr1.addTerm(1, gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] );
												//	expr2.addTerm(1, gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] );	
												//	expr1.addTerm(-1,  xcbr[i][k][n][j] );
												//	expr2.addTerm(-1,  xc[ii][kkk][nn][jj][kkkk] );
													if (cplex.getValue(gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk]) > 0.5){
															System.out.println("---> gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk] i "+i+" k "+ k+ " n "+n +" j "+j+  " ii "+ ii+ " kkk " + kkk + " nn " +nn +" jj " + jj + " kkkk "+ kkkk +" = " + +cplex.getValue(gcbrc[i][k][n][j][ii][kkk][nn][jj][kkkk]) );
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
	//9 10
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
												//	expr1.addTerm(1, gcbrcbr[i][k][n][j][ii][kkk][nn][jj] );
												//	expr2.addTerm(1, gcbrcbr[i][k][n][j][ii][kkk][nn][jj] );	
												//	expr1.addTerm(-1,  xcbr[i][k][n][j] );
												//	expr2.addTerm(-1,  xcbr[ii][kkk][nn][jj] );
													if (cplex.getValue(gcbrcbr[i][k][n][j][ii][kkk][nn][jj]) > 0.5){
															System.out.println("---> gcbrcbr[i][k][n][j][ii][kkk][nn][jj] i "+i+" k "+ k+ " n "+n +" j "+j+  " ii "+ ii+ " kkk " + kkk + " nn " +nn +" jj " + jj +" = " + +cplex.getValue(gcbrcbr[i][k][n][j][ii][kkk][nn][jj]) );
													}
												}	
											}	
									}
								 }
							}
					}
				}
			}

	//11 12
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													//expr1.addTerm(1, gcbrcar[i][k][n][j][ii][nn][jj][kkkk] );
													//expr2.addTerm(1, gcbrcar[i][k][n][j][ii][nn][jj][kkkk] );	
													//expr1.addTerm(-1,  xcbr[i][k][n][j] );
													//expr2.addTerm(-1,  xcar[ii][nn][jj][kkkk] );
													if (cplex.getValue(gcbrcar[i][k][n][j][ii][nn][jj][kkkk]) > 0.5){
															System.out.println("---> gcbrcar[i][k][n][j][ii][nn][jj][kkkk] i "+i+" k "+ k+ " n "+n +" j " + " ii "+ ii+ " nn " +nn +" jj " + jj + " kkkk "+ kkkk +" = " + +cplex.getValue(gcbrcar[i][k][n][j][ii][nn][jj][kkkk]) );
													}
												}	
											}	
										}
									}
							}
					}
				}
			}

		//13 14
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													//expr1.addTerm(1, gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] );
													//expr2.addTerm(1, gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] );	
													//expr1.addTerm(-1,  xcar[i][n][j][kk] );
													//expr2.addTerm(-1,  xc[ii][kkk][nn][jj][kkkk] );
													if (cplex.getValue(gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk]) > 0.5){
															System.out.println("---> gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk] i "+i+ " n "+n +" j "+j+ " kk "+ kk + " ii "+ ii+ " kkk " + kkk + " nn " +nn +" jj " + jj + " kkkk "+ kkkk +" = " + +cplex.getValue(gcarc[i][n][j][kk][ii][kkk][nn][jj][kkkk]) );
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

	//15 16
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
								for (int kkk = 0; kkk < data.rep; kkk++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
												//	expr1.addTerm(1, gcarcbr[i][n][j][kk][ii][kkk][nn][jj] );
												//	expr2.addTerm(1, gcarcbr[i][n][j][kk][ii][kkk][nn][jj] );	
												//	expr1.addTerm(-1,  xcar[i][n][j][kk] );
												//	expr2.addTerm(-1,  xcbr[ii][kkk][nn][jj] );
													if (cplex.getValue(gcarcbr[i][n][j][kk][ii][kkk][nn][jj] ) > 0.5){
															System.out.println("---> gcarcbr[i][n][j][kk][ii][kkk][nn][jj]  i "+i+ " n "+n +" j "+j+ " kk "+ kk + " ii "+ ii+ " kkk " + kkk + " nn " +nn +" jj " + jj +" = " + +cplex.getValue(gcarcbr[i][n][j][kk][ii][kkk][nn][jj] ) );
													}
												}	
											}	
									}
								 }
							}
						}
					}
			}

	//17 18
			for (int i = 0; i < data.ntasks; i++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int kk = 0; kk < data.rep; kk++) {
							for (int ii = 0; ii < data.ntasks; ii++) {
									for (int jj = 0; jj < data.ntasks; jj++) {				
										for (int kkkk = 0; kkkk < data.rep; kkkk++) {
											for (int n = 0; n < data.nlinks; n++) {
												for (int nn = 0; nn < data.nlinks; nn++) {
													//expr1.addTerm(1, gcarcar[i][n][j][kk][ii][nn][jj][kkkk] );
													//expr2.addTerm(1, gcarcar[i][n][j][kk][ii][nn][jj][kkkk] );	
													//expr1.addTerm(-1,  xcar[i][n][j][kk] );
													//expr2.addTerm(-1,  xcar[ii][nn][jj][kkkk] );
													if (cplex.getValue(gcarcar[i][n][j][kk][ii][nn][jj][kkkk]) > 0.5){
															System.out.println("---> gcarcar[i][n][j][kk][ii][nn][jj][kkkk] i "+i+ " n "+n +" j "+j+ " kk "+ kk + " ii "+ ii+ " nn " +nn +" jj " + jj + " kkkk "+ kkkk +" = "  +cplex.getValue(gcarcar[i][n][j][kk][ii][nn][jj][kkkk]) );
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
		
			//chbr
			for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {				
						for (int l = 0; l < data.nprocs; l++) {				
							for (int ll = 0; ll < data.nprocs; ll++) {		
								for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
									if (cplex.getValue( chbr[i][k][j][l][ll][w]) > 0.5){
											System.out.println("--->  chbr[i][k][j][l][ll][w] i "+i+ " k "+k +" j "+j+ " l "+ l+ " ll " +ll +" w " + w +" = "  +cplex.getValue(chbr[i][k][j][l][ll][w]) );
									}
								}
							}
						}
					}
				}
			}	
	
			//chaar
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {	
					for (int kk = 0; kk < data.rep; kk++) {
						for (int l = 0; l < data.nprocs; l++) {				
							for (int ll = 0; ll < data.nprocs; ll++) {		
								for (int w = 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++) {
									if (cplex.getValue( chaar[i][j][kk][l][ll][w]) > 0.5){
											System.out.println("--->  chaar[i][j][kk][l][ll][w] i "+i+ " j "+j +" kk "+kk+ " l "+ l+ " ll " +ll +" w " + w +" = "  +cplex.getValue(chaar[i][j][kk][l][ll][w]) );
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

		//xcbr
		for (int i = 0; i < data.ntasks; i++) {
				for (int k = 0; k < data.rep; k++) {
					for (int j = 0; j < data.ntasks; j++) {	
						for (int m = 0; m < data.nlinks; m++) {
							if (cplex.getValue( xcbr[i][k][m][j] ) > 0.5){
									System.out.println("--->  xcbr[i][k][m][j] i "+i+ " k "+k +" m "+m+ " j "+ j +" = "  +cplex.getValue(xcbr[i][k][m][j]) );
							}
						}			
					}
				}
			}	


		//xcar
			for (int i = 0; i < data.ntasks; i++) {
				for (int j = 0; j < data.ntasks; j++) {				
					for (int kk = 0; kk < data.rep; kk++) {
						for (int m = 0; m < data.nlinks; m++) {
							if (cplex.getValue( xcar[i][m][j][kk] ) > 0.5){
								System.out.println("--->  xcar[i][m][j][kk] i "+i +" m "+m+ " j "+ j+ " kk " + kk+" = "  +cplex.getValue(xcar[i][m][j][kk]) );
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
		   } // for p2
	              
			write_file(out_channel1, "\n");
			write_file(out_channel2, "\n");
			write_file(out_channel3, "\n");
			write_file(out_channel4, "\n");
			write_file(out_channel5, "\n");
			write_file(out_channel6, "\n");
			write_file(out_channel7, "\n");

		} // for l1
	} // for l2
     
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
  
 /* algo initialiser S3  K a ll c : 
  pour tout K plus grand ou egal a 2
		pour toute combinaison a de K element
			pour tout proc ll
				trouver la comb a2 de K-1 element sous-combinaison de a ( s2 a = s2 a2 + l)
				pour toute combinaison de chemin c2 de a2 vers ll (S3 a2 K-1 ll)
					pour tout chemin w de l vers ll (CH l ll)
						concatener le chemin w avec la combinaison c2 en dernier (c = c2, ch)
						inserer cette combinaison c ( S3 a K ll )
					fin
				fin				
			fin
		fin
	fin
	
	Pour K  egal a 1 :
    	Pour toute combinaison a de 1 element
			pour tout proc ll
					pour tout chemin w de (S2 1 a)  vers ll (CH l ll)
						inserer une combinaison compose du chemin w  ( S3 a K ll )
					fin
			fin	
		fin
*/
  
  // S3 K a ll c
  // dans le cas ou pas de chemins entre les procs alors la combin c = (-1, ....,-1) est inseree
  private static Vector<Vector<Vector<Vector<Vector<Integer>>>>> initialiserS3(Data data, int[][] S1){
     Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3 = new Vector();
	S3.add(new Vector()); // element inutilsee car K commence de 1   
	//for (int K=1; K<=data.rep; K++){ // K>=2
	initialiserS3(data, S3, data.rep, S1);
	//}
	return S3;
  }
  
  
    
  private static void initialiserS3(Data data, Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3, int K, int[][] S1)
  { 
  
	assert(K >= 1);
	if (K == 1) {
		S3.add(new Vector()); // K=1
		for (int a = 0; a < Math.pow(data.nprocs, K); a++){
			S3.elementAt(1).add(new Vector());
			for (int ll = 0; ll < data.nprocs; ll++){
				S3.elementAt(1).elementAt(a).add(new Vector());
				int l = S1[a][0];
				for (int w= 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++){
					Vector<Integer> c = new Vector();
					c.add(w);	
					assert(c.size() == K);
					S3.elementAt(K).elementAt(a).elementAt(ll).add(c);				
				}
				if (data.CH.elementAt(l).elementAt(ll).isEmpty()){
					Vector<Integer> c = new Vector();
					c.add(-1);		
					assert(c.size() == K);
					S3.elementAt(K).elementAt(a).elementAt(ll).add(c);
				}
			}
		}
	} else {
		initialiserS3(data, S3, K-1, S1);
	//for (int K=2; K<=data.rep; K++){ // K>=2
		S3.add(new Vector());
		for (int a = 0; a < Math.pow(data.nprocs, K); a++){
			S3.elementAt(K).add(new Vector());
			int l = S1[a][K-1]; // last proc
			for (int ll = 0; ll < data.nprocs; ll++){
				S3.elementAt(K).elementAt(a).add(new Vector());
				int a2 = trouver_sous_comb(data, K-1, K,a, S1);
				assert( a2 >= 0 && a2 <  Math.pow(data.nprocs, K-1));
				for (int c2 = 0; c2 < S3.elementAt(K-1).elementAt(a2).elementAt(ll).size(); c2++){
					for (int w= 0; w < data.CH.elementAt(l).elementAt(ll).size(); w++){
							Vector<Integer> c = (Vector<Integer>)S3.elementAt(K-1).elementAt(a2).elementAt(ll).elementAt(c2).clone();
							c.add(w);
							assert(c.size() == K);
							S3.elementAt(K).elementAt(a).elementAt(ll).add(c);
					} 
					if (data.CH.elementAt(l).elementAt(ll).isEmpty()){
							Vector<Integer> c = (Vector<Integer>)S3.elementAt(K-1).elementAt(a2).elementAt(ll).elementAt(c2).clone();
							c.add(-1);
							assert(c.size() == K);
							S3.elementAt(K).elementAt(a).elementAt(ll).add(c);
					}
						
				}
			}
		}
	  }		 			
  }			

//NXXXX						
/* algo : S4 initialiserS4  
  exemple : S4 i e = (0,1,-1,0,1) 0 means predecessor but not routing 1 means predecessor and routing -1 means not a predecessor
  
	instancier S4 Vector<Vector<Vector<Integer>>> 
	Pour toute tache i
		np=trouver_nb_predecesseur(i)
		initialiserS4 S4 i  np
	fin	 
	retourner S4


  initialiserS4 S4 i np

    Si np >= 2
	initialiser S4 i np-1
	Pour toute combinaison e initialise jusuq'au np-1eme predecesseur : S4 i e
			jj = trouver_predecesseur(i,np)
			Modifier la combinaison e telle que e(jj)=0 (cas npieme predecesseur non routant)
			calulcer la combinaison e2 = e / e2(jj)=1 (cas mise du npieme predecesseur en etat routant)
			ajouter e2 a S4 i tout à la fin du vecteur S4 i 
	fin
	Si np == 1
		Ajouter un nouveau vecteur a S4 (pour les combs de i)
		jj = trouver_predecessor(i,np)
		calculer la combinaison e1 = (-1, -1, ..., 0,-1,...-1)  ou e1(jj)=0
		calculer la combinaison e2 = (-1, -1, ..., 1,-1,...-1)  ou e1(jj)=1 
		ajouter e1 et e2 a S4 i
	Si np == 0
		Ajouter un nouveau vecteur a S4 i (pour les combs de i mais il sera toujours vide, c-a-d pas de S4 i e) NO : ajouter un vecteur e. RAISON : rendre logRelPOS multi-usage
	fin si
*/

// peut inclure combin e =(-1,-1,-1,....,-1) dans le cas sans preds 
private static Vector<Vector<Vector<Integer>>> initialiserS4(Data data){
	Vector<Vector<Vector<Integer>>> S4 = new Vector();
	for (int i=0; i< data.ntasks; i++){
			int np = trouver_nb_predecesseurs(data, i);
			initialiserS4(data,S4,i,np);
	}
	return S4;
}

private static void initialiserS4(Data data, Vector<Vector<Vector<Integer>>> S4, int i, int np){
	if (np>=2){
		initialiserS4(data,S4,i,np-1);
	//	for (int i=0; i< data.ntasks; i++){
			int taille = S4.elementAt(i).size();
			for (int e=0; e<taille; e++){
				int jj = trouver_predecesseur(data, i,np);
				Vector<Integer> comb = S4.elementAt(i).elementAt(e);
				Vector<Integer> combclone = (Vector<Integer>)comb.clone();
				S4.elementAt(i).elementAt(e).setElementAt(0,jj); // predecesseur non reoutant
				combclone.setElementAt(1,jj); //predecesseur routant
				S4.elementAt(i).add(combclone); 
			}		
	//	}
	}else if (np == 1){
		S4.add(new Vector());
		int jj = trouver_predecesseur(data, i,np);
		Vector<Integer> e1 = new Vector();
		for (int k=0; k< data.ntasks; k++){
			e1.add(-1);
		}	
		Vector<Integer> e2 = (Vector<Integer>)e1.clone();
		e1.setElementAt(0,jj);
		e2.setElementAt(1,jj);
		S4.elementAt(i).add(e1);
		S4.elementAt(i).add(e2);
	}else if (np == 0) { // i n a pas de predecesseurs 
		assert(!has_preds(data,i));
		S4.add(new Vector());
		Vector<Integer> e1 = new Vector();
		for (int k=0; k< data.ntasks; k++){
			e1.add(-1);
		}	
		S4.elementAt(i).add(e1);		
	} else
		assert(false);
}


/* algo : S5 initialiserS5  
   exemple : S5 i e f = (-1,3,-1,-1,0) -1 means either not a predecessor or a predecessor but not routing, 
                                       0 means a predecessor (and routing) on processor 0, 
									   3 means a predecessor (and routing) on processor 3
									   
   instancier S5 Vector<Vector<Vector<Vector<Integer>>>> 
   Pour toute tache i
		ajouter un vecteur vide a S5
		Pour toute combin e de routans de i
			ajouter un vecteur vide a S5 i
			nr=trouver_nb_routants(i)  dans e : c-à-d le nombre de 1 dans e
			pour toute combinaison de proc f de taille nr
				instancier une combinaison f2 de taille ntasks :
				Pour tout jj allant de 0 a nbtasks :
					f2(jj) = -1 si e(jj) != 1
					f2(jj) = f(ind++) si e(jj) == 1
				fin
				ajouter f2 a S5 i e
			fin
		fin
	fin		 
	retourner S5
*/				

// une combin f =(-1,-1,-1,....,-1) signifie aucun predecesseur n'est routant OU BIEN aucun predecesseur tout court
private static Vector<Vector<Vector<Vector<Integer>>>> initialiserS5(Data data, int[][] S1, Vector<Vector<Vector<Integer>>> S4){
	assert(S4.size() == data.ntasks);
	Vector<Vector<Vector<Vector<Integer>>>> S5 = new Vector(); 
	for (int i=0; i< data.ntasks; i++){
		S5.add(new Vector());
		for (int e=0; e<S4.elementAt(i).size(); e++){
			if (!has_preds(data,i)) assert(S4.elementAt(i).size()==1);
			S5.elementAt(i).add(new Vector());
			int nr = trouver_nb_routants(data,i,S4,e);
			for (int a = 0; a<Math.pow(data.nprocs,nr); a++){
				Vector<Integer> acomb = new Vector();
				int ind = 0;
				for (int k=0; k< data.ntasks; k++){
					if (S4.elementAt(i).elementAt(e).elementAt(k) != 1) acomb.add(-1);
					else if (S4.elementAt(i).elementAt(e).elementAt(k) == 1) {
						assert(ind < nr);
						acomb.add(S1[a][ind++]);
					} else assert(false);
				}
				assert(ind == nr);
				S5.elementAt(i).elementAt(e).add(acomb);					
			}
		}
	}
	return S5;
}
						
/* algo : S6 initialiserS6  
   exemple : S6 i g = (0,1,-1,0,1) 0 means first task is a successor but not routing 
								   1 means second task is a successor and routing 
								   -1 means third task is  not successor
								   ...
    
	idem S4 initialiserS4 mais	en remplacant  "S4" par "S6"
												"e" par "g"
											   "np" par "ns"
											   "jj" par "j"  
	                                           "np=trouver_nombre_predecesseur(i)" par "ns=trouver_nombre_successeur(i)"
				                               "jj = trouver_predecesseur(i,np)" par "j = trouver_successeur(i,ns)"

	
*/	


private static Vector<Vector<Vector<Integer>>> initialiserS6(Data data){
	Vector<Vector<Vector<Integer>>> S6 = new Vector();
	for (int i=0; i< data.ntasks; i++){
			int ns = trouver_nb_successeurs(data,i);
			initialiserS6(data,S6,i,ns);
	}
	return S6;
}

//g=(-1 ... -1) dans le cas sans succs
private static void initialiserS6(Data data, Vector<Vector<Vector<Integer>>> S6, int i, int ns){
	if (ns>=2){
		initialiserS6(data,S6,i,ns-1);
	//	for (int i=0; i< data.ntasks; i++){
			int taille = S6.elementAt(i).size();
			for (int x=0; x<taille; x++){
				int j = trouver_successeur(data,i,ns);
				Vector<Integer> comb = S6.elementAt(i).elementAt(x);
				Vector<Integer> combclone = (Vector<Integer>)comb.clone();
				S6.elementAt(i).elementAt(x).setElementAt(0,j); // successeur non routant
				combclone.setElementAt(1,j); //successeur routant
				S6.elementAt(i).add(combclone); 
			}		
	//	}
	}else if (ns == 1){
		S6.add(new Vector());
		int j = trouver_successeur(data,i,ns);
		Vector<Integer> g1 = new Vector();
		for (int k=0; k< data.ntasks; k++){
			g1.add(-1);
		}	
		Vector<Integer> g2 = (Vector<Integer>)g1.clone();
		g1.setElementAt(0,j) ;
		g2.setElementAt(1,j) ;
		S6.elementAt(i).add(g1);
		S6.elementAt(i).add(g2);
	}else if (ns == 0) { // i n a pas de successeurs 
		assert(!has_succs(data,i));
		S6.add(new Vector());
		Vector<Integer> g1 = new Vector();
		for (int k=0; k< data.ntasks; k++){
			g1.add(-1);
		}	
		S6.elementAt(i).add(g1);
	} else
		assert(false);
}


/*  algo : S7 initialiserS7
 exemple : S7 i g h = (-1,3,-1,-1,0)  -1 means the first task is not a successor, 
                                       0 means the fifth task is a successor (may be routing or not depending on the combination of S6 i g) mapped on processor 0, 
									   3 means the second task is a successor (may be routing or not depending on the combination of S6 i g) mapped on processor 3  
									   ...
	idem 	S5 initialiserS5  en remplacant
									"S5" par "S7"
									"nr" par "ns"
									"e" par "g"
									"f" par "h"
									"jj" par "j"
									 e(jj) != 1 par  g(jj) == -1
									 e(jj) == 1 par g(jj) != -1
									 "trouver_nb_routants" par "trouver_nb_successeurs"
*/ 
// une combin h =(-1,-1,-1,....,-1) ne devrait jamais arrivé. NO : si dans le cas sans succs. RAISON : multi-usage de logRelPOS

 private static Vector<Vector<Vector<Vector<Integer>>>> initialiserS7(Data data, int[][] S1, Vector<Vector<Vector<Integer>>> S6){
	assert(S6.size() == data.ntasks);
	Vector<Vector<Vector<Vector<Integer>>>> S7 = new Vector(); 
	for (int i=0; i< data.ntasks; i++){
		S7.add(new Vector());
		for (int x=0; x<S6.elementAt(i).size(); x++){
			if (!has_succs(data,i)) assert(S6.elementAt(i).size()==1);
			S7.elementAt(i).add(new Vector());
			int ns = trouver_nb_successeurs(data,i);
			for (int a = 0; a<Math.pow(data.nprocs,ns); a++){
				Vector<Integer> acomb = new Vector();
				int ind = 0;
				for (int k=0; k< data.ntasks; k++){
					if (S6.elementAt(i).elementAt(x).elementAt(k) == -1) acomb.add(-1);
					else if (S6.elementAt(i).elementAt(x).elementAt(k) != -1) {
						assert(ind < ns);
						acomb.add(S1[a][ind++]);
					} else assert(false);
				}
				assert(ind == ns);
				S7.elementAt(i).elementAt(x).add(acomb);					
			}
		}
	}
	return S7;
}

													
/*  algo : S8 initailserS8
	S8 i e f K a g h o = une combin d'indice o dans le vecteur S8 i e f K a g h = une combin de chemins entre les pred de i (routants uniquement) et les K replicas de i + les chemins entre les K replicas de i et les succs de i (routants ou non) 
	                   = ((r1->i0,r2->i0,r3<-i0,s1<-i0,s2<-i0),(r1->i1,r2->i1,r3->i1,s1<-i1,s2<-i1), ..., (r1->iK-1,r2->iK-1,r3->iK-1,s1<-iK-1,s2<-iK-1))
						 avec r1 ... r3 sont donnés par S4 i e
						 avec s1 .. s2 sont donnés par S6 i g
						
		instancier S8  Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Integer>>>>>>>>>> 
		initialiserS8 S8 K
		return S8
	
	initialiserS8 S8 K 
		Si K>= 2
		initialiserS8 S8 K-1
		Pour toute tache i, ajouter vecteur i
			Pour toute combin e de routants predecesseurs de i, ajouter vecteur e
				Pour toute combin f de procs desdits routants, ajouter vecteur f, ajouter 2 vecteurs K (K=0 non utilise)
					Pour toute combin a de proc des K replicas , ajouter vecteur a
						Pour toute combin g d'etats des successeurs de i, ajouter vecteur g
							Pour toute combin h de proc desdits succeseurs, ajouter vecteur h
								Pour toute combin o de chemins entre les routants et les successseurs d'une part et les K-1 replicas MAIS mappe sur les K-1 procs de a d'autre part; c-a-d extraite de S8 i e f K-1 a2 g h,
									Quelle est la valeur de a2 ? a2 est tel que S1[k-1][a2] egal a (S[a] projete sur (0,...,K-2))
									a2 =trouver_sous_comb(K-1, K, a);
									Pour toute combin o2 de chemins entre les routants et les succeseurs d'une part et le Keme replica d'aure part MAIS mappe sur le dernier proc de a; c-a-d  extraite de S8 i e f 1 a3 g h
										Quelle est la valeur de a3 ? a3 est tel que S1[1][a3] egal a (S[a] projete sur K-1)
										a3 =trouver_sous_comb(1, S1[a][K-1]);
										o3 = cloner o
										ajouter o2(0) a la fin de o3
										ajouter o3 a S8 i e f K a g h 
									fin
								fin
							fin
						fin
					fin
				fin
			fin
		fin
		
	Si K==1
		Pour toute tache i, ajouter vecteur i
			Pour toute combin e de routants predecesseurs de i, ajouter vecteur e
				Pour toute combin f de procs desdits routants, ajouter vecteur f, ajouter 2 vecteur K  (K=0 non utilise)
					Pour toute combin a de proc allant de 0 a nbproc^1 -1, ajouter vecteur a 
						Pour toute combin g d'etats des successeurs de i, ajouter vecteur g
							Pour toute combin h de proc desdits succeseurs, ajouter vecteur h
								Pour toute combin c1 de chemins entre les routants d'une part et le replica d'autre part extraite de S3 K2 f l (avec l= S1 K a et K2= taille en proc de S5 i e f) 
									Attention : gerer le cas K2 = 0
									Pour toute combin c2 de chemins entre le replica et les succeseurs extraite de S3r (ATTENTION pas S3 sauf si vous inverser les chemins)
										idem Attention : gerer le cas K3 = 0
										c = fusionner c1 et c2
										instancier un vecteur o
										o(0)=c
										ajouter o a S8 i e f K a g h
									fin
								fin
							fin
						fin
					fin
				fin
			fin
		fin
	sinon erreur	
*/
/*NXXXX il faut S9 i e f K a o pour les chemins des operations avec pred mais sans succ. NO : cà peut etre gere par S8 avec h=g=(-1 ... -1)
  XXXX il faut S10 i K a g h o pour les chemins des operations sans pred mais avec succ. NO : çà peut etre gere par S8 avec e=f=(-1 .... -1) 
*/
// combin o=((), ...., ()) signifie pas preds et pas de succs pour replica 0 ... replica K-1 
private static Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Integer>>>>>>>>>> initialiserS8(Data data, int[][] S1,   Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3, Vector<Vector<Vector<Integer>>> S4,  Vector<Vector<Vector<Vector<Integer>>>> S5, Vector<Vector<Vector<Integer>>>S6, Vector<Vector<Vector<Vector<Integer>>>> S7){
	assert(S1.length == Math.pow(data.nprocs,data.rep));
	assert(S3.size() == data.rep+1);
	assert(S4.size() == data.ntasks);
	assert(S5.size() == data.ntasks);
	assert(S6.size() == data.ntasks);
	assert(S7.size() == data.ntasks);
	Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Integer>>>>>>>>>> S8 = new Vector();
	initialiserS8(data, S8, data.rep, S1, S3, S4, S5, S6, S7);
	return S8;
}

private static void  initialiserS8(Data data, Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Integer>>>>>>>>>> S8, int K,int[][] S1,  Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3, Vector<Vector<Vector<Integer>>> S4,  Vector<Vector<Vector<Vector<Integer>>>> S5, Vector<Vector<Vector<Integer>>> S6, Vector<Vector<Vector<Vector<Integer>>>> S7){
	assert(K<= data.rep);
	if (K>= 2) {
		initialiserS8(data, S8, K-1,S1, S3, S4, S5, S6, S7);
		for (int i=0; i<data.ntasks; i++){
		//	S8.add(new Vector());
			for (int e =0; e< S4.elementAt(i).size(); e++){
		//		S8.elementAt(i).add(new Vector());
				for (int f=0; f < S5.elementAt(i).elementAt(e).size(); f++){
		//			S8.elementAt(i).elementAt(e).add(new Vector());
					//S8.elementAt(i).elementAt(e).elementAt(f).add(new Vector());
					S8.elementAt(i).elementAt(e).elementAt(f).add(new Vector()); // K
					for (int a=0; a< Math.pow(data.nprocs, K); a++){
						S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).add(new Vector());
						for (int x =0; x< S6.elementAt(i).size(); x++){
							S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).add(new Vector());
							for (int h =0; h< S7.elementAt(i).elementAt(x).size(); h++){
								S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).add(new Vector());
								int a2 =trouver_sous_comb(data, K-1, K, a, S1);
								for (int o=0; o <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K-1).elementAt(a2).elementAt(x).elementAt(h).size(); o++){
									int a3 =trouver_sous_comb(data, 1, S1[a][K-1], S1);
									for (int o2=0; o2 <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(1).elementAt(a3).elementAt(x).elementAt(h).size(); o2++){
										Vector<Vector<Integer>> o3 = (Vector<Vector<Integer>>)S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K-1).elementAt(a2).elementAt(x).elementAt(h).elementAt(o).clone();
										assert(o3.size() == K-1);
										assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(1).elementAt(a3).elementAt(x).elementAt(h).elementAt(o2).size() == 1);
										o3.add(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(1).elementAt(a3).elementAt(x).elementAt(h).elementAt(o2).elementAt(0));
										S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).add(o3);
									}
								}
							}
						}
					}
				}
			}
		}
	} else if (K==1) {
		for (int i=0; i<data.ntasks; i++){
			S8.add(new Vector());
			for (int e =0; e< S4.elementAt(i).size(); e++){
				S8.elementAt(i).add(new Vector());
				for (int f=0; f < S5.elementAt(i).elementAt(e).size(); f++){
					S8.elementAt(i).elementAt(e).add(new Vector());
					S8.elementAt(i).elementAt(e).elementAt(f).add(new Vector());
					S8.elementAt(i).elementAt(e).elementAt(f).add(new Vector());
					for (int a=0; a< Math.pow(data.nprocs, K); a++){
						S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).add(new Vector());
						for (int x =0; x< S6.elementAt(i).size(); x++){
							S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).add(new Vector());
							for (int h =0; h< S7.elementAt(i).elementAt(x).size(); h++){
								S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).add(new Vector());
								//int K2 = S5.elementAt(i).elementAt(e).elementAt(f).size(); XXXX FAUX !!!!!!!! en plus il faut gerer le cas ou e est sans routants ou sans preds cad f=(-1 ... -1) dans ce cas pas de process. 
								                                                                       //  donc pas de fusion de c1 avec c2 car :  c= c2
								//int K3 = S7.elementAt(i).elementAt(g).elementAt(h).size();  XXXX idem FAUX !!!!!!!!!! en plus le cas sans succ g=h (-1 ... -1) est-il correctement gere ?
								int K2 = trouver_nb_routants_procs(data,i,S5,e,f);	assert(K2 == trouver_nb_routants(data,i,S4,e));
								int K3 = trouver_nb_successeurs_procs(data,i,S7,x,h);	assert(K3 == trouver_nb_successeurs(data,i)); assert(K3 == trouver_nb_successeurs(data,i,S6,x));
																	 
								if (K2 != 0 && K3 !=0) { // cas avec preds avec routants avec succs  c = c1 + c2
									assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).isEmpty());
									for (int c1=0; c1 < S3.elementAt(K2).elementAt(f).elementAt(S1[a][0]).size(); c1++){ //  XXXX : est ce que la combin des proc de S3 est f ????? oui par construction de S5
										for (int c2=0; c2 < S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).size(); c2++){ // XXXXX : idem h est-il le bon numero pour S3 ????  oui par construction de S7
											Vector<Integer> c = trouver_fusion(S3.elementAt(K2).elementAt(f).elementAt(S1[a][0]).elementAt(c1), S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2));
											Vector<Vector<Integer>> o = new Vector();
											o.add(c);
											S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).add(o);
										}
									} 
								} else if (K2 ==0 && K3 !=0) { //cas sans preds OU sans routants avec succs   c = c2
									assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).isEmpty());
									for (int c2=0; c2 < S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).size(); c2++){ 
											assert(h<Math.pow(data.nprocs,K3));
											assert(S7.elementAt(i).elementAt(x).size() == Math.pow(data.nprocs,K3));
											Vector<Integer> c = trouver_fusion(null, S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2));
											Vector<Vector<Integer>> o = new Vector();
											o.add(c);
											S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).add(o);
									}
									
									
											//debug start
											
									for (int c2=0; c2 < S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).size(); c2++){ 
											assert(h<Math.pow(data.nprocs,K3));
											assert(S7.elementAt(i).elementAt(x).size() == Math.pow(data.nprocs,K3));
											Vector<Integer> c = trouver_fusion(null, S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2));
											assert(S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2).size() == K3);
										//	assert(S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2).size() == S1[h].length);
											for (int j = 0; j <S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2).size(); j++){
												int w = S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).elementAt(c2).elementAt(j);
												if (w != -1) {
													int l = S1[a][0];
													int ll = S1[h][j];
													assert(ll == trouver_successeur_proc(data, i, S7, x, h, j-K2+1));
													assert(	w < data.CH.elementAt(l).elementAt(ll).size() );
												}
											}
									}
											
											
									assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size() == S3.elementAt(K3).elementAt(h).elementAt(S1[a][0]).size());
									for (int o=0; o <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size(); o++){
											int k = 0;
											Vector<Integer> ok = S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).elementAt(k);
											assert(ok.size() == K2+K3);
											for (int c=0 ; c < ok.size(); c++){
												int w = ok.elementAt(c);
												if (w != -1) {
													int l = -1;
													int ll = -1;
													double tcom = -1;
													if (c < K2){
														l = trouver_routant_proc(data, i, S5, e, f, c+1); // le "c+1" eme proc dans la combinaison f
														int jj = trouver_routant(data, i, S4, e, c+1);// le "c+1" eme predecesseur routant dans la combinaison e
														ll = S1[a][k];
														tcom = data.tcoms[jj][i];
													} else if (c-K2 < K3 ){
														l = S1[a][k];
														ll = trouver_successeur_proc(data, i, S7, x, h, c-K2+1); // le "c-K1+1" eme proc dans la combinaison h
														int j = trouver_successeur(data, i, S6, x, c-K2+1); // le "c-K1+1" eme successeur  dans la combinaison h
														assert(j==trouver_successeur(data, i, c-K2+1));// le "c-K1+1" eme successeur dans la data.succs[i]
														tcom = data.tcoms[i][j];
													} else 
														assert(false);
												//	System.out.println("K K2  K3  c " + K + " " +K2 + " " + K3+ " " +  c );	
												//	System.out.println("l ll  data.CH.elementAt(l).elementAt(ll).size() w" + " " +l + " " + ll+ " " +  data.CH.elementAt(l).elementAt(ll).size() + " " + w);	
													assert(	data.CH.elementAt(l).elementAt(ll).size() == data.CH.elementAt(ll).elementAt(l).size() );
													assert(	w < data.CH.elementAt(l).elementAt(ll).size() );
												}
											}
										}			
										//debug end
																						
								} else if  (K2 !=0 && K3 ==0) { //cas avec preds  avec routants sans succs    c = c1
									assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).isEmpty());
									for (int c1=0; c1 < S3.elementAt(K2).elementAt(f).elementAt(S1[a][0]).size(); c1++){ 
										Vector<Integer> c = trouver_fusion(S3.elementAt(K2).elementAt(f).elementAt(S1[a][0]).elementAt(c1), null);
										Vector<Vector<Integer>> o = new Vector();
										o.add(c);
										S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).add(o);
									} 
																		
								} else if  (K2 ==0 && K3 ==0) { //cas sans preds OU sans routants sans succs  c = ()
										assert(S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).isEmpty());
										Vector<Integer> c = new Vector();
										Vector<Vector<Integer>> o = new Vector();
										o.add(c);
										S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).add(o);
								} else assert(false);									
							}
						}
					}
				}
			}
		}				
	} // fin K=1	
	else assert(false);
}

//NXXXX : S3r. Mais vrai dire je ne suis pas sur si ca fausse les resultats en gardant S3, apres tout c est juste des numeros de chemins
//NXXXX : S3 calcule les chemins des replicas vers une op => inverser l ordre des liens dans chemin indexe par chaar car sinon la relation de precedence entre les communications
//          d un chemin est fausse. PAS SUR car on peut toujour inverser les exteminites du chemin lors de l appel de data.CH (avec le numero de chemin errone)
		  																						

   private static int trouver_sous_comb(Data data, int sousK, int K, int a, int[][] S1){
	assert(K <= data.rep);
	assert(a <= Math.pow(data.nprocs, K));
	assert(sousK < K);
	boolean trouve = false;
	for (int a2 = 0; a2 < Math.pow(data.nprocs, sousK); a2++){
		trouve =true;
		for (int k2=0; k2 < sousK; k2++) {
			if (S1[a][k2] != S1[a2][k2]) trouve = false; 
		} 
		if (trouve) return a2;
	}
	assert(false);
	return -1;
  }
  
 private static int trouver_sous_comb(Data data, int sousK, int P, int[][] S1){
	assert(sousK == 1);
	assert(P < data.nprocs);
	boolean trouve = false;
	for (int a2 = 0; a2 < Math.pow(data.nprocs, sousK); a2++){
		trouve =true;
		for (int k2=0; k2 < sousK; k2++) {
			assert(S1[a2][k2] == a2);
			if (P != S1[a2][k2]) trouve = false; 
		} 
		if (trouve) return a2;
	}
	assert(false);
	return -1;
  }

private static int trouver_nb_predecesseurs(Data data, int i){
	assert(i< data.ntasks);
	int nb = 0;
	for (int jj=0; jj< data.ntasks; jj++){
		if (data.succs[jj][i] == 1) 
			nb++;
	}
	return nb;
}

private static int trouver_nb_successeurs(Data data, int i){
	assert(i< data.ntasks);
	int nb = 0;
	for (int j=0; j< data.ntasks; j++){
		if (data.succs[i][j] == 1) 
			nb++;
	}
	return nb;
}


// np start from 1 to ntasks
private static int trouver_predecesseur(Data data, int i, int np){
	assert(i< data.ntasks);
	assert(np<= trouver_nb_predecesseurs(data,i));
	assert(np>= 1);
	int n = 0;
	for (int jj=0; jj< data.ntasks; jj++){
		if (data.succs[jj][i] == 1) 
			n++;
		if (n==np) return jj;	
	}
	assert(false);
	return -1;
}

// ns start from 1 to ntasks
private static int trouver_successeur(Data data, int i, int ns){
	assert(i< data.ntasks);
	assert(ns<= trouver_nb_successeurs(data,i));
	assert(ns>= 1);
	int n = 0;
	for (int j=0; j< data.ntasks; j++){
		if (data.succs[i][j] == 1) 
			n++;
		if (n==ns) return j;	
	}
	assert(false);
	return -1;
}

private static int trouver_nb_routants_procs(Data data, int i, Vector<Vector<Vector<Vector<Integer>>>> S5, int e, int f){
	assert(i< data.ntasks);
	assert(S5.size() == data.ntasks);
	assert(e < S5.elementAt(i).size());
	assert(f < S5.elementAt(i).elementAt(e).size());
	//assert(S5.elementAt(i).elementAt(e).size() == data.ntasks);
	assert(S5.elementAt(i).elementAt(e).elementAt(f).size() == data.ntasks);
	int nb = 0;
	for (int jj=0; jj< S5.elementAt(i).elementAt(e).elementAt(f).size(); jj++){
		if (S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj) != -1) {
			assert(S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj) >= 0);
			assert(S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj) < data.nprocs);
			nb++;
		}
	} 
	assert(nb <= trouver_nb_predecesseurs(data,i));
	return nb;
}

// retourne le "c+1 eme" predecesseur routant de i
private static int trouver_routant_proc(Data data, int i, Vector<Vector<Vector<Vector<Integer>>>> S5, int e, int f, int npr){

	assert(i< data.ntasks);
	assert(S5.size() == data.ntasks);
	assert(e < S5.elementAt(i).size());
	assert(f < S5.elementAt(i).elementAt(e).size());
	//assert(S5.elementAt(i).elementAt(e).size() == data.ntasks);
	assert(S5.elementAt(i).elementAt(e).elementAt(f).size() == data.ntasks);
	assert(npr <= trouver_nb_predecesseurs(data, i));
	assert(npr >= 1);
	int nb = 0;
	for (int jj=0; jj< S5.elementAt(i).elementAt(e).elementAt(f).size(); jj++){
		if (S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj) != -1) {
			assert(S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj) >= 0);
			assert(S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj) < data.nprocs);			
			nb++;
			if (nb == npr) return S5.elementAt(i).elementAt(e).elementAt(f).elementAt(jj);
		}
	} 
	assert(false);
	return -1;
}



private static int trouver_nb_routants(Data data, int i, Vector<Vector<Vector<Integer>>> S4, int e){
	assert(i< data.ntasks);
	assert(S4.size() == data.ntasks);
	assert(e < S4.elementAt(i).size());
	assert(S4.elementAt(i).elementAt(e).size() == data.ntasks);
	int nb = 0;
	for (int jj=0; jj< S4.elementAt(i).elementAt(e).size(); jj++){
		if (S4.elementAt(i).elementAt(e).elementAt(jj) == 1)
			nb++;
	} 
	assert(nb <= trouver_nb_predecesseurs(data,i));
	return nb;
	
}


private static int trouver_routant(Data data, int i, Vector<Vector<Vector<Integer>>> S4, int e, int nr){
	assert(i< data.ntasks);
	assert(S4.size() == data.ntasks);
	assert(e < S4.elementAt(i).size());
	assert(S4.elementAt(i).elementAt(e).size() == data.ntasks);
	int nb = 0;
	for (int jj=0; jj< S4.elementAt(i).elementAt(e).size(); jj++){
		if (S4.elementAt(i).elementAt(e).elementAt(jj) == 1){
			nb++;
			if (nb == nr) return jj;
		}
	} 
	assert(false);
	return -1;	
}





private static int trouver_nb_successeurs_procs(Data data, int i, Vector<Vector<Vector<Vector<Integer>>>> S7, int x, int h){
	assert(i< data.ntasks);
	assert(S7.size() == data.ntasks);
	assert(x < S7.elementAt(i).size());
	assert(h < S7.elementAt(i).elementAt(x).size());
//	assert(S7.elementAt(i).elementAt(x).size() == data.ntasks);
	assert(S7.elementAt(i).elementAt(x).elementAt(h).size() == data.ntasks);
	int nb = 0;
	for (int j=0; j< S7.elementAt(i).elementAt(x).elementAt(h).size(); j++){
		if (S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j) != -1){
			assert(S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j) >= 0);
			assert(S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j) < data.nprocs);
			nb++;
		}
	} 
	assert(nb == trouver_nb_successeurs(data,i));
	return nb;
}

//nsp starts with 1 to ns
private static int trouver_successeur_proc(Data data, int i, Vector<Vector<Vector<Vector<Integer>>>> S7, int x, int h, int nsp){
	assert(i< data.ntasks);
	assert(S7.size() == data.ntasks);
	assert(x < S7.elementAt(i).size());
	assert(h < S7.elementAt(i).elementAt(x).size());
	//assert(S7.elementAt(i).elementAt(x).size() == data.ntasks);
	assert(S7.elementAt(i).elementAt(x).elementAt(h).size() == data.ntasks);
	assert(nsp <= trouver_nb_successeurs(data, i));
	assert(nsp >= 1);

	int nb = 0;
	for (int j=0; j< S7.elementAt(i).elementAt(x).elementAt(h).size(); j++){
		if (S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j) != -1){
			assert(S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j) >= 0);
			assert(S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j) < data.nprocs);
			nb++;
			if (nb == nsp) return S7.elementAt(i).elementAt(x).elementAt(h).elementAt(j);
		}
	} 
	assert(false);
	return -1;
}

private static int trouver_nb_successeurs(Data data, int i, Vector<Vector<Vector<Integer>>> S6, int x){
	assert(i< data.ntasks);
	assert(S6.size() == data.ntasks);
	assert(x < S6.elementAt(i).size());
	assert(S6.elementAt(i).elementAt(x).size() == data.ntasks);
	int nb = 0;
	for (int j=0; j< S6.elementAt(i).elementAt(x).size(); j++){
		if (S6.elementAt(i).elementAt(x).elementAt(j) != -1){
			assert(S6.elementAt(i).elementAt(x).elementAt(j) == 1 || S6.elementAt(i).elementAt(x).elementAt(j) == 0);
			nb++;
		}
	} 
	assert(nb == trouver_nb_successeurs(data,i));
	return nb;
}


private static int trouver_successeur(Data data, int i, Vector<Vector<Vector<Integer>>> S6, int x,  int ns){
	assert(i< data.ntasks);
	assert(x < S6.elementAt(i).size());
	assert(S6.elementAt(i).elementAt(x).size() == data.ntasks);
	assert(ns <= trouver_nb_successeurs(data, i));
	assert(ns >= 1);

	int nb = 0;
	for (int j=0; j< S6.elementAt(i).elementAt(x).size(); j++){
		if (S6.elementAt(i).elementAt(x).elementAt(j) != -1){
			assert(S6.elementAt(i).elementAt(x).elementAt(j) == 1 || S6.elementAt(i).elementAt(x).elementAt(j) == 0);
			nb++;
			if (nb == ns) return j;
		}
	} 
	assert(false);
	return -1;
}


private static Vector<Integer> trouver_fusion(Vector<Integer> v1, Vector<Integer> v2 ){
	Vector v3 = new Vector();
	if (v1 != null) assert(v1.size() >= 1); //car Dans S3 par contruction, meme si pas de chemins il y aura un vecteur (-1,-1,...) 
	if (v2 != null) assert(v2.size() >= 1);
	if (v1 != null) 
		for (int i=0; i < v1.size(); i++){
			v3.add(v1.elementAt(i));
		}	
	if (v2 != null) 
		for (int j=0; j< v2.size(); j++){
			v3.add(v2.elementAt(j));	
		}
	return v3;
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

private static void afficherS3( Vector<Vector<Vector<Vector<Vector<Integer>>>>> S3, Data data)
  {
  
	System.out.println (" //////////////////////////////////////////////////////// AFFICHAGE S3 ");
  
  // S3 K a ll c k  
			 for (int K=1; K<=data.rep; K++){
				for (int a = 0; a < Math.pow(data.nprocs, K); a++){
					for (int ll = 0; ll < data.nprocs; ll++){
							for (int c = 0; c < S3.elementAt(K).elementAt(a).elementAt(ll).size() ; c++){ 
								assert( S3.elementAt(K).elementAt(a).elementAt(ll).elementAt(c).size() == K);
								for (int k=0; k <  S3.elementAt(K).elementAt(a).elementAt(ll).elementAt(c).size(); k++){
									System.out.println( " K , a -> ll :  " + K + " , "+ a +" -> "+ ll + "  combinaison : "+ c + " , chemin " + k +" = "  +S3.elementAt(K).elementAt(a).elementAt(ll).elementAt(c).elementAt(k));
								}
							}
						}
					}
				}
  }
  
  
   private static void afficherS4andS5(Vector<Vector<Vector<Integer>>> S4, Vector<Vector<Vector<Vector<Integer>>>> S5, Data data){
   	System.out.println (" //////////////////////////////////////////////////////// AFFICHAGE S6 & S7 ");

		for (int i=0; i< data.ntasks; i++){
			assert(S5.elementAt(i).size()==S4.elementAt(i).size());
			for (int e=0; e<S5.elementAt(i).size(); e++){
				System.out.print(" S4 "+i + ","+ e + " = ( "	);	
					//for (int ind = 0; ind<S4.elementAt(i).elementAt(e).size(); ind++)
					//	System.out.print(S4.elementAt(i).elementAt(e).elementAt(ind) +",");	
					System.out.print(S4.elementAt(i).elementAt(e));			
					System.out.println(") "	);		
				for (int f = 0; f<S5.elementAt(i).elementAt(e).size(); f++){
					System.out.print("		S5 "+i + ","+ e + ","+ f+ " = ( "	);	
					//for (int ind = 0; ind<S5.elementAt(i).elementAt(e).elementAt(f).size(); ind++)
					//	System.out.print(S5.elementAt(i).elementAt(e).elementAt(f).elementAt(ind) +",");	
					System.out.print(S5.elementAt(i).elementAt(e).elementAt(f));			
					System.out.println(") "	);			
				}
			}
		}
	}

   private static void afficherS6andS7(Vector<Vector<Vector<Integer>>> S6, Vector<Vector<Vector<Vector<Integer>>>> S7, Data data){
   	System.out.println (" //////////////////////////////////////////////////////// AFFICHAGE S6 & S7 ");

		for (int i=0; i< data.ntasks; i++){
			assert(S7.elementAt(i).size()==S6.elementAt(i).size());
			for (int x=0; x<S7.elementAt(i).size(); x++){
				System.out.print(" S6 "+i + ","+ x + " = ( "	);	
					//for (int ind = 0; ind<S6.elementAt(i).elementAt(x).size(); ind++)
					//	System.out.print(S6.elementAt(i).elementAt(x).elementAt(ind) +",");	
					System.out.print(S6.elementAt(i).elementAt(x));			
					System.out.println(") "	);		
				for (int h = 0; h<S7.elementAt(i).elementAt(x).size(); h++){
					System.out.print("		S7 "+i + ","+ x + ","+ h+ " = ( "	);	
					//for (int ind = 0; ind<S7.elementAt(i).elementAt(x).elementAt(h).size(); ind++)
					//	System.out.print(S7.elementAt(i).elementAt(x).elementAt(h).elementAt(ind) +",");	
					System.out.print(S7.elementAt(i).elementAt(x).elementAt(h));			
					System.out.println(") "	);			
				}
			}
		}
	}

   private static void afficherS8( Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Vector<Integer>>>>>>>>>> S8, Data data)
  {
  
	System.out.println (" //////////////////////////////////////////////////////// AFFICHAGE S8 ");
	
  // S8 i e f K  a x h o 
	int emax =0 , fmax =0, xmax =0, hmax=0, omax = 0;
	for (int i=0; i<data.ntasks; i++){
		for (int e =0; e< S8.elementAt(i).size(); e++){
			emax = Math.max(emax, S8.elementAt(i).size());
			for (int f=0; f < S8.elementAt(i).elementAt(e).size(); f++){
				fmax = Math.max(fmax, S8.elementAt(i).elementAt(e).size());
				for (int K=0; K< S8.elementAt(i).elementAt(e).elementAt(f).size(); K++){
					for (int a=0; a< S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).size(); a++){
						for (int x =0; x< S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).size(); x++){
							xmax = Math.max(xmax, S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).size());
							for (int h =0; h< S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).size(); h++){
								hmax = Math.max(hmax, S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).size());
								for (int o=0; o <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size(); o++){ 
									omax = Math.max(omax, S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).size());
									System.out.print (" 8 i e f K a x h o  = S8 "+ i + ", " + e + ", " + f + ", " + K + ", " + a + ", " + x + ", " +  h + ", " + o + " =  ( ");
									for (int w=0; w <S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).size(); w++)
										System.out.print (S8.elementAt(i).elementAt(e).elementAt(f).elementAt(K).elementAt(a).elementAt(x).elementAt(h).elementAt(o).elementAt(w) + ", ");
									System.out.println (" ) ");
								}
							}
						}
					}
				}
			}
		}
	}
	System.out.println (" emax fmax xmax hmax omax "+ emax + " " + fmax + " " + xmax + " " +  hmax + " " + omax);

	}

}

