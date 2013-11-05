import ilog.concert.*;
import ilog.cplex.*;
import java.io.*;

public class Isolve {
	static private class Data {
	     
	      
	      double[] plambdas;
	      double[] pfreqs;
	      double[] texes;
	      
	      int[][] succs;
	      
	      int ntasks;
	      int nprocs;
	      int nfreqs;
	      
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
	         pfreqs = reader.readDoubleArray();
	         texes = reader.readDoubleArray();
	         rep  = reader.readInt();
	         succs = reader.readIntArrayArray();
	        	 
	         
	         ntasks = texes.length / plambdas.length;
	         nprocs = plambdas.length;
	         nfreqs = pfreqs.length;
	         
	    
	         Po = reader.readDouble();
	         lo = reader.readDouble();
	         limit = reader.readInt(); 
	      
	         Max = 0.0;
	         for (int j = 0; j < texes.length; j++)
		           Max = Max + texes[j]/pfreqs[0];
	      }
	   }

	
	
	
	
	
	
	public static File create_file(String n){
	try {
        File file = new File(n);
    
        // Create file if it does not exist
        boolean success = file.createNewFile();
        if (success) {
            // File did not exist and was created
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
	// BufferedWriter out = new BufferedWriter(fw);
	  fw.write(n);
	  //Close the output stream
	  fw.close();
	  }catch (Exception e){//Catch exception if any
	  //System.err.println("Error: " + e.getMessage());
	    	System.out.println("n ="+n);

	  e.printStackTrace();
	  System.exit(0);
	  }
	  
	}
	   public static void main (String args[]) {
	      try {
	         String filename;
	         if ( args.length > 1)  filename = args[0];
	         else                 
	        	                    filename = "data/tasks10.txt";

	         Data     data  = new Data(filename);
	        
	         
	       //  for (int l2 = 15; l2 <= 20; l2++) { // 40 value
	        //     for (int l1 = 1; l1 <= 2; l1++) { 
	          //    for (int p2 = 5; p2 <= 24; p2++){ // 20 value
	        
	               
	             
	           
	            	// data.lo = l1 * 4 * Math.pow(10,-(21-l2)) ;
	            	 
	 	            // data.Po =  p2 * 0.2;
	 	             System.out.println("Lambda obj = "+data.lo);
	 	             System.out.println("Power obj = "+data.Po);

	         IloCplex cplex = new IloCplex();

	         // Create start variables
	         IloNumVar[][] t = new IloNumVar[data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
	            t[j] = cplex.numVarArray(data.rep, 0.0, Double.MAX_VALUE);
	         
	         IloIntVar[][][] x3 = new IloIntVar[data.ntasks][data.rep][data.nprocs];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		 	            x3[j][k] = cplex.intVarArray(data.nprocs, 0, 1);

	         IloIntVar[][][][] x4 = new IloIntVar[data.ntasks][data.rep][data.nprocs][data.nfreqs];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.nprocs; l++)
		        			 x4[j][k][l] = cplex.intVarArray(data.nfreqs, 0, 1);

	         IloNumVar[][] P = new IloNumVar[data.ntasks][];
	         for (int j = 0; j < data.ntasks; j++)
	            P[j] = cplex.numVarArray(data.rep, 0, data.nprocs);
	         
	         IloIntVar[][][][] g = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.ntasks; l++)
		        			 g[j][k][l] = cplex.intVarArray(data.rep, 0, 1);
	         
	        
	         IloIntVar[][][][] v = new IloIntVar[data.ntasks][data.rep][data.ntasks][data.rep];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int k = 0; k < data.rep; k++)
		        	 for (int l = 0; l < data.ntasks; l++)
		        			 v[j][k][l] = cplex.intVarArray(data.rep, 0, 1);
	         
	         //X j K a b
	         IloIntVar[][][][] X = new IloIntVar[data.ntasks][data.rep+1][(int)Math.pow(data.nprocs, data.rep)][(int)Math.pow(data.nfreqs, data.rep)];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int K = 0; K <= data.rep; K++)
		        	 for (int a = 0; a < (int)Math.pow(data.nprocs, data.rep); a++)
			           for (int b = 0; b < (int)Math.pow(data.nfreqs, data.rep); b++)
		        			 X[j][K][a] = cplex.intVarArray((int)Math.pow(data.nfreqs, data.rep), 0, 1);
	         
	         
	         //Y j K 
	         IloIntVar[][] Y = new IloIntVar[data.ntasks][data.rep+1];
	         for (int j = 0; j < data.ntasks; j++)
		        			 Y[j] = cplex.intVarArray(data.rep+1, 0, 1);
	         
	         
	         
	         //XY j K a b
	         IloIntVar[][][][] XY = new IloIntVar[data.ntasks][data.rep+1][(int)Math.pow(data.nprocs, data.rep)][(int)Math.pow(data.nfreqs, data.rep)];
	         for (int j = 0; j < data.ntasks; j++)
		         for (int K = 0; K <= data.rep; K++)
		        	 for (int a = 0; a < (int)Math.pow(data.nprocs, data.rep); a++)
			           for (int b = 0; b < (int)Math.pow(data.nfreqs, data.rep); b++)
		        			 XY[j][K][a] = cplex.intVarArray((int)Math.pow(data.nfreqs, data.rep), 0, 1);
	         
	         //S1 a k
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
			 int[][] S2 = new int[(int)Math.pow(data.nfreqs, data.rep)][data.rep];
				   for (int b = 0; b < (int)Math.pow(data.nfreqs, data.rep); b++){
			    	   	int tmpb = b;
					    for (int k = 0; k < data.rep; k++){
				        	//S2[a][k] = keme_bit_base_nfreqs(a);
					    		S2[b][k] = tmpb%data.nfreqs;
			        			tmpb = tmpb/data.nfreqs;
					     }
				   }
	   
	         IloNumVar W = cplex.numVar(0.0, Double.MAX_VALUE);
	         IloNumVar En = cplex.numVar(0.0, Double.MAX_VALUE);
	         IloNumVar Sum = cplex.numVar(0.0, Double.MAX_VALUE);
	         IloNumVar LSum = cplex.numVar(0.0, Double.MAX_VALUE);
	         IloNumVar logR = cplex.numVar(Double.NEGATIVE_INFINITY, 0.0);


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

	         //t[jj][kk] - t[j][k] - M*g[j][k][jj][kk]>= sum_lm (djl/fm)*x4[j][k][l][m] - M 
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

	        	 					if (data.succs[j][jj]!=0){
	        	 						 assert(j != jj);
	        	 						 assert( data.succs[j][jj] == 1);
		        					     //System.out.println("EXPR 2 = "+expr);

	    	        	 				cplex.addGe(expr,0);
	        	 					}else{	
	        	 						expr.addTerm(-data.Max,g[j][k][jj][kk]);
		        					   //  System.out.println("EXPR 2 = "+expr);

	    	        	 				cplex.addGe(expr,-data.Max);
	        	 				    }
	        	 				  }
	        	 			     }
	        	 			 
	        		    }	        		 
	        	   }
	           }
	         
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
        	 						
        	 					    // System.out.println("EXPR 4 = "+expr);

        	 						cplex.addGe(expr,1);
        	 					}
	        	 	    }
	        	     }
	               }
	           }
	          	 	
	         //g[j][k][jj][kk] = 1 for all jjkk successor of jk
	         
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
		        					   //  System.out.println("EXPR 5 = "+expr);

	        	 						cplex.addEq(expr,1);
	        	 			     }
	        	 			 }
	        		    }	        		 
	        	   }
	           }
	         }
	        	 	
	        	 
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
		     
		     // for each tasks j
		     for (int j = 0; j < data.ntasks; j++) {
		      		IloLinearNumExpr expr = cplex.linearNumExpr(); 
		   	 		expr.addTerm(1, P[j][0]);
				    // System.out.println("EXPR 10 = "+expr);

		   	 		cplex.addRange(1,expr,data.nprocs);  
	          }
		     
		     {
		    IloLinearNumExpr expr = cplex.linearNumExpr(); 
    		expr.addTerm(-1,En);
		    for (int j = 0; j < data.ntasks; j++) {
	        	for (int k = 0; k < data.rep; k++){
		        	 for (int l = 0; l < data.nprocs; l++){
			        	 for (int m = 0; m < data.nfreqs; m++){
			        		 expr.addTerm(+data.texes[data.nprocs*j+l]*data.pfreqs[m]*data.pfreqs[m],x4[j][k][l][m]);
			        	 }
		        	 }

	        	 }
	         }
		    // System.out.println("EXPR 11 = "+expr);

       	    cplex.addEq(expr,0);
		     }
		     {
		    IloLinearNumExpr expr = cplex.linearNumExpr(); 
    		expr.addTerm(1,En);
    		expr.addTerm(-data.Po,W);
		    // System.out.println("EXPR 12 = "+expr);

    		//cplex.addLe(expr,data.Eno);
    		cplex.addLe(expr, 0);

		     }
		     
    		{ IloLinearNumExpr expr = cplex.linearNumExpr(); 
    		expr.addTerm(-1,Sum);
		    for (int j = 0; j < data.ntasks; j++) {
	        	for (int k = 0; k < data.rep; k++){
		        	 for (int l = 0; l < data.nprocs; l++){
			        	 for (int m = 0; m < data.nfreqs; m++){
			        		 expr.addTerm(+data.texes[data.nprocs*j+l]/data.pfreqs[m],x4[j][k][l][m]);
			        	 }
		        	 }
	        	 }
	         }
		    // System.out.println("EXPR 13 = "+expr);

		    cplex.addEq(expr,0);
    		}
    		
    	
    		
    		{
        		IloLinearNumExpr expr = cplex.linearNumExpr(); 
        		expr.addTerm(-1,logR);
        		Double F = 1.0;
    		    for (int j = 0; j < data.ntasks; j++) {
    	        	for (int K = 1; K <= data.rep; K++){
    	        		for (int a=0; a < Math.pow(data.nprocs, K); a++){
        	        		for (int b=0; b < Math.pow(data.nfreqs, K); b++){
        	       		    // System.out.println("---> EXPR 13  : j, K, a, b "+j+" "+ K+ " "+a +" "+b);

        	        			//	(1-F)*XY[j][K][a][b]
        	        			F = 1.0;
        	        			for (int k= 0; k < K; k++){
        	        					F = F*(1-java.lang.Math.exp(-data.plambdas[S1[a][k]]*data.texes[data.nprocs*j+S1[a][k]]/data.pfreqs[S2[a][k]]));
        	        			}
        	       		      //  System.out.println("EXPR 13 : log(1-F)  vaut : "+Math.log(1-F));

        	        			expr.addTerm(Math.log(1-F),XY[j][K][a][b]);
        	        		}

    	        		}
    		        
    	        	 }
    	         }
    		    // System.out.println("EXPR 13  = "+expr);
    		     cplex.addEq(expr,0);
        		}
    	      
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
    		
    		//sum_K YjK = 1  1<=K<=Rep
    		//sum_K K*YjK = sum_kl x3jkl     1<=K<=Rep  0<=k<Rep 0<=l<nproc
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
    		
    		
    		// YjK <= sum_l x3jkl  all  0<=k<K all 1<=K<Rep
    		{        		
    		    for (int j = 0; j < data.ntasks; j++) {

    	        	for (int K = 1; K <= data.rep; K++){
        	        	for (int k = 0; k < K; k++){
        	        		IloLinearNumExpr expr = cplex.linearNumExpr(); 
        	        		expr.addTerm(1,Y[j][K]);
        	        		for (int l = 0; l < data.nprocs; l++){
   			        		 	expr.addTerm(-1, x3[j][k][l]);
        	        		}
            	        	cplex.addLe(expr,0);
        	       		  //  System.out.println("EXPR 17 = "+expr);

        	        	}
    	        	}
    		    }
    		}

    		
    		
    		{
    		    IloLinearNumExpr expr = cplex.linearNumExpr(); 
        		//expr.addTerm(1,logR);
    		   // System.out.println("EXPR 17 = "+expr);
        		//cplex.addGe(expr,Math.log(data.Ro));
    		    expr.addTerm(-1,logR);
        		expr.addTerm(-data.lo,Sum);
        		cplex.addLe(expr,0);

        	}
        		
    		
    		cplex.addMinimize(W);

//		     System.out.println("cplex = "+cplex);
    		//System.out.println("lo = "+data.lo);
    		//System.out.println("Po = "+data.Po);
    		int limit = data.limit; 
    		cplex.setParam(IloCplex.DoubleParam.TiLim, limit);
    		long start = System.currentTimeMillis();
    		Boolean sol = cplex.solve();
    		long end = System.currentTimeMillis();
    		Double time = (end - start)/1000. ;
    		System.out.println(" Schedule tackled in " + time + " seconds using currentTimeMillis");
    		//Boolean sol = false;
	         if ( sol ) {
	             System.out.println(" Status  = " + cplex.getStatus());
	             System.out.println(" Length  = " + cplex.getObjValue());
	             Double lam = -cplex.getValue(logR)/cplex.getValue(Sum);
	             System.out.println(" Lambda  = " + lam);
	             Double power = cplex.getValue(En)/cplex.getValue(W);
	             System.out.println(" Power  = " + power);
	             Double reliability = Math.exp(cplex.getValue(logR));
	             System.out.println(" Reliability  = " + reliability);
	             System.out.println(" Energy  = " + En);
	             System.out.println(" Time  = " + time);



	       // write_file(out_channel1, -cplex.getValue(logR)/cplex.getValue(Sum)+" "); 
	       // write_file(out_channel2, cplex.getValue(En)/cplex.getValue(W)+" "); 
	       // write_file(out_channel3, cplex.getValue(W)+" ");
	       // write_file(out_channel4, data.lo+" ");
	       // write_file(out_channel5, data.Po+" ");
	       // write_file(out_channel6, cplex.getStatus()+" ");
	       // write_file(out_channel7, time+" ");

	         } else {
	        	  System.out.println(" No solution found ");
	        //	write_file(out_channel1, "NaN ");
	        //	write_file(out_channel2, "NaN ");
	        //	write_file(out_channel3, "Nan ");
	        //	write_file(out_channel4, data.lo+" ");
	        //	write_file(out_channel5, data.Po+" ");
	        //	write_file(out_channel6, "NaN "+" ");
	  	      //  write_file(out_channel7, time +" ");
	         }
	         
           

	         cplex.end();
	         
	       //  } // for p2
	              
	         //     write_file(out_channel1, "\n");
	         //     write_file(out_channel2, "\n");
	         //     write_file(out_channel3, "\n");
	         //     write_file(out_channel4, "\n");
	          //    write_file(out_channel5, "\n");
	          //    write_file(out_channel6, "\n");
	          //    write_file(out_channel7, "\n");

	      // } // for l1
	   // } // for l2
	      
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
	   }
	}



