/* Example of a GraphPareto and pareto set files :
MILP pareto set for GraphPareto G1 whose file is /home/popart/assayad/workspace/ILPINRIA-2011-2012-batch-et-simple/data/FMDEP : 
/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1period_rep1_nbop8.txt
/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1nbproc_rep1_nbop8.txt
/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1power_rep1_nbop8.txt
*/
class GraphPareto {

	Double period; // constraint
	int numberOfAddedProcessors; // added to needed to achieve better energy while keeping constraint
	int numberOfNeededProcessors; // needed to achieve the constraint above
	Double consumedEnergy; // consumed energy when using needed + added if any
	double[][]  vperiods; 
	int[][] vnprocs; 
	double[][] vcenergy;

	public GraphPareto(Double period, String fvperiods, String fvnprocs, String fvcenergy){
		this.period = period;
		this.numberOfAddedProcessors = 0;
		this.numberOfNeededProcessors = -1;
		try {
			SInputDataReader reader = new SInputDataReader(fvperiods);
			vperiods = reader.readDoubleArrayArray();
			reader = new SInputDataReader(fvnprocs);
			vnprocs = reader.readIntArrayArray();
			reader = new SInputDataReader(fvcenergy);
			vcenergy = reader.readDoubleArrayArray();
		}catch ( Exception ex ) {
			ex.printStackTrace();
		}
		computeNumberOfNeededProcessorsAndConsumedEnergy(period); // from pareto set
		
	}

	
	double getPeriod(){
		assert(period >0);
		return this.period;
	}
	void addNumberOfUsedProcessors(int n){
		assert(n==1);
		numberOfAddedProcessors += n;
		updateConsumedEnergy(period, numberOfNeededProcessors + numberOfAddedProcessors ); // from pareto set
	
	}

	void subNumberOfUsedProcessors(int n){
		assert(n==1);
		numberOfAddedProcessors -= n;
		updateConsumedEnergy(period, numberOfNeededProcessors + numberOfAddedProcessors); // from pareto set
	}

	int getNumberOfNeededProcessors (){
		return numberOfNeededProcessors;
	}


	int getNumberOfAddedProcessors (){
		return numberOfAddedProcessors;
	}


	void zeroNumberOfAddedProcessors (){
		int n=getNumberOfAddedProcessors();
		this.numberOfAddedProcessors -= n;
		updateConsumedEnergy(period, numberOfNeededProcessors + numberOfAddedProcessors); // from pareto set
	}

	// instead of that, use :  get needed + get added
	void setNumberOfUsedProcessors(){
		assert(false);	

	}



	int getNumberOfUsedProcessors(){
		return numberOfAddedProcessors + numberOfNeededProcessors;
	}

	Double getConsumedEnergy(){
		return this.consumedEnergy;
	}
	

	private void computeNumberOfNeededProcessorsAndConsumedEnergy(double period){
		// First line correspond to one proc, second line corresponds to two processors
		// First column correspond to the biggest period, and the last column to smallest period
		Boolean schedulefound = false;
		for (int i=0; i< vperiods.length; i++){
			for (int j=0; j< vperiods[i].length; j++){
				// look for a pareto with a non NaN value and with a schedule period equal to the constraint 
				if (vperiods[i][j] != -1.0 && vperiods[i][j] <= this.period){ // non NaN value and the period of the pareto schedule is equal to the constraint 
					// we took the greatest period which is less or equal the wanted period
					assert(vnprocs[i][j] > 0 ); 
					assert(vnprocs[i][j] <= i+1 ); 
					this.numberOfNeededProcessors = vnprocs[i][j];
					assert(vcenergy[i][j] > 0);
					this.consumedEnergy = vcenergy[i][j]*vperiods[i][j]  ;
					schedulefound = true;
					break;
				}
			}
			if (schedulefound) break;
		}
		if (! schedulefound) { System.out.println("period "+ period); assert(false); } // no pareto found 
	}


	private void updateConsumedEnergy(double period, int numberOfUsedProcessors){
		// First line correspond to on		GraphPareto.print(this.vnprocs);e proc, second line corresponds to two processors
		// First column correspond to the biggest period, and the last column to smallest period
		Boolean schedulefound = false;
		for (int i=0; i< vperiods.length; i++){
			for (int j=0; j< vperiods[i].length; j++){
				// look for a pareto with a non NaN value and with a schedule period equal to the constraint 
				if (vperiods[i][j] != -1.0 && vperiods[i][j] <= this.period){ // non NaN value and the period of the pareto schedule is equal to the constraint 
					// we took the greatest period which is less or equal the wanted period
					assert(vnprocs[i][j] > 0 ); 
					assert(vnprocs[i][j] <= i+1 ); 
					if (vnprocs[i][j] == numberOfUsedProcessors) {
						assert(vcenergy[i][j] > 0);
						this.consumedEnergy = vcenergy[i][j]*vperiods[i][j]; // because in fact cplex program gives power so we multiply it by the period to get the energy
						schedulefound = true;
						break;
					}
				}
			}
			if (schedulefound) break;
		}
		if (! schedulefound) this.consumedEnergy = -1.0;
	}

	static void  print(double[][] vperiods){
		for (int i=0; i< vperiods.length; i++){
			for (int j=0; j< vperiods[i].length; j++)
				System.out.print("	"+vperiods[i][j]+ " ");
			System.out.println("");
		}
	}

	static void  print(int[][] vnprocs){
		for (int i=0; i< vnprocs.length; i++){
			for (int j=0; j< vnprocs[i].length; j++)
				System.out.print("	"+vnprocs[i][j]+ " ");
			System.out.println("");
		}
	}
	
	// MILP results trace file scan
	public static void mainGraphPareto2 (String args[]) {
		GraphPareto G1 = new GraphPareto( 80.0, "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1bisperiod_rep1_nbop8.txt", "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1bisnbproc_rep1_nbop8.txt","/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1bispower_rep1_nbop8.txt" );
		System.out.println("vperiods : ");GraphPareto.print(G1.vperiods);
		System.out.println("vnprocs : ");GraphPareto.print(G1.vnprocs);
		System.out.println("vcenergy : ");GraphPareto.print(G1.vcenergy);

		System.out.println("Period constraint :  "+ G1.period);
		System.out.println("---> Number of Used processors : "+ G1.getNumberOfUsedProcessors());
		System.out.println("---> Consumed Energy : "+ G1.getConsumedEnergy());
	}

	// TODAES results trace file scan
	public static void mainGraphPareto1(String args[]) {
		GraphPareto G1 = new GraphPareto( 100.0, "/home/popart/assayad/Desktop/Adaptive-todaes-results/8op-tuilescarre/G1bisperiod_obj_nbop8.txt", "/home/popart/assayad/Desktop/Adaptive-todaes-results/8op-tuilescarre/G1bisnbproc_nbop8.txt","/home/popart/assayad/Desktop/Adaptive-todaes-results/8op-tuilescarre/G1bispower_nbop8.txt" );
		System.out.println("vperiods : ");GraphPareto.print(G1.vperiods);
		System.out.println("vnprocs : ");GraphPareto.print(G1.vnprocs);
		System.out.println("vcenergy : ");GraphPareto.print(G1.vcenergy);

		System.out.println("Period constraint :  "+ G1.period);
		System.out.println("---> Number of Used processors : "+ G1.getNumberOfUsedProcessors());
		System.out.println("---> Consumed Energy : "+ G1.getConsumedEnergy());
	}

} // end GraphPareto
