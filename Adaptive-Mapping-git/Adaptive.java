import java.util.*;

// version august 2013
// not done 1 : tiles form and check if a given set of tiles form fit inside the noc grid
// not done 2 : adapt without first flushing pipelines 
// not done 3 : select best quality graph to keep (the period) constraint ?
// comparison with todaes 2012 : a curve of solution in the space (proc,period,energy) instead of a surface==> 
//                          priviliged solutions with constrained period: point in the border of the surface 
//                          whose projection on the plan (proc, period) is the closest to the origin of the axis  
//                          ==> thus not applicable for finding soutions with constrained power resulting in more period : 
//                          here we need points which are in the border whose projection is the farest from the origin 
//                          skiped by todaes 2012 => to generate the surface it should have constrained the procs and the period
//                          and then minimize the power when searching the paretos instead of constraining only the procs and then 
//                          minimizing the period and then then power
//                          we compare (nevertheless, still, anyway) with the period constrained case and adapt will surely be better (why ? phase 1 not optimal, dvfs, ... OVERESTIMATIOM of communications (over maxhops links) even if it is on 1 link, + underestim of comm with one comm at most per link ==> no exploration of comms mappings on links)
// Like BASTEN : we can see a mapping on a given number of processors as a mapping on a given budget on these processors
// 		(utilisation on each pro and each link) so that we can share ressources by using empty slacks for next application 
//		(if sufficient) otherwise we use new procs and links like now
class Adaptive
{  

	// curent active set of applications
	// platform noc of size P

	Vector<GraphPareto> currentAppliSet; //Tab of GraphParetos with their period constaints and their pareto schedules
	Vector<Double> currentAppliSetEnergySaving; // Tab for the stocking the energy saving values with adding processors in the adaptive algo
	int ArchiSize;
	Boolean mFailure;

	public Adaptive(){
		currentAppliSet = new Vector();
		currentAppliSetEnergySaving = new Vector();
		mFailure = false;
	}

	void addApplicationGraphPareto(GraphPareto G){
		assert(G.period>0); // The constraint on the period must be specified
		assert(!currentAppliSet.contains(G));
		currentAppliSet.addElement(G);
		currentAppliSetEnergySaving.addElement(0.0);

	}

	void removeApplicationGraphPareto(GraphPareto G){
		currentAppliSetEnergySaving.removeElementAt(currentAppliSet.indexOf(G));
		currentAppliSet.removeElement(G);
		G.zeroNumberOfAddedProcessors(); // init
	}

	void setArchiSize(int N){
		assert(N>=2);
		this.ArchiSize = N;
	}

	int getArchiSize(){
		return this.ArchiSize;
	}

	int getApplinumber() {

		return currentAppliSet.size();
	}
	Boolean isMappingFailure(){
		return mFailure;
	}

	int getNumberOfUsedProcessors(){
		assert(!isMappingFailure());
		int n = 0;
		for (int i=0; i < currentAppliSet.size(); i++){
			n += currentAppliSet.elementAt(i).getNumberOfNeededProcessors() + currentAppliSet.elementAt(i).getNumberOfAddedProcessors();
		}
		return n;
	}

	int  getNumberOfUnusedProcessors(){
		assert(! isMappingFailure());
		return this.ArchiSize - this.getNumberOfUsedProcessors();
	}

	int getNumberOfNeededProcessors(){
		int n = 0;
		for (int i=0; i < currentAppliSet.size(); i++){
			n += currentAppliSet.elementAt(i).getNumberOfNeededProcessors();
		}
		return n;
	}

	//?????????????
	double getConsumedEnergy(){
		double e =0;
		double periodsppcm = getPeriodsppcm();
		for (int i=0; i < currentAppliSet.size(); i++){
			e += (periodsppcm/currentAppliSet.elementAt(i).getPeriod())*currentAppliSet.elementAt(i).getConsumedEnergy();
		}
		return e;
                //????????????? you should sum energies of each application on the ppcm of the applications periods 
		//????????????? instead of now where you are just summming energies of each application on its period 
	}

	double getPeriodsppcm(){
		for (int i=0; i < currentAppliSet.size(); i++){
			assert(currentAppliSet.elementAt(i).getPeriod() == currentAppliSet.elementAt(0).getPeriod());
		}
		return currentAppliSet.elementAt(0).getPeriod();
	}
	/*double getConsumedPower(){
		double e =0;
		for (int i=0; i < currentAppliSet.size(); i++){
			e += currentAppliSet.elementAt(i).getConsumedEnergy();
		}
		return e;


	} */

	// for each application A do 
	//	assign the min number of procs such the throughput constraint of A is met
	// end for

	// if the sum of the allocated processors is greather than P => Mapping failure
	// end if

	// if there are not unused processors then => return

	// while there are unused processors
	//     	  for each application A
	//		assign the proc to A *** see note 1 and 2 bottom ***
	//              evaluate the energy saving (note 1 and 2)
	//     	  end for
	//        update the schedule of the application with the biggest energy saving with one additional core
	//        decrease the number of unused processors
	// end while

	public void adaptSchedule(){
		mFailure = false; // re-init it before adapting a new set

		for (int i=0; i < currentAppliSet.size(); i++){
			currentAppliSet.elementAt(i).zeroNumberOfAddedProcessors(); // initialize
			currentAppliSetEnergySaving.setElementAt(-1.0, i); // intialize
		}
	
		int n = getNumberOfUsedProcessors();
		if (ArchiSize < n) {
			mFailure = true;
			System.out.println("Mapping Failure.");
			//System.exit(0);
			return;
		}
		assert(ArchiSize >=2);
		if (ArchiSize == getNumberOfUsedProcessors()) return;
        	assert(ArchiSize - getNumberOfUsedProcessors() >= 1);

		while (ArchiSize - getNumberOfUsedProcessors() >= 1) {
			int unUsed = ArchiSize - getNumberOfUsedProcessors();
			int max = -1; // max remains -1 if no energy scurrentAppliSetaving is achieved by adding an additional processor
			for (int i=0; i < currentAppliSet.size(); i++){
				Double EnergyBeforeAdd = currentAppliSet.elementAt(i).getConsumedEnergy(); 
				currentAppliSet.elementAt(i).addNumberOfUsedProcessors(1);
				Double EnergyAfterAdd = currentAppliSet.elementAt(i).getConsumedEnergy();
				Double EnergySaving = -1.0;
				if (EnergyAfterAdd == -1) // no mapping found in the pareto set
					EnergySaving = -1.0;
				else
					EnergySaving = EnergyBeforeAdd - EnergyAfterAdd ; //???????????????????? correct this
				currentAppliSetEnergySaving.setElementAt(EnergySaving, i);
				if (max == -1 &&  EnergySaving > 0 ) max = i;
				if (max > -1 && EnergySaving > currentAppliSetEnergySaving.elementAt(max)) max = i;
			}

			for (int i=0; i < currentAppliSet.size(); i++){
				if (i != max) {
					currentAppliSet.elementAt(i).subNumberOfUsedProcessors(1);
					currentAppliSetEnergySaving.setElementAt(-1.0, i);
				}
			}

			if (max >= 0) assert( ArchiSize - getNumberOfUsedProcessors() == unUsed - 1 );
			else {
				assert( ArchiSize - getNumberOfUsedProcessors() == unUsed );
				//mFailure = true;
				break;
			}
			 // number of unsed processors  is decremented as expected (compared to its intial value) 
			 // because one additional processor is given to the application that has the biggest energy 
                         // saving with one additional processor
		}
	} // end adaptSchedule



       public static void main(String args[])
        {

		GraphPareto G1 = new GraphPareto( 80.0, "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1bisperiod_rep1_nbop8.txt", "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1bisnbproc_rep1_nbop8.txt","/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G1bispower_rep1_nbop8.txt" );

		System.out.println("vperiods : ");GraphPareto.print(G1.vperiods);
		System.out.println("vnprocs : ");GraphPareto.print(G1.vnprocs);
		System.out.println("vcenergy : ");GraphPareto.print(G1.vcenergy);

		System.out.println("Period constraint :  "+ G1.period);
		System.out.println("---> Number of Used processors : "+ G1.getNumberOfUsedProcessors());
		System.out.println("---> Consumed Energy : "+ G1.getConsumedEnergy());



		GraphPareto G2 = new GraphPareto( 53.0, "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G2period_rep1_nbop8.txt", "/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G2nbproc_rep1_nbop8.txt","/home/popart/assayad/Desktop/Desktop/ilp-results-adapt-optimal/8op/G2power_rep1_nbop8.txt" );

		System.out.println("vperiods : ");GraphPareto.print(G2.vperiods);
		System.out.println("vnprocs : ");GraphPareto.print(G2.vnprocs);
		System.out.println("vcenergy : ");GraphPareto.print(G2.vcenergy);

		System.out.println("Period constraint :  "+ G2.period);
		System.out.println("---> Number of Used processors : "+ G2.getNumberOfUsedProcessors());
		System.out.println("---> Consumed Energy : "+ G2.getConsumedEnergy());



		Adaptive adaptiveMapping = new Adaptive();
		adaptiveMapping.addApplicationGraphPareto(G1);
		adaptiveMapping.addApplicationGraphPareto(G2);
		adaptiveMapping.setArchiSize(6);
		System.out.println("---------------------- Adaptive Mapping --------------------");
		adaptiveMapping.adaptSchedule();
		System.out.println("---> Success ? "+ !adaptiveMapping.isMappingFailure());
		if (!adaptiveMapping.isMappingFailure()) {
			System.out.println("---> Number of Used processors ? "+ adaptiveMapping.getNumberOfUsedProcessors());
			System.out.println("---> Number of Unused processors ? "+ adaptiveMapping.getNumberOfUnusedProcessors());
			System.out.println("---> Consumed Energy ? "+ adaptiveMapping.getConsumedEnergy());
		} else {
			System.out.println("---> Number of Needed processors ? "+ adaptiveMapping.getNumberOfNeededProcessors());
		}

        }
} // end adaptive






/******************************************************************************
// note 1 :  there are 2x2 cases to consider :  - assign the additional proc to A without 
//           touching the mapping of tasks of other appication
//                                              - assign the additional proc to A and moving
//           the tasks of other adjacent applications 
//           <!-- and then for each of the two cases there are two new cases:
//                                               - assign the other proc to A without reexecuting
//                                                 the finished tasks : it means that a part of
//                                                 A will be scheduled according to pareto schedule s1
//                                                 and the other part will be scheduled according to pareto
//                                                 schedule s2 using the additonal proc
//                                               - assign the other proc to A and then reecute A form
//                                                 start to fully apply the pareto schedule -->
//            neither one or other : you do not touch the schedule of the current ith iteration, 
//                                   you apply the new pareto schedule in the next i+1th iteration

// note 2 : First assign the additional proc and form the best energy saving tile and 
//          if not possible try the next best energy saving tile format, .... and so on  */
