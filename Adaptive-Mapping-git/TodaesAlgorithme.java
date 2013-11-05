
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */


import java.util.*;
import java.io.*;

public class TodaesAlgorithme {
         public static double fin_dern_comm;
	// Contains (tiles - 1) + (tiles - 2) + .... (tiles -1) temporary mapping using tiles procs 
        // todaes algo will choose the best element from them and put it inside todaesBestMappings
	// ex : <<1 2>  <3>, <1> <2 3>>
	public static Vector<Vector<Vector<String>>>  todaesMappings= null;; // temporary solutions from which paretos are chosen by toadesAlgo		

       public static ArrayList<Chemin> chemins=new ArrayList<Chemin>();
	public static ArrayList<Lien> liens_temp=new ArrayList<Lien>(); 
	public ArrayList<Lien> copie_liens=new ArrayList<Lien>(); 
	
	public static double[][] UtilisationDesLiensTodaesFictifsDeLongeurMaxHops = null; // utilisation for todaes logical links of maxhop real links
	public static double[] UtilisationDesLiensTodaesExact = null; // utilisation for real links

	// Init the set ToadesBestMappings with one mapping containing as tiles as operations number
	// The initial content is : <<<1> <2> <3>>>
	public void initTodaesBestMappings(){
		assert(TodaesMain.todaesBestMappings == null );
		TodaesMain.todaesBestMappings = new Vector();
		Vector<Vector<String>> onemapping = new Vector();
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			Vector opids= new Vector();
			
			opids.addElement(TodaesMain.todaesOperations[i].getLibelle()); // vector <f>
			//assert(TodaesMain.todaesOperations[i].getLibelle() == i); // is it ?
			onemapping.addElement(opids); // vector <..... <f>>
		}
		assert(onemapping.size()==TodaesMain.todaesOperations.length);
		TodaesMain.todaesBestMappings.addElement(onemapping);// vector < <.....<f>>
	}

	// Best -with regards period- solution using tiles proc
        // follow algorithm 1 in acm todaes paper (a. k. singh et al)
	public void findBestSolutionToPutInTodaesBestMappings(int tiles){

		assert(todaesMappings == null); 
		assert(tiles <= TodaesMain.todaesOperations.length - 1);
		assert(tiles >= 1);
		assert(TodaesMain.todaesBestMappings.size()==TodaesMain.todaesOperations.length - (tiles+1) + 1);
		
		todaesMappings = new Vector();
		// look for the mapping who has tiles+1 proc (there is only one) inside TodaesBestMappings
                // it is simply an element of TodaesBestMappings whose size is tiles+1
		int best = -1;
		for (int i=0; i< TodaesMain.todaesBestMappings.size(); i++){
			if (TodaesMain.todaesBestMappings.elementAt(i).size() == tiles+1) {
				best = i;
				break;
			}
		}
		assert(best >=0);
		// then compute recursively all its possible sub-mapping and store them inside todaesMappings set by doing this :
		

		Vector<Vector<String>> todaesBestMappingElement = TodaesMain.todaesBestMappings.elementAt(best);
		
			       
		//recursive function call computeTodaesSubMappings with two paremeters : the (best) tiles+1 mapping and the 
		//first empty temporary set of Mappings
		computeTodaesSubMappings(todaesBestMappingElement, tiles+1, todaesMappings);
		// loook for the best inside the temporary todaesMappings and put it inside toadesBestMappings

		putBestSolutionInTodaesBestMappings();
		// put to null the temprary set todaesMappings
		todaesMappings.clear(); todaesMappings=null;
	}

	// todaesBestMappingElement : mapping with tiles proc
	// we look for tmp, the set of mappings with tile-1 proc 
 	void computeTodaesSubMappings(Vector<Vector<String>>  todaesBestMappingElement, int tiles, Vector<Vector<Vector<String>>>  tmp){
		assert(todaesBestMappingElement.size() == tiles) ;
		
		assert (todaesBestMappingElement.size() >= 2);


		for (int  i=1; i< tiles; i++){

			// create a clone of the best mapping 
			Vector<Vector<String>> todaesBestMappingElementClone = (Vector<Vector<String>>) deepClone(todaesBestMappingElement);
			//2- then you take the content of the first element v0 of the clone and 3- add it to the second element v1 4- then remove v0 
                	//from the clone,
		 	Vector<String> v0 = todaesBestMappingElementClone.elementAt(0);
			assert(todaesBestMappingElementClone.size()>=1);

			todaesBestMappingElementClone.elementAt(i).addAll(v0); // side efect : modifies original (tiles+1) todaesBestMappingElement i-th element because it is not cloned by the clone method (see vector clone api)

			todaesBestMappingElementClone.remove(0);

			//5- put then the clone inside the temporary todaesMappings
			tmp.add(todaesBestMappingElementClone); 

			// repeat the 5 actions above but this time use v2 instead of v1
			// ...
			// repeat the 5 actions above but this time use vtiles instead of vtiles-1
		}
			       
		if (tiles-1 >= 2) {
			//7- create a new clone and remove v0 from it
			Vector<Vector<String>> todaesBestMappingElementClone2 = (Vector<Vector<String>>) todaesBestMappingElement.clone(); // deepClone not needed, because no side effect due to submappings computations hereafter
			todaesBestMappingElementClone2.remove(0);
			//8- add to the end of the function a recursive call with as parameter the new clone above and a second temporary set of Mappings tmp
			Vector<Vector<Vector<String>>>  tmp2 = new Vector();

			computeTodaesSubMappings(todaesBestMappingElementClone2, tiles-1, tmp2);
			//9- take the second temporary set of Mappings content, append v0 to it at the BEGINING, and put the content of the set inside the 
			//first temporary set. In fact since comm are uniform in TODAES it doesn matter if we add v0 at the beginning or at the end of vector so
        	        // add at the end (?)
			assert(tmp2.size()>=1);
			for (int j=0; j< tmp2.size(); j++) {
				tmp2.elementAt(j).add(todaesBestMappingElement.elementAt(0)); // no side effect here for the original todaesBestMappingElement
			}
			tmp.addAll(tmp2);
			//10- nullify the second temporary set
			tmp2.clear(); tmp2=null;
		} else return; // recursive stop condition : check if the tiles+1 mapping is not reduced to one element if it is the case exit the function call
	}

	// look for the best inside the temporary todaesMappings and put it inside toadesBestMappings
	void putBestSolutionInTodaesBestMappings(){
		assert(todaesMappings != null);
		assert(todaesMappings.size() >= 1);
		assert(TodaesMain.todaesBestMappings != null);	
		assert(TodaesMain.todaesBestMappings.size() >= 1);	
		int best = 0;

		double bestPeriod =  computeTodaesPeriod(todaesMappings.elementAt(best));


		for (int i=0; i<todaesMappings.size(); i++){
			assert(todaesMappings.elementAt(i).size() == todaesMappings.elementAt(0).size());
			//double consumedEnergy = computeConsumedenergy(todaesMappings.elementAt(i)); // compute energy ???
			double period = computeTodaesPeriod(todaesMappings.elementAt(i));
			if (period < bestPeriod) {
				bestPeriod = period;
				best = i;
			}
		}


		//System.out.println("Adding best map, period "+bestPeriod);

		TodaesMain.todaesBestMappings.add(todaesMappings.elementAt(best));

		/*System.out.println("Checking best periods : ");
		for (int i=0; i < TodaesMain.todaesBestMappings.size()  ; i++){
	         	System.out.println("		best periods : "+TodaesAlgorithme.computeTodaesPeriod(TodaesMain.todaesBestMappings.elementAt(i)));
		} */


	}

	
	static double  computeTodaesPeriod(Vector<Vector<String>> todaesMappingElement){
		double period = -1;
		assert(todaesMappingElement.size() >= 1 );
		// deepClone in order not to affect attributes of operations and coms of todaesMappingElementp with temporary values
		//Vector<Vector<String>> todaesMappingElement = (Vector<Vector<String>>) deepClone(todaesMappingElementp);

		for (int i=0; i<todaesMappingElement.size(); i++){
			double sum = 0;
			
			assert(todaesMappingElement.elementAt(i).size() >=1);
			for (int j=0; j< todaesMappingElement.elementAt(i).size(); j++){
				// in todaes i value is not important because exec times are the same on todaes GPP ???
				sum += TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).getDurees()[0]; // if we put i instead of 0 
															       // it dooesn t work when 
															       // num proc < num op 
															       // because duree is then undefined
			}
			assert(sum > 0);
			if (sum > period) period = sum;
		}

		//date de debut/fin de chaque operation : d'abord sur chaque proc ordonner les operations de telle sorte qu'il 
		//n y ait pas de sucesseur avant predecesseur ensuite les ordonnancer dans l'ordre ou elles apparaissent dans todaesMappingElement
		// (on aura besoin des dates de fin des operations pour calucler les dates de debut/fin des com apres
		order_operations_on_tiles(todaesMappingElement);

		// clone liens and arcs. because side effect on links (myAjouterCommunication) and on arcs (setDebut, setFin , etc)
	/*	ArrayList<Lien> cliens = (ArrayList<Lien>) deepClone(TodaesMain.liens);
		ArrayList<Arc> cArcs = (ArrayList<Arc>) deepClone(TodaesMain.Arcs); */
		order_comms_on_todaes_links(todaesMappingElement, TodaesMain.Arcs, TodaesMain.liens);

				

		//period des comms ?
		for (int j=0; j < TodaesMain.liens.size(); j++){	
			double fin_derniere_comm = TodaesMain.liens.get(j).getFinDerniereComm();
			if (fin_derniere_comm > period){
						System.out.println(" computeTodaesPeriod fin_derniere_comm " + fin_derniere_comm);
						period = fin_derniere_comm;
			}
		}
		/*
		if (todaesMappingElement.size() >= 2){ // s il y a au moins deux procs dans le mapping on peut parler des coms
			int maxhops = computeMaxHops(todaesMappingElement.size()); // equal to : width-1 + height-1
			// recuperer la liste des arcs et pour chaque arc si procs des operations sont differents faire 
			// placer la comm dans le lien logique Todaes de longueur maxhops (liens supposes disjoints si differents) 
			// a la fin regarder la somme des comm de chaque lien logique et si cette somme depasse la valeur de period
			// alors mettre a jour periode a cette valeur

			//init
			assert(UtilisationDesLiensTodaesFictifsDeLongeurMaxHops == null);
			UtilisationDesLiensTodaesFictifsDeLongeurMaxHops=new double[todaesMappingElement.size()][todaesMappingElement.size()]; 
			for(int i=0;i< UtilisationDesLiensTodaesFictifsDeLongeurMaxHops.length;i++){
				for(int j=0;j< UtilisationDesLiensTodaesFictifsDeLongeurMaxHops.length;j++)
					UtilisationDesLiensTodaesFictifsDeLongeurMaxHops[i][j]=0;
			}
			//fill
			for(int i=0;i<TodaesMain.Arcs.size();i++){ 
				int p1 = getProc(TodaesMain.Arcs.get(i).getOpSource(), todaesMappingElement);
				int p2 = getProc(TodaesMain.Arcs.get(i).getOpDestination(), todaesMappingElement);
				assert(p1 < todaesMappingElement.size());
				assert(p2 < todaesMappingElement.size());

				if (p1 != p2){
					assert(TodaesMain.liens.size()>0);
					UtilisationDesLiensTodaesFictifsDeLongeurMaxHops[p1][p2] += TodaesMain.Arcs.get(i).getDurees()[0];
					UtilisationDesLiensTodaesFictifsDeLongeurMaxHops[p2][p1] += TodaesMain.Arcs.get(i).getDurees()[0];
				}
					
			}
			//use
			assert(UtilisationDesLiensTodaesFictifsDeLongeurMaxHops.length >=2);
			for(int i=0;i< UtilisationDesLiensTodaesFictifsDeLongeurMaxHops.length;i++){
				for(int j=i+1;j< UtilisationDesLiensTodaesFictifsDeLongeurMaxHops.length;j++){
					assert(i!=j); // utilisation d'une connexion logique entre deux procs
					if (UtilisationDesLiensTodaesFictifsDeLongeurMaxHops[i][j] > period)
						period = UtilisationDesLiensTodaesFictifsDeLongeurMaxHops[i][j];
				}
			} 
			UtilisationDesLiensTodaesFictifsDeLongeurMaxHops = null;
	
		}
	
		*/
		clear_procs_liens_arcs();
		assert(period >0);
		return period;
	}

	// Note : side effect on operations and operators of todaesMappingElement. If not wanted make deep clone of it
	static void  order_operations_on_tiles(Vector<Vector<String>> todaesMappingElement){
		
		//First sur chaque proc ordonner les operations de telle sorte qu'il 
		//n y ait pas de sucesseur avant predecesseur
		for (int i=0; i<todaesMappingElement.size(); i++){
			for (int j=0; j< todaesMappingElement.elementAt(i).size(); j++){
				for (int k=j+1; k< todaesMappingElement.elementAt(i).size(); k++){
					// op j is a sucessor of op k : not possible so insert k before j in the set todaesMappingElement.elementAt(i) and then remove k
					if(TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).isSuccessor(TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(k)))){
						todaesMappingElement.elementAt(i).insertElementAt(todaesMappingElement.elementAt(i).elementAt(k), j);
						j=j+1;
						todaesMappingElement.elementAt(i).remove(k+1);
					}
				}
			}
		}
			 
		//Then Fill attributes debut/fin/listOpExecute of operations and operators	
		for (int i=0; i<todaesMappingElement.size(); i++){
			for (int j=0; j< todaesMappingElement.elementAt(i).size(); j++){
				TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).setPosteExecutant(TodaesMain.operateurs[i]);
				int taille = TodaesMain.operateurs[i].getListOpExecute().size();
				double Duree_LastOpSurPoste;
				if(taille==0){
					TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).setDebut(0);
					Duree_LastOpSurPoste=0;
				}
				else{
					Duree_LastOpSurPoste=TodaesMain.operateurs[i].getListOpExecute().get(taille-1).getFin();
				}
				TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).setDebut(Duree_LastOpSurPoste);
				TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).setFin(Duree_LastOpSurPoste+(TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).getDurees()[0])); 	
				TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).setPlace(1); 
				TodaesMain.operateurs[i].AjouterOperation(TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)));
			}
		}
	}

	// must call order_operatins_on_tiles before that function
	static void  order_comms_on_exact_todaes_links(Vector<Vector<String>> todaesMappingElement, ArrayList<Arc> cArcs, ArrayList<Lien> cliens){
		// order the set of arcs according to the begining time of the destination operation 
		// for equal destination operation begining time order them on end time of source operation
		// then for each arc compute its chemin, l1.debut, l1.fin, l2.debut, l2.fin, .... , lk.debut, lk.fin where l1...lk are links of the path
		// chemin = l1,....,lk : Todaes shortest path from source to destination in terms of hops number 
		// l0.debut : max( fin op source, fin derniere comm sur l0)
		// li.debut : max (li-1.fin , fin derniere comm sur li)
		// li.fin = li.debut+li.duree 
		assert(cArcs.size() >= 1);
		for (int j=0; j<cArcs.size(); j++){
			for (int k=j+1; k< cArcs.size(); k++){
				// arc k dest op begin time is before that of j : not possible so insert k before j in the set cArcs  and then remove k
				if(cArcs.get(k).getOpDestination().getDebut() < cArcs.get(j).getOpDestination().getDebut()){
					cArcs.add(j, cArcs.get(k));
					j=j+1;
					cArcs.remove(k+1);
				}else if (cArcs.get(k).getOpDestination().getDebut() == cArcs.get(j).getOpDestination().getDebut()){
					//For equal begin time of destinations we see the end time of sources
					if (cArcs.get(k).getOpSource().getFin() < cArcs.get(j).getOpSource().getFin()){
						cArcs.add(j, cArcs.get(k));
						j=j+1;
						cArcs.remove(k+1);										
					}
				}
			}
		}
		
		for (int i=0; i<cArcs.size(); i++){
			int p1 = getProc(cArcs.get(i).getOpSource(), todaesMappingElement);
			int p2 = getProc(cArcs.get(i).getOpDestination(), todaesMappingElement);	
			if (p1 != p2){
				TodaesMain.todaesAlgo.chemins = Chemin.Chemins(p1,p2, cliens); 
				ArrayList<Lien> shortest = computeShortestChemin().getChemin();
				for (int j=0; j < shortest.size(); j++){	
					double debut = ((j==0) ? cArcs.get(i).getOpSource().getFin() : shortest.get(j-1).getFinDerniereComm());
					shortest.get(j).myAjouterCommunication(cArcs.get(i), debut, todaesMappingElement);
				}
				TodaesMain.todaesAlgo.chemins = null;
			}
		}

	}


// must call order_operatins_on_tiles before that function
	static void  order_comms_on_todaes_links(Vector<Vector<String>> todaesMappingElement, ArrayList<Arc> cArcs, ArrayList<Lien> cliens){
		// order the set of arcs according to the begining time of the destination operation 
		// for equal destination operation begining time order them on end time of source operation
		// then for each arc compute its chemin, l1.debut, l1.fin, l2.debut, l2.fin, .... , lk.debut, lk.fin where l1...lk are links of the path
		// chemin = l1,....,lk : Todaes shortest path from source to destination in terms of hops number 
		// l0.debut : max( fin op source, fin derniere comm sur l0)
		// li.debut : max (li-1.fin , fin derniere comm sur li)
		// li.fin = li.debut+li.duree 
		assert(cArcs.size() >= 1);
		for (int j=0; j<cArcs.size(); j++){
			for (int k=j+1; k< cArcs.size(); k++){
				// arc k dest op begin time is before that of j : not possible so insert k before j in the set cArcs  and then remove k
				if(cArcs.get(k).getOpDestination().getDebut() < cArcs.get(j).getOpDestination().getDebut()){
					cArcs.add(j, cArcs.get(k));
					j=j+1;
					cArcs.remove(k+1);
				}else if (cArcs.get(k).getOpDestination().getDebut() == cArcs.get(j).getOpDestination().getDebut()){
					//For equal begin time of destinations we see the end time of sources
					if (cArcs.get(k).getOpSource().getFin() < cArcs.get(j).getOpSource().getFin()){
						cArcs.add(j, cArcs.get(k));
						j=j+1;
						cArcs.remove(k+1);										
					}
				}
			}
		}
		int maxhops = computeMaxHops(TodaesMain.operateurs.length); ; // equal to : width-1 + height-1
		for (int i=0; i<cArcs.size(); i++){
			int p1 = getProc(cArcs.get(i).getOpSource(), todaesMappingElement);
			int p2 = getProc(cArcs.get(i).getOpDestination(), todaesMappingElement);	
			if (p1 != p2){
				TodaesMain.todaesAlgo.chemins = Chemin.Chemins(p1,p2, TodaesMain.liens); 
				ArrayList<Lien> shortest = computeMaxHopsChemin(maxhops).getChemin();
				for (int j=0; j < shortest.size(); j++){	
					double debut = ((j==0) ? cArcs.get(i).getOpSource().getFin() : shortest.get(j-1).getFinDerniereComm());
					shortest.get(j).myAjouterCommunication(cArcs.get(i), debut, todaesMappingElement);
				}
				TodaesMain.todaesAlgo.chemins = null;
			}
		}

	}


	static double  computeExactTodaesPeriod(Vector<Vector<String>> todaesMappingElement){
		double period = -1;
		assert(todaesMappingElement.size() >= 1 );
		
		// deepClone in order not to affect attributes of operations and coms of todaesMappingElementp with temporary values
		//Vector<Vector<String>> todaesMappingElement = (Vector<Vector<String>>) deepClone(todaesBestMappingElementp);

		// exec times period : same as  computeTodaesPeriod
		for (int i=0; i<todaesMappingElement.size(); i++){
			double sum = 0;
			assert(todaesMappingElement.elementAt(i).size() >=1);
			for (int j=0; j< todaesMappingElement.elementAt(i).size(); j++){
				sum += TodaesMain.getOperation(todaesMappingElement.elementAt(i).elementAt(j)).getDurees()[0];
			}
			assert(sum > 0);
			if (sum > period) period = sum;
		}
		
		// period des comms : different from computeTodaesPeriod
		// don't use periods computed using todaes logical paths of maxhops links (disjoint links !) 
		// use shortest paths with real links potentially shared between paths like in the real life

		order_operations_on_tiles(todaesMappingElement);

		// clone liens and arcs. because side effect on links (myAjouterCommunication) and on arcs (setDebut, setFin , etc)
	/*	ArrayList<Lien> cliens = (ArrayList<Lien>) deepClone(TodaesMain.liens);
		ArrayList<Arc> cArcs = (ArrayList<Arc>) deepClone(TodaesMain.Arcs); */
		order_comms_on_exact_todaes_links(todaesMappingElement, TodaesMain.Arcs, TodaesMain.liens);

				

		//period des comms ?
		for (int j=0; j < TodaesMain.liens.size(); j++){	
			double fin_derniere_comm = TodaesMain.liens.get(j).getFinDerniereComm();
			if (fin_derniere_comm > period) {
						System.out.println(" computeExactTodaesPeriod fin_derniere_comm " + fin_derniere_comm);
						period = fin_derniere_comm;
			}
		}

		
		/*if (todaesMappingElement.size() >= 2){ // s il y a au moins deux procs dans le mapping on peut parler des coms
			// init 
			assert(UtilisationDesLiensTodaesExact == null);
			UtilisationDesLiensTodaesExact=new double[TodaesMain.liens.size()]; 
			for(int i=0;i< UtilisationDesLiensTodaesExact.length;i++){
					UtilisationDesLiensTodaesExact[i]=0;
			}
			//fill
			for(int i=0;i<TodaesMain.Arcs.size();i++){ 
				int p1 = getProc(TodaesMain.Arcs.get(i).getOpSource(), todaesMappingElement);
				int p2 = getProc(TodaesMain.Arcs.get(i).getOpDestination(), todaesMappingElement);
				assert(p1 < todaesMappingElement.size());
				assert(p2 < todaesMappingElement.size());
				assert(p1 < TodaesMain.operateurs.length); //???? we have 3 operators and 6 operations (so 6 tiles to 1 todaes tiles) => it should nt work when p1 p2 reach 4 or 5 or 6 ????????????  
				assert(p2 < TodaesMain.operateurs.length);


				if (p1 != p2){
					TodaesMain.todaesAlgo.chemins=new ArrayList<Chemin>(); // Remise a 0
					TodaesMain.todaesAlgo.liens_temp=new ArrayList<Lien>();
					// la liste de tous les chemins on supposant que l'archi est completement connecte
					//TodaesMain.todaesAlgo.MinChemin(TodaesMain.operateurs[p1],TodaesMain.operateurs[p2]);
					// remove the links which are not in the arch, then compute shortest path wr number of hops
					//ArrayList<Lien> shortest = removeInexistantCheminsAndComputeShortestChemin().getChemin();
					TodaesMain.todaesAlgo.chemins = Chemin.Chemins(p1,p2, TodaesMain.liens); 
					ArrayList<Lien> shortest = computeShortestChemin().getChemin();
					for (int j=0; j < shortest.size(); j++){	// le chemin pris par cette communication
						UtilisationDesLiensTodaesExact[shortest.get(j).getIdentifiant()]+= TodaesMain.Arcs.get(i).getDurees()[0];
					}
				}
			}
			//use
			assert(UtilisationDesLiensTodaesExact.length >=1);
			for(int i=0;i< UtilisationDesLiensTodaesExact.length;i++){
				if (UtilisationDesLiensTodaesExact[i] > period)
					period = UtilisationDesLiensTodaesExact[i];
			} 
			
			UtilisationDesLiensTodaesExact = null;
		} */

		System.out.println("Period " + period);
		TodaesMain.AfficheTodaesOperation();
		TodaesMain.AfficheLien();
		clear_procs_liens_arcs();
		assert(period >0);
		return period;
	}
	

	static void   clear_procs_liens_arcs() {
		// re-init attributes debut/fin/listOpExecute of operations and operators
		// re-init links communication list. comms are thrown so not necessary to re-init comms too
			
		for (int i=0 ; i< TodaesMain.operateurs.length; i++){
			TodaesMain.operateurs[i].setListOpExecute(new ArrayList<TodaesOperation>());
		}
		
		for (int i=0 ; i< TodaesMain.todaesOperations.length; i++){
			TodaesMain.todaesOperations[i].setDebut(0);
			TodaesMain.todaesOperations[i].setPlace(0); 
			TodaesMain.todaesOperations[i].setFin(0);
			TodaesMain.todaesOperations[i].setPosteExecutant(null);

		}
		
		for (int i=0 ; i< TodaesMain.liens.size(); i++){
			TodaesMain.liens.get(i).setCommunication(new ArrayList<Communication>());
		}
		
	}
	static double  computeTodaesConsumedEnergy(Vector<Vector<String>> todaesBestMappingElement){
		double energy = 0;
		assert(todaesBestMappingElement.size() >= 1 );
		for (int i=0; i<todaesBestMappingElement.size(); i++){
			assert(todaesBestMappingElement.elementAt(i).size() >=1);
			for (int j=0; j< todaesBestMappingElement.elementAt(i).size(); j++){
				// in todaes i value is not important because exec times are the same on todaes GPP ???
				energy += TodaesMain.getOperation(todaesBestMappingElement.elementAt(i).elementAt(j)).getDurees()[0]; 
				// if i put i instead of 0 it doesn't work when num proc < num op 
                                // because "la i-th duree" is then undefined and todaes has computed a mapping with one proc per operation 
                                // in the algorithm
			}
			assert(energy > 0);
		}


		// energy of comms. First we compare our algo with todaes with a restriction to uni-ligne biderectional comms, 
		// that is, if we have p1 p2 p3 p4 then p1 - p2 - p3 - p4, that is no square coms where p4 communicate directly with p1
		// for each operation for each successor add the energy consumption on all the links between proc of source and dest operation
		
		// then for other cases ywe should take the energy of the links of the shortest path in term of hops (since, with
		// regards comms, todaes thinks in terms of the maximum of the shortest number of hops and exclusive usage of links 
		// rather than in terms of exact path of comms resulting in smallest period ) 
		// => that means for TODAES you need to know the path with shortest HOPS between two procs
                //    Then TODAES algo use the max hop for computing energy or exec time of a given inter oeprations comm
		//    so we need to know the max number of hops over all the paths in a given architecture and use this number for computing
		//    energy and period of comms used in the TODAES static algo and TODAES run-time algo
		//    Finally for comparing with our algo we  need also to compute exact todaes period and energies so at the end of runtime mapping 
		//    we must also store the exact energy and period
		// => That is why we need : computeExactTodaesConsumedEnergy, computeTodaesConsumedEnery, 
		//                          computeExactTodaesPeriod, computeTodaesPeriod	



		int maxhops = computeMaxHops(TodaesMain.operateurs.length); ; // equal to : width-1 + height-1
		// recuperer la liste des arcs et pour chaque arc si procs des operations sont differents faire 
		// energy = Duree_arc * maxhops
		for(int i=0;i<TodaesMain.Arcs.size();i++){ 
			int p1 = getProc(TodaesMain.Arcs.get(i).getOpSource(), todaesBestMappingElement);
			int p2 = getProc(TodaesMain.Arcs.get(i).getOpDestination(), todaesBestMappingElement);
			assert(p1 < todaesBestMappingElement.size());
			assert(p2 < todaesBestMappingElement.size());
			if (p1 != p2)
				energy += maxhops*(TodaesMain.Arcs.get(i).getDurees()[0]); // meme duree sur tous les liens pour todaes, 
		}
		assert(energy >0);
		return energy;
	}

	static double  computeExactTodaesConsumedEnergy(Vector<Vector<String>> todaesBestMappingElement){
		double energy = 0;
		assert(todaesBestMappingElement.size() >= 1 );
		//same as computeTodaesConsumedEnergy
		for (int i=0; i<todaesBestMappingElement.size(); i++){
			assert(todaesBestMappingElement.elementAt(i).size() >=1);
			for (int j=0; j< todaesBestMappingElement.elementAt(i).size(); j++){
				energy += TodaesMain.getOperation(todaesBestMappingElement.elementAt(i).elementAt(j)).getDurees()[0]; 
			}
			assert(energy > 0);
		}
		//cette partie est differente de computeTodaesConsumedEnergy. As Todaes says nothing about how to mapp comms on links. I choosed
		//to map a communication on the shortest path not in term on end of execution time of the comms but in term of number of hops between
		//extremities of the path
		for(int i=0;i<TodaesMain.Arcs.size();i++){ 
			int p1 = getProc(TodaesMain.Arcs.get(i).getOpSource(), todaesBestMappingElement);
			int p2 = getProc(TodaesMain.Arcs.get(i).getOpDestination(), todaesBestMappingElement);
			assert(p1 < todaesBestMappingElement.size());
			assert(p2 < todaesBestMappingElement.size());
			if (p1 != p2){
				TodaesMain.todaesAlgo.chemins=new ArrayList<Chemin>(); // Remise a 0
				TodaesMain.todaesAlgo.liens_temp=new ArrayList<Lien>();
				// la liste de tous les chemins on supposant que l'archi est completement connecte
				//TodaesMain.todaesAlgo.MinChemin(TodaesMain.operateurs[p1],TodaesMain.operateurs[p2]);
				// remove the links which are not in the arch, then compute shortest path wr number of hops
				//System.out.println("p1 " + p1 +" p2 "+p2);
				TodaesMain.todaesAlgo.chemins = Chemin.Chemins(p1,p2, TodaesMain.liens); 
				//TodaesMain.AfficheLien();
				//TodaesMain.todaesAlgo.AfficheChemin();
				//ArrayList<Lien> shortest = removeInexistantCheminsAndComputeShortestChemin().getChemin(); 
				ArrayList<Lien> shortest = computeShortestChemin().getChemin();
				for (int j=0; j < shortest.size(); j++){
					energy += TodaesMain.Arcs.get(i).getDurees()[0]; // meme duree sur tous les liens pour todaes, 
				}
			}
		}		
		assert(energy >0);
		return energy;
	}
	


	

	static int getProc(TodaesOperation op, Vector<Vector<String>> todaesBestMappingElement){
		int p = -1;
		assert(todaesBestMappingElement.size() > 0);
		for (int i=0; i<todaesBestMappingElement.size(); i++){
			assert(todaesBestMappingElement.elementAt(i).size() >=1);
			for (int j=0; j< todaesBestMappingElement.elementAt(i).size(); j++){
				if (TodaesMain.getOperation(todaesBestMappingElement.elementAt(i).elementAt(j)).getLibelle().equals(op.getLibelle()))
				 p= i;	
			}
		}	
		assert(p>=0);
		return p;
	}


	static int  computeMaxHops(int mappingsize){
		
		return (int) Math.ceil((double)mappingsize/2); //Retourne l'entier le plus proche, supérieur ou égal au réel passé en paramètre
	}



	// iterate through the set chemins and get the path which has the smallest number of links
	static Chemin computeShortestChemin(){
		assert(TodaesMain.todaesAlgo.chemins.size() >= 1);
		int min = 0; 
		for (int i=0; i< TodaesMain.todaesAlgo.chemins.size(); i++) {
			if (TodaesMain.todaesAlgo.chemins.get(i).getChemin().size() <  TodaesMain.todaesAlgo.chemins.get(min).getChemin().size())
				min = i;
		}

		return TodaesMain.todaesAlgo.chemins.get(min);
	}

	// iterate through the set chemins and get the path which has the smallest number of links
	static Chemin computeMaxHopsChemin(int maxhops){
		assert(TodaesMain.todaesAlgo.chemins.size() >= 1);
		int min = 0; 
		for (int i=0; i< TodaesMain.todaesAlgo.chemins.size(); i++) {
			if (TodaesMain.todaesAlgo.chemins.get(i).getChemin().size() ==  maxhops)
				min = i;
				break;
		}

		return TodaesMain.todaesAlgo.chemins.get(min);
	}
	// if a path contains a link which does not exist in the archi description file, remove it from the set Chemins of minimal paths
	// then iterate through the set chemins and get the path which has the smallest number of links
	/*static Chemin removeInexistantCheminsAndComputeShortestChemin(){
		assert(TodaesMain.todaesAlgo.chemins.size() >= 1);
		Vector<Integer> toberemoved = new Vector();
		for (int i=0; i< TodaesMain.todaesAlgo.chemins.size(); i++) { // paths
			assert(TodaesMain.todaesAlgo.chemins.get(i).getChemin().size() >= 1);
			for (int j=0; j< TodaesMain.todaesAlgo.chemins.get(i).getChemin().size(); j++) { //links in paths
				if (!TodaesMain.liens.contains(TodaesMain.todaesAlgo.chemins.get(i).getChemin().get(j))){
					toberemoved.add(i);
					break;
				}
			}
		}

		for(int i=0; i < toberemoved.size(); i++){
			TodaesMain.todaesAlgo.chemins.remove(toberemoved.elementAt(i));
			//i = i-1;
		}  
		
		assert(TodaesMain.todaesAlgo.chemins.size() >= 1);
		int min = 0; 
		for (int i=0; i< TodaesMain.todaesAlgo.chemins.size(); i++) {
			if (TodaesMain.todaesAlgo.chemins.get(i).getChemin().size() <  TodaesMain.todaesAlgo.chemins.get(min).getChemin().size())
				min = i;
		}

		return TodaesMain.todaesAlgo.chemins.get(min);
	}
	*/

	static public Object deepClone(Object oldObj) {

		ObjectOutputStream oos = null;
    		ObjectInputStream ois = null;
    		try {
        		ByteArrayOutputStream bos = new ByteArrayOutputStream(); // A
        		oos = new ObjectOutputStream(bos); // B
        		// serialize and pass the object
        		oos.writeObject(oldObj); // C
        		oos.flush(); // D
        		ByteArrayInputStream bin = new ByteArrayInputStream(bos.toByteArray()); // E
        		ois = new ObjectInputStream(bin); // F
        		// return the new object
       			 return ois.readObject(); // G
 		   } catch (Exception e) {
    			    System.out.println("Exception in ObjectCloner = " + e);
    			  //  throw (e);
  		  } finally {
    			try{
      		  		oos.close();
       		 		ois.close();
			} catch (Exception e) {
    			    System.out.println("Exception in ObjectCloner = " + e);
    			  //  throw (e);	
  		  	} 
   		 }
		return null;
	}

/*public int OperationAPlace() throws NOSOLFOUNDEXCEPTION{ 
		int id_operation=0;
		for(int i=1;i<TodaesMain.opPrete.size();i++){
			if(TodaesMain.opPrete.get(i).FonCout().Cout > TodaesMain.opPrete.get(id_operation).FonCout().Cout) //if(TodaesMain.opPrete.get(i).MinDesCout().Cout > TodaesMain.opPrete.get(id_operation).MinDesCout().Cout)
				id_operation=i;
		}
		
		TodaesMain.en = TodaesMain.opPrete.get(id_operation).FonCout().CoutEnEtapen;
	
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			TodaesMain.len=Math.max((TodaesMain.todaesOperations[i].getPlace()==1)?TodaesMain.todaesOperations[i].getFin():0, TodaesMain.len);
		}
		return id_operation;
	}
	
	public void PlacerOperation(int id_operationAplacer)  throws NOSOLFOUNDEXCEPTION{ 
           int id_PosteExecutant=TodaesMain.opPrete.get(id_operationAplacer).FonCout().getIdPoste();//int id_PosteExecutant=TodaesMain.opPrete.get(OperationPlacee()).FonCout().getIdPoste();
            OperationPlacee(TodaesMain.opPrete.get(id_operationAplacer).getLibelle(),id_PosteExecutant, TodaesMain.opPrete.get(id_operationAplacer).FonCout().getIdFreq());
            TodaesMain.operateurs[id_PosteExecutant].AjouterOperation(TodaesMain.opPrete.get(id_operationAplacer)); //TodaesMain.operateurs[TodaesMain.opPrete.get(id_operationAplacer).MinDesCout().getIdPoste()].AjouterOperation(TodaesMain.opPrete.get(id_operationAplacer));
            TodaesMain.opPrete.remove(id_operationAplacer);
	}
*/
	
	/*
	 *   modify operations 
	 */
	/*public void OperationPlacee(String libelleOp, int id_postExecutant, int id_freq){
		
		int j=-1;
		fin_dern_comm=0;
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){ 
			j=i;
			if(TodaesMain.todaesOperations[i].getLibelle().equals(libelleOp))
				break;
		}
		
		TodaesMain.todaesOperations[j].setPosteExecutant(TodaesMain.operateurs[id_postExecutant]);
		
		System.out.println(TodaesMain.operateurs[id_postExecutant]);
		
		assert(TodaesMain.operateurs[id_postExecutant].getFrequences()[id_freq] != 0.0);

		TodaesMain.todaesOperations[j].setFreq(TodaesMain.operateurs[id_postExecutant].getFrequences()[id_freq]);
		

		Arc arc_a_utiliser=TodaesMain.todaesOperations[j].MaxFinOpAvantThis(id_postExecutant); // Arc arc_a_utiliser=TodaesMain.todaesOperations[j].MaxFinOpAvantThis()
		if(!arc_a_utiliser.getOpSource().getLibelle().equals("null")){ // Si c'est pas les premieres op prete(A,F). Si null alors arc avec op nomme null
	
			ArrayList<Integer> indicelien=IndiceLien(arc_a_utiliser);
			if(indicelien.get(0)!= -1){ // pas d'arc
				System.out.println("\nL'arc : "+arc_a_utiliser);
				TodaesMain.liens.get(0).AjouterCommunication(arc_a_utiliser); 
				AfficheChemin(); 
				System.out.println("ch opt : ");
				AfficheCheminOptimal(arc_a_utiliser);
			}
		}else{
			System.out.println("pas d'arc");
		}

		
		int taille = TodaesMain.operateurs[id_postExecutant].getListOpExecute().size();
		double Duree_LastOpSurPoste;
		if(taille==0){
			TodaesMain.todaesOperations[j].setDebut(0);
			Duree_LastOpSurPoste=0;
		}
		else{
			Duree_LastOpSurPoste=TodaesMain.operateurs[id_postExecutant].getListOpExecute().get(taille-1).getFin();
		}
		double DureeArc=0;
		double FinDeOpPrecedente=0;
		if(!(arc_a_utiliser.getOpSource().getLibelle().equals("null"))){ // pas d'op precedante

			FinDeOpPrecedente=getFin(arc_a_utiliser.getOpSource().getLibelle()); 
			DureeArc=arc_a_utiliser.DureeArcSurLien(chemins);

		}
		TodaesMain.todaesOperations[j].setDebut(Math.max(Math.max(FinDeOpPrecedente+DureeArc,Duree_LastOpSurPoste),fin_dern_comm));
		TodaesMain.todaesOperations[j].setFin(TodaesMain.todaesOperations[j].getDebut()+(TodaesMain.todaesOperations[j].getDurees()[id_postExecutant]/TodaesMain.operateurs[id_postExecutant].getFrequences()[id_freq])); 	
			TodaesMain.todaesOperations[j].setPlace(1);

		chemins=new ArrayList<Chemin>(); // Remise a 0
		liens_temp=new ArrayList<Lien>();
	}
	*/
	
        public ArrayList<Integer> IndiceLien(Arc arc){ // optimal
		ArrayList<Integer> liste_indices=new ArrayList<Integer>();
		if(chemins.size()>0){
			double duree_temp=arc.SommeDuree(chemins.get(0).getChemin());
			int k=0;
			for(int i=1;i<chemins.size();i++){
				if(arc.SommeDuree(chemins.get(i).getChemin())<duree_temp){ // somme minimal des durees
					duree_temp=arc.SommeDuree(chemins.get(i).getChemin());
					k=i; 
				}
			}
			for(int i=0;i<chemins.get(k).getChemin().size();i++) 
				liste_indices.add(IndiceLien(chemins.get(k).getChemin().get(i)));
			return liste_indices;
		}
		else{
			liste_indices.add(-1);
			return liste_indices;
		}
	}
        public int IndiceLien(Lien l){
		for(int i=0;i<TodaesMain.liens.size();i++){
			if(TodaesMain.liens.get(i).equals(l))
				return i;
			}
			return -1;
	}
        public double getFin(String LibelleOperation) {
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			if(TodaesMain.todaesOperations[i].getLibelle().equals(LibelleOperation))
				return TodaesMain.todaesOperations[i].getFin();
		}
		return -1;
	}
	public void AfficheChemin(){
		for(int i=0;i<chemins.size();i++){
			System.out.println("\nch  "+i+" : ");
			for(int j=0;j<chemins.get(i).getChemin().size();j++){
				System.out.println("Lien n"+j+" : "+chemins.get(i).getChemin().get(j).getLibelle()); // chemins.get(i).getChemin() est un lien
			}
		}
	}
	public void AfficheCheminOptimal(Arc arc){
		if(chemins.size()>0){
			double duree_temp=arc.SommeDuree(chemins.get(0).getChemin());
			int k=0;
			for(int i=1;i<chemins.size();i++){
				if(arc.SommeDuree(chemins.get(i).getChemin())<duree_temp){ // la somme minimal des durees
					duree_temp=arc.SommeDuree(chemins.get(i).getChemin());
					k=i; 				}
			}
			for(int i=0;i<chemins.get(k).getChemin().size();i++) 
				System.out.println(chemins.get(k).getChemin().get(i).getLibelle());
		}
	}
	
	public void InitOpPrete(){
		int bool=0;
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			bool=TodaesMain.todaesOperations[i].ExistinDest(); // si elle existe dans la destination ou nn.
			
			if((bool==0 || TodaesMain.todaesOperations[i].IsTousPredecesseurPlace()==1) && TodaesMain.todaesOperations[i].getPlace()==0) 
				TodaesMain.opPrete.add(TodaesMain.todaesOperations[i]);
		}
		AfficheOpPrete();
	}
	public void AfficheOpPrete(){
		System.out.println("Op pretes : ");
		for(int i=0;i<TodaesMain.opPrete.size();i++)
			System.out.println((i+1)+"- "+TodaesMain.opPrete.get(i).getLibelle());
	}
	/*public void RechUnLien(Operateur src,Operateur dest){
		for(int i=0;i<copie_liens.size();i++){
			if(((copie_liens.get(i).getOperateur1().equals(src) && copie_liens.get(i).getOperateur2().equals(dest)) || (copie_liens.get(i).getOperateur1().equals(dest) && copie_liens.get(i).getOperateur2().equals(src)))){
				liens_temp.add(copie_liens.get(i));
				chemins.add(new Chemin(liens_temp));
				liens_temp=new ArrayList<Lien>();
			}
		}
	} */

	// tous les chemins "minimaux" entre des operateurs, i.e., sans liens doublons
/*	public void MinChemin(Operateur src,Operateur dest){
	if(chemins.size()<1 && !src.equals(dest)){ // semi rec

		copie_liens=TodaesMain.liensSiConnexe;
		
		 // path 1 link
		RechUnLien(src,dest);
	
		if(TodaesMain.operateurs.length>2){ // path 2 link
			for(int i=0;i<copie_liens.size();i++){
				if(copie_liens.get(i).getOperateur1().equals(src) && !copie_liens.get(i).getOperateur2().equals(dest)){ 
					liens_temp.add(copie_liens.get(i));
					RechUnLien(copie_liens.get(i).getOperateur2(),dest);
				}
				if(copie_liens.get(i).getOperateur2().equals(src) && !copie_liens.get(i).getOperateur1().equals(dest)){
					liens_temp.add(copie_liens.get(i));
					RechUnLien(copie_liens.get(i).getOperateur1(),dest);
				}
			}
		}
		if(TodaesMain.operateurs.length>3){// path 3 link
			for(int i=0;i<copie_liens.size();i++){
				if(copie_liens.get(i).getOperateur1().equals(src) && !copie_liens.get(i).getOperateur2().equals(dest)){
					liens_temp.add(copie_liens.get(i));
					for(int j=0;j<copie_liens.size();j++){
						if(copie_liens.get(j).getOperateur1().equals(copie_liens.get(i).getOperateur2()) && !copie_liens.get(j).getOperateur2().equals(copie_liens.get(i).getOperateur1()) && !copie_liens.get(j).getOperateur2().equals(dest)){
							liens_temp.add(copie_liens.get(j));
							RechUnLien(copie_liens.get(j).getOperateur2(),dest);
						}
						else if(copie_liens.get(j).getOperateur2().equals(copie_liens.get(i).getOperateur2()) && !copie_liens.get(j).getOperateur1().equals(copie_liens.get(i).getOperateur1()) && !copie_liens.get(j).getOperateur1().equals(dest)){
							liens_temp.add(copie_liens.get(j));
							RechUnLien(copie_liens.get(j).getOperateur1(),dest);
						}
					}
				}
				if(copie_liens.get(i).getOperateur2().equals(src) && !copie_liens.get(i).getOperateur1().equals(dest)){ //
					liens_temp.add(copie_liens.get(i));
					for(int j=0;j<copie_liens.size();j++){
						if(copie_liens.get(j).getOperateur1().equals(copie_liens.get(i).getOperateur1()) && !copie_liens.get(j).getOperateur2().equals(copie_liens.get(i).getOperateur2()) && !copie_liens.get(j).getOperateur2().equals(dest)){
							liens_temp.add(copie_liens.get(j));
							RechUnLien(copie_liens.get(j).getOperateur2(),dest);
						}
						else if(copie_liens.get(j).getOperateur2().equals(copie_liens.get(i).getOperateur1()) && !copie_liens.get(j).getOperateur1().equals(copie_liens.get(i).getOperateur2()) && !copie_liens.get(j).getOperateur1().equals(dest)){
							liens_temp.add(copie_liens.get(j));
							RechUnLien(copie_liens.get(j).getOperateur1(),dest);
						}
					}
				}
			}
		}
	}
	} 
*/
	public boolean IsDejaPris(ArrayList<Integer> tab,int i){
		for(int j=0;j<tab.size();j++){
			if(tab.get(j)==i)
				return true;
		}
		return false;
	}
	public int DureeMoyenne(TodaesOperation op){
		assert(op.getPlace() == 0);
		int s=0; 
		for(int i=0;i<op.getDurees().length;i++){
			s+=op.getDurees()[i];
		}
		return (s/op.getDurees().length); 
	}
        public boolean ResteOperationNonPlace(){
            for(int i=0;i<TodaesMain.todaesOperations.length;i++){
                if(TodaesMain.todaesOperations[i].getPlace()==0)
                    return true;
            }
            return false;
        }






	 
}
