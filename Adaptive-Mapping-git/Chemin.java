//
//  Chemins.java
//  
//
//  Created by asssayad on 8/18/12.
//  Copyright 2012 __MyCompanyName__. All rights reserved.
//

import java.util.ArrayList;

public class Chemin {

	ArrayList<Lien> chemin;
	//ArrayList chemin;

	public Chemin(ArrayList<Lien> liens) {
		assert(chemin == null);
		chemin = new ArrayList<Lien>();
		for(int i=0;i<liens.size();i++){
			this.chemin.add(liens.get(i));
		}
	}
	
		
	public ArrayList<Lien> getChemin() {
		return chemin;
	}

	public static void AjoutUnLien(int src,int dest, ArrayList<Lien> nouveauchemin_liens, ArrayList<Lien> arch_liens, ArrayList<Chemin> chemins){
		for(int i=0;i<arch_liens.size();i++){
			if(((arch_liens.get(i).getOperateur1()== src && arch_liens.get(i).getOperateur2()== dest) || 
			    (arch_liens.get(i).getOperateur1()== dest && arch_liens.get(i).getOperateur2()== src))){
				nouveauchemin_liens.add(arch_liens.get(i));
				chemins.add(new Chemin(nouveauchemin_liens));
			}
		}
	}
	
	
	
	private static ArrayList copie(ArrayList opers){
		ArrayList a = new ArrayList();
		for(int i=0;i<opers.size();i++){
			a.add(opers.get(i));
		}
		return a;
	}
	
	public static ArrayList<Chemin> Chemins(int src,int dest, ArrayList<Lien> arch_liens){
		 ArrayList<Chemin> chemins = new  ArrayList<Chemin>();
		 Chemins(src,dest, null, null, arch_liens, chemins);
		 assert(chemins.size()>=1);
		 return chemins;
	}
	
	private static void Chemins(int src,int dest, ArrayList<Lien> liens, ArrayList operaexclure,  ArrayList<Lien> arch_liens, ArrayList<Chemin> chemins){
		if (liens == null) {
				assert(operaexclure == null);
				liens = new ArrayList<Lien>();
				operaexclure = new ArrayList();
		}
		assert(operaexclure.size()!=0 || liens.size()==0);
		assert(arch_liens.size() >= 1);
		assert(src != dest);
		assert(!operaexclure.contains(dest));	
		// path 1 link
		ArrayList<Lien> nouveauchemin_liens = copie(liens); //prefixe de liens auquel on rajoutera un lien 
		AjoutUnLien(src,dest, nouveauchemin_liens, arch_liens, chemins); // rajoute un lien direct de src' a dest a la suite du chemin menant de src jusqu'a src' => ce qui fait un chemin de src vers src'
	
		// path 2 link
		int op1 = -1;
		int op2 = -1;
		for(int i=0;i<arch_liens.size();i++){
			if(arch_liens.get(i).getOperateur1()== src && arch_liens.get(i).getOperateur2() != dest && !operaexclure.contains(arch_liens.get(i).getOperateur2())){ 
				op1 = arch_liens.get(i).getOperateur1();
				op2 = arch_liens.get(i).getOperateur2();
			} else if (arch_liens.get(i).getOperateur2()== src && arch_liens.get(i).getOperateur1() != dest && !operaexclure.contains(arch_liens.get(i).getOperateur1())){ 
				op1 = arch_liens.get(i).getOperateur2();
				op2 = arch_liens.get(i).getOperateur1();
			} else {
				op1 = -1;
				op2 = -1;
			}
		
			if(op1 != -1 && op2 != -1){
				ArrayList<Lien> nouveauxchemins_liens = copie(liens); // prefixe des liens pour les nouveaux chemins
				ArrayList nouveauxoperaexclure = copie(operaexclure);// prefixe des oper a exclure pour les nouveaux chemins
				assert(op1== src);
				nouveauxchemins_liens.add(arch_liens.get(i));
				nouveauxoperaexclure.add(op1);
				//RechUnLien(op1,dest);
				Chemins(op2,dest,nouveauxchemins_liens,nouveauxoperaexclure, arch_liens, chemins);
			}else assert(op1 == -1 && op2 == -1);
		}
	//	assert(chemins.size()>=1); // vrai pour les archis fully connected mais pas dans le cas d'archi NOC
	}
	
	@Override
	public String toString(){
		String str = "";
		for(int i=0;i<chemin.size();i++){
			str += chemin.get(i).toString();
		}
		return str;
	}
	
	
	
	// Les liens d'une archi  composée de nb_oper processeurs fully connected
	public static ArrayList<Lien> initialiser_archi_liens(int nb_oper) {
			ArrayList<Lien> arch_liens= new ArrayList<Lien>();
			int id = 0;
			for(int i=0;i<nb_oper-1;i++){
				for(int j=i+1;j<nb_oper;j++){
					Lien l = new Lien(i,j);
					l.setIdentifiant(id++);
                	arch_liens.add(l); 
				}
			}
			return arch_liens;
		}

	// retourne la liste des liens reliant une sous-arch de nb_oper procs d'un NOC
	// NB : pour simplifier l'ecriture du programme ILP pour FMDEP 2013 
	//      on considère uniquement une sous-arch de la forme uni-ligne bi-directionelle, i.e, 
	//      P1 <----> P2<----> .... <----> Pnb_oper
	//      et une sous-arch pentanome bi-directionnlle inlcuant un SEUL carre a gauche ou a droite en haut ou en bas :
	//   
        //     |_4_|_3_|_2_|_1_|   1 CARREE
	//     |_5_|_6_|_7_|_8_|
	//
        //     |_4_|_3_|_2_|_1_|   1 CARREE
	//     |_5_|_6_|_7_| .
	//      _______________
	//     |_4_|_3_|_2_|_1_|   1 CARREE
	//     |_5_|_6_| .   .
	//      ___________
	//     |_3_|_2_|_1_| .     1 CARREE
	//     |_4_|_5_| .   .
	//
	//        ______
	//     |_2_|_1_| . .      1 CARREE
	//     |_3_|_4_| . .

	public static ArrayList<Lien> initialiser_archi_liens_noc(int nb_oper, String archpattern) {
			ArrayList<Lien> arch_liens= new ArrayList<Lien>();
			int id = 0;
			if (archpattern.equals("ligne")) { // ligne bidirectionnelle
				for(int i=0;i<nb_oper-1;i++){
					//for(int j=i+1;j<nb_oper;j++){
						int j = i+1;
						assert(j < nb_oper);
						Lien l = new Lien(i,j); // i means proc i+1 and j means proc j+1
						l.setIdentifiant(id++);
						arch_liens.add(l); 
				//}
				}
			}else if (archpattern.equals("carre")){ // un et un seul carre
				for(int i=0;i<nb_oper-1;i++){
						int j = i+1; // on ajoute les liens d'un pattern ligne
						assert(j < nb_oper);
						Lien l = new Lien(i,j); // i means proc i+1 and j means proc j+1
						l.setIdentifiant(id++);
						arch_liens.add(l); 
				 }
				 
				 // puis on rajoute ce qui manque comme liens
				if (nb_oper>=4&& nb_oper<=6){ // pour les sous-arch de taille 4 ou 5 ou 6
					//Lien l = new Lien(nb_oper-4, nb_oper-1); // 1 means proc 2 and nb_oper-1 means proc nb_oper
					Lien l = new Lien(nb_oper-4, nb_oper-1); // 1 means proc 2 and nb_oper-1 means proc nb_oper
					l.setIdentifiant(id++); 		
					arch_liens.add(l);
				} else if (nb_oper==7){
					// on rajoute un seul carre toujours
					Lien l = new Lien(nb_oper-5, nb_oper-2); //
				} else if  (nb_oper==8){  // jusqu a 8 max
					// on rajoute un seul carre encore une fois c est plus rqpide pour ILP
					Lien l = new Lien(nb_oper-6, nb_oper-3); //
				} else assert(false); // nombre de processeurs plus que 8 non gere car taille des appli ne depassent pas 8
			}else assert(false);
				
			if (nb_oper == 1) assert( arch_liens.size()==0);
			assert(nb_oper >=1);
			return arch_liens;
		}

	public static void Afficher(ArrayList<Chemin> chemins){
		String str ="";
		for(int i=0;i<chemins.size();i++){
			str += chemins.get(i).toString() +"\n";
		}
		System.out.println(str);
	}
	
	public static void main2(String[] args){
		ArrayList<Lien> arch_liens = initialiser_archi_liens(3);

		System.out.println("chemins 0 --> 2");
		ArrayList<Chemin> chemins = Chemins(0,2, arch_liens);
		Afficher(chemins);
		
		System.out.println("chemins 1 --> 2");		
		chemins = Chemins(1,2, arch_liens);
		Afficher(chemins);

		System.out.println("chemins 1 --> 0");		
		chemins = Chemins(1,0, arch_liens);
		Afficher(chemins);
	}
	
} //chemin
