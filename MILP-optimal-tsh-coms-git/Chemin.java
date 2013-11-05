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
		AjoutUnLien(src,dest, nouveauchemin_liens, arch_liens, chemins);
	
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
		assert(chemins.size()>=1);
	}
	
	@Override
	public String toString(){
		String str = "";
		for(int i=0;i<chemin.size();i++){
			str += chemin.get(i).toString();
		}
		return str;
	}
	
	
	
	public static class Lien {
		private int Operateur1; 
		private int Operateur2;
		private int Identifiant;
	
		public Lien(int i,int j){
			Operateur1=i;
			Operateur2=j;
		}
	
		public int getOperateur1() {
			return Operateur1;
		}
		public int getOperateur2() {
			return Operateur2;
		}
		public void setOperateur1(int operateur1) {
			Operateur1 = operateur1;
		}
		public void setOperateur2(int operateur2) {
			Operateur2 = operateur2;
		}
	
		public void setIdentifiant(int id){
			Identifiant = id;
		}
		
		public int getIdentifiant(){
			return Identifiant;
		}
		
		public boolean equals(Object obj) {
			return (Operateur1==((Lien)obj).getOperateur1() && Operateur2==((Lien)obj).getOperateur2()) || (Operateur1 == ((Lien)obj).getOperateur2() && Operateur2 == ((Lien)obj).getOperateur1());
		}
	
				
		 @Override
		public String toString(){
			return " Lien "+ Identifiant +" ( "+Operateur1+",  "+Operateur2+") ";
		}
	} // Lien


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
