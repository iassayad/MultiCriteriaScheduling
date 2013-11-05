

import java.util.ArrayList;



public class Arc {
	private TodaesOperation OpSource,OpDestination;
	private double[] Durees;

	public Arc(){
		OpSource=new TodaesOperation("null");
	}

	public void Init(){
		Durees= new double[TodaesMain.liens.size()];
		for(int i=0;i<Durees.length;i++){
			Durees[i]=(double)(Math.random()*(10)+1);
		}
	}
	public Arc(TodaesOperation opsrc,TodaesOperation opdest){
		OpSource=opsrc;
		OpDestination=opdest;
	}

	public TodaesOperation getOpSource() {
		return OpSource;
	}
	public void setOpSource(TodaesOperation opSource) {
		OpSource = opSource;
	}
	public TodaesOperation getOpDestination() {
		return OpDestination;
	}
	public void setOpDestination(TodaesOperation opDestination) {
		OpDestination = opDestination;
	}
	public double[] getDurees() {
		return Durees;
	}
	public void setDurees(double[] durees) {
		Durees=new double[durees.length];
		for(int i=0;i<durees.length;i++)
			Durees[i] = durees[i];
	}
    @Override
	public String toString(){
		String s = OpSource+"-->"+OpDestination+" Durees(";
		
		for(int i=0;i<Durees.length;i++)
			s+=Durees[i]+",";
		return s+")";
	}
	public double DureeArcSurLien(ArrayList<Chemin> chemins){ // je calcule la duree de l'arc sur le(s) lien(s) en tenant en compte les chemins possibles
		if(chemins.size()>0){
			double duree_temp=SommeDuree(chemins.get(0).getChemin()); // je prend la premiere somme pour la comparer avec les autres
			int k=0;
			for(int i=1;i<chemins.size();i++){
				if(SommeDuree(chemins.get(i).getChemin())<duree_temp){ // pour chercher la somme minimal des durees
					duree_temp=SommeDuree(chemins.get(i).getChemin());
					k=i; // l'indice du chemin avec le min des durees
				}
			}
			double duree_max_chemin=0;
			for(int i=0;i<chemins.get(k).getChemin().size();i++){ // pour parcourir la liste des chemins
				if(Durees[IndiceLien(chemins.get(k).getChemin().get(i))]>duree_max_chemin)
					duree_max_chemin=Durees[IndiceLien(chemins.get(k).getChemin().get(i))];
			}
			return duree_max_chemin;
		}
		else
			return 0;
	}
	
	public double SommeDuree(ArrayList<Lien> liens){ // Cette fonction re�oi un chemin en parametre, et return la somme des dur�es de l'arc en question sur ces liens
		double s=0;
		for(int i=0;i<liens.size();i++)
			s+=Durees[IndiceLien(liens.get(i))];
		return s;
	}
	public int IndiceLien(Lien l){
		for(int i=0;i<TodaesMain.liens.size();i++){
			if(TodaesMain.liens.get(i).equals(l))
				return i;
		}
		return -1;
	}

}
