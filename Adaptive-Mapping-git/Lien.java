import java.util.*;

public class Lien {
		private int Operateur1; 
		private int Operateur2;
		private int Identifiant;
		private String libelle;
		private ArrayList<Communication> communication;

		// IMPORTANT : getIdentifiant() utilisee pour le cacul de lenergie consomme de chaque lien
		// IMPORTANT =====> Appeler l.setIdentifiant( ) immediatement apres la creation  d'un lien
		// IMPORTANT : identifiant commence de zero
		public Lien(int i,int j){
			Operateur1=i;
			Operateur2=j;
			libelle="L"+(i+1)+(j+1);
			communication = new ArrayList<Communication>();
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
		
		public String getLibelle() {
			return libelle;
		}
		public boolean equals(Object obj) {
			return (Operateur1==((Lien)obj).getOperateur1() && Operateur2==((Lien)obj).getOperateur2()) || (Operateur1 == ((Lien)obj).getOperateur2() && Operateur2 == ((Lien)obj).getOperateur1());
		}
	
				
		 @Override
		public String toString(){
			return " Lien "+ Identifiant +" ( "+Operateur1+",  "+Operateur2+") ";
		}


public void AjouterCommunication(Arc arc_temp){
		
		if(!arc_temp.getOpSource().getPosteExecutant().equals(arc_temp.getOpDestination().getPosteExecutant())){
			ArrayList<Integer> indicelien=TodaesMain.todaesAlgo.IndiceLien(arc_temp);
			int taille_com;
			double maxdebut=0;
			for(int i=0;i<indicelien.size();i++){
				taille_com=TodaesMain.liens.get(indicelien.get(i)).communication.size(); 
				Communication com_test=new Communication();
				if(taille_com>0){ 
					com_test=new Communication(arc_temp,Math.max(Math.max(TodaesMain.liens.get(indicelien.get(i)).communication.get(taille_com-1).getFin(), (arc_temp.getOpSource().getFin())), maxdebut));//la date de debut = max (entre fin de l'op source+duree sur le lien, et fin de la derniere comm sur le lien), Fin = Debut+duree de l'arc sur le lien
					maxdebut=Math.max(TodaesMain.liens.get(indicelien.get(i)).communication.get(taille_com-1).getFin(), (arc_temp.getOpSource().getFin()));
					TodaesMain.liens.get(indicelien.get(i)).communication.add(com_test);
					TodaesAlgorithme.fin_dern_comm=com_test.getFin();
					//System.out.println("fin_dern_com = "+TodaesAlgorithme.fin_dern_comm);
				}
				else{ 
					com_test=new Communication(arc_temp,Math.max(arc_temp.getOpSource().getFin(),maxdebut)); 
					maxdebut=arc_temp.getOpSource().getFin();
					TodaesMain.liens.get(indicelien.get(i)).communication.add(com_test);
					TodaesAlgorithme.fin_dern_comm=com_test.getFin();
                                        //System.out.println("fin_dern_com = "+TodaesAlgorithme.fin_dern_comm);
				}
			}
		}
	}
public void myAjouterCommunication(Arc arc, double debut, Vector<Vector<String>> todaesMappingElement){
			int p1 = TodaesAlgorithme.getProc(arc.getOpSource(), todaesMappingElement);
			int p2 = TodaesAlgorithme.getProc(arc.getOpDestination(), todaesMappingElement);	
			assert (p1 != p2);
			Communication com = new Communication(arc);
						
			com.setDebut(Math.max(debut, getFinDerniereComm()));
			com.setFin(com.getDebut() + arc.getDurees()[0]);
			communication.add(com);			 
}


public double getFinDerniereComm(){
	int taille=communication.size(); 	
	if (taille == 0 ) {
		return 0;	
	} else {
		assert(communication.get(taille-1).getFin() > 0);
		return communication.get(taille-1).getFin();
	}	
}


public ArrayList<Communication> getComminication() {
		return communication;
	}
	public void setCommunication(ArrayList<Communication> comm) {
		assert(communication != null);
		communication = comm;
	}
	} // Lien


