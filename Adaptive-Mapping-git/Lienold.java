
import java.util.ArrayList;


public class Lien {
	private String libelle;
	private Operateur Operateur1; 
	private Operateur Operateur2;
	private ArrayList<Communication> communication;
	public Lien(){
		
		Operateur1=new Operateur();
		Operateur2=new Operateur();
		communication = new ArrayList<Communication>();
	}
	public Lien(int i,int j){
		assert(i!=j);
		//if (i>j) { int aux = j; j=i; i=aux;}
		Operateur1=TodaesMain.operateurs[i];
		libelle="L"+(i+1)+(j+1);
		Operateur2=TodaesMain.operateurs[j];
		communication=new ArrayList<Communication>();

	}

	
        public static void InitLien(int nb_operateur){
	
            for(int i=0;i<nb_operateur-1;i++){
		for(int j=i+1;j<nb_operateur;j++){
                	TodaesMain.liensSiConnexe.add(new Lien(i, j)); 
		}
            }
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
	public Operateur getOperateur1() {
		return Operateur1;
	}
	public Operateur getOperateur2() {
		return Operateur2;
	}
	public void setOperateur1(Operateur operateur1) {
		Operateur1 = operateur1;
	}
	public void setOperateur2(Operateur operateur2) {
		Operateur2 = operateur2;
	}
	public void setLibelle(String libelle) {
		this.libelle = libelle;
	}
	  public boolean equals(Object obj) {
	      return (Operateur1.equals(((Lien)obj).getOperateur1()) && Operateur2.equals(((Lien)obj).getOperateur2())) || (Operateur1.equals(((Lien)obj).getOperateur2()) && Operateur2.equals(((Lien)obj).getOperateur1()));
	    }
	public String getLibelle() {
		return libelle;
	}
    @Override
	public String toString(){
		return "Lien : "+libelle+", oper 1 : "+Operateur1+", oper 2 : "+Operateur2;
	}
	public ArrayList<Communication> getComminication() {
		return communication;
	}
	public void setComminication(ArrayList<Communication> comm) {
		communication = comm;
	}
	
}
