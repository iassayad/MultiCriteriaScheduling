



import java.util.ArrayList;


public class TodaesOperation {
	protected String libelle;
	private double[] Durees; 
	private int place;
	private Operateur PosteExecutant;
	private double debut;
	private double fin;
	float enEtapen = 0;
	private double freq;
	
	public TodaesOperation(){
		libelle=null;
		place=0;
		debut=0;
		fin=0;
		PosteExecutant=null;
	}
	public TodaesOperation(TodaesOperation op){
		libelle=op.getLibelle();
		debut=op.getDebut();
		assert(op.getPlace()==0);
		fin=0;
		place=op.getPlace();
		Durees=new double[op.getDurees().length];
		for(int i=0;i<op.getDurees().length;i++)
			Durees[i]=op.getDurees()[i];
	}
	public TodaesOperation(String nul){
		if(nul.equals("null"))
			libelle="null";
	}
	public TodaesOperation(String libelle, double[] durees) {
		super();
		this.libelle = libelle;
		Durees = durees;
	}

	
	 //liste des TodaesOperation qui precedent  celle ci
	/*public MinCout FonCout() throws NOSOLFOUNDEXCEPTION { 
		MinCout mc=new MinCout();
		double[][] tab_cout = new double[TodaesMain.operateurs.length][Operateur.getFrequences().length]; 
		double[][] tab_cout_pow = new double[TodaesMain.operateurs.length][Operateur.getFrequences().length]; 
		double[][] tab_cout_enEtapen = new double[TodaesMain.operateurs.length][Operateur.getFrequences().length]; 
		Arc arc_a_utiliser = new Arc();
		double s=0,L=0,SS=0; 
		for(int i=0;i<tab_cout.length;i++){ 
			arc_a_utiliser=MaxFinOpAvantThis(i); 
			s=arc_a_utiliser.getOpSource().getFin()+arc_a_utiliser.DureeArcSurLien(TodaesMain.todaesAlgo.chemins);
			for(int j=0; j< Operateur.getFrequences().length; j++){	
				L=EstimerL();
				SS=SS(this,i,j); 
				tab_cout[i][j]=s+SS-L; 
				tab_cout_pow[i][j]= EstimerEn(this,i,j)/L;
				tab_cout_enEtapen[i][j]  = CalculerEnEtapen(this,i,j);
			}
		}
		int m1=-1, m2=-1; 
		
		Boolean solfound=false;
		for(int i=0;i<tab_cout.length && solfound==false;i++){
			for(int j=0; j< Operateur.getFrequences().length && solfound==false; j++){
				if(tab_cout_pow[i][j] <= TodaesMain.POBJ && tab_cout_pow[i][j] <= TodaesMain.POBJ) { 
					m1=i;m2=j; //ia
					solfound=true;//ia
					}
			}
		}
		if (TodaesMain.mode == "-l") {
			m1=0; m2=0;
			solfound=true;
		}
		if (solfound==false) 
			throw 	new NOSOLFOUNDEXCEPTION(" TodaesOperation : "+this.toString());
		assert(m1>=0 && m2>=0);
		for(int i=0;i<tab_cout.length;i++){
			for(int j=0; j< Operateur.getFrequences().length; j++){	//ia
				if(tab_cout[i][j]<tab_cout[m1][m2] && (TodaesMain.mode=="-l"?true:(tab_cout_pow[i][j] <= TodaesMain.POBJ && tab_cout_pow[i][j] <= TodaesMain.POBJ))) 
					m1=i;m2=j;
			}
		}
		mc.setCouts(tab_cout[m1][m2],tab_cout_pow[m1][m2],tab_cout_enEtapen[m1][2]); 
		mc.setIdPoste(m1); 		
		mc.setIdFreq(m2); 		
		return mc;
		
	}
*/
	public double SS(TodaesOperation opsource,int indice_poste, int ind_freq){ 
		ArrayList<TodaesOperation> tabOpDest = new ArrayList<TodaesOperation>();
		double Duree=0;
		double SSmax=0;
		for(int j=0;j<TodaesMain.Arcs.size();j++){
			if(TodaesMain.Arcs.get(j).getOpSource().equals(opsource))
				tabOpDest.add(TodaesMain.Arcs.get(j).getOpDestination());
		}
		if(indice_poste!=-1) // Si c l'appel original, donc on prend la duree reel
			Duree=opsource.getDurees()[indice_poste]/Operateur.getFrequences()[ind_freq]; 
		else
			Duree=TodaesMain.todaesAlgo.DureeMoyenne(opsource)/average(Operateur.getFrequences()); 
		for(int j=0;j<tabOpDest.size();j++){
			if(SS(tabOpDest.get(j),-1,-1)>SSmax)
				SSmax=SS(tabOpDest.get(j),-1,-1);
		}
		return Duree+SSmax;
	}
	public double EstimerL(){
		double Duree_Distribution=0; 
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			Duree_Distribution=Math.max(Fin(TodaesMain.todaesOperations[i]), Duree_Distribution);
		}
		return Duree_Distribution;
	}
	
	
	public double CalculerEnEtapen(TodaesOperation opsource, int indice_poste, int indice_freq){
		double ennmoins1 = TodaesMain.en; 
		double enopcour = Durees[indice_poste]*Operateur.getFrequences()[indice_freq]*Operateur.getFrequences()[indice_freq];
		return (ennmoins1+enopcour);
	}

	
	public double EstimerEn(TodaesOperation opsource, int indice_poste, int indice_freq){
	
		double enestimeeOprestantes = 0;
		double wcetm =0;
		double fm = 0;
		fm = average(Operateur.getFrequences());
		for(int i=0;i<TodaesMain.todaesOperations.length;i++){
			if(!(TodaesMain.todaesOperations[i].getPlace() == 1)){
				wcetm = average(TodaesMain.todaesOperations[i].getDurees());
				enestimeeOprestantes += wcetm*fm*fm  ;
			}
		}
		return (CalculerEnEtapen(opsource, indice_poste, indice_freq)+enestimeeOprestantes);
	}
	
	
	
	
	public double Fin(TodaesOperation op){ // place ou non
		if(op.getPlace()==1){ 
			return op.getFin();
		}else{
			double max_fin=0;
			for(int i=0;i<TodaesMain.Arcs.size();i++){ 
				if(TodaesMain.Arcs.get(i).getOpDestination().equals(op))
						max_fin=Math.max(Fin(TodaesMain.Arcs.get(i).getOpSource()),max_fin);
			}
			return max_fin+TodaesMain.todaesAlgo.DureeMoyenne(op); // max_fin+ la duree moyenne de l'opetation de destination (celle en question)
		}
	}
	
	public void setFreq(double f) {
		freq = f;
	}

	public double getFreq() {
		assert(freq != 0);
		assert(getPlace() == 1);
		return freq;
	}

	public String getLibelle() {
		return libelle;
	}
	public void setLibelle(String libelle) {
		this.libelle = libelle;
	}
	public double[] getDurees() {
		assert(getPlace()==0);
		return Durees;
	}
	public void setDurees(double[] durees) {
		Durees = durees;
	}
	public int getPlace() {
		return place;
	}
	public void setPlace(int place) {
		this.place = place;
	}
	public Operateur getPosteExecutant() {
		return PosteExecutant;
	}
	public void setPosteExecutant(Operateur posteExecutant) {
		PosteExecutant = posteExecutant;
	}
	public double getDebut() {
		return debut;
	}
	public void setDebut( double debut) {
		this.debut = debut;
	}
	public double getFin() {
		if (getLibelle()!="null") assert(getPlace()==1);
		
		return fin;
	}
	public void setFin(double fin) {
		assert(fin != Double.POSITIVE_INFINITY);
		assert(getPlace()==0);
		this.fin = fin;
	}
	
		
	public int ExistinDest(){ //Si Cette TodaesOperation existe dans la partie opDestination ou non.
		for(int i=0;i<TodaesMain.Arcs.size();i++){
			if(TodaesMain.Arcs.get(i).getOpDestination().getLibelle().equals(libelle))
				return 1;
		}
		return 0;
	}
	public ArrayList<TodaesOperation> ListPredecesseur(){
		ArrayList<TodaesOperation> ListPredecesseur=new ArrayList<TodaesOperation>();
		for(int i=0;i<TodaesMain.Arcs.size();i++){
			if(TodaesMain.Arcs.get(i).getOpDestination().getLibelle().equals(libelle))//on cherche cette TodaesOperation dans les op de destination
				ListPredecesseur.add(TodaesMain.Arcs.get(i).getOpSource()); // puis on ajoute son predecesseur � la liste
		}
		return ListPredecesseur;
	}
	public int IsTousPredecesseurPlace(){
		ArrayList<TodaesOperation> ListPredecesseur = ListPredecesseur();
		for(int i=0;i<ListPredecesseur.size();i++){
			if(ListPredecesseur.get(i).getPlace()==0)
				return 0;
		}
		return 1;
	}

	public boolean isSuccessor(TodaesOperation op){
		for(int i=0;i<TodaesMain.Arcs.size();i++){
			if(TodaesMain.Arcs.get(i).getOpSource().getLibelle().equals(op.getLibelle()))
				if (TodaesMain.Arcs.get(i).getOpDestination().getLibelle().equals(libelle))
					return true;
		}
		return false;
	}
/*	public Arc MaxFinOpAvantThis(int indicePoste){ 
		Arc arc=new Arc(); 
		double fin=0;
		ArrayList<Chemin> chemins_final=new ArrayList<Chemin>();
		for(int j=0;j<TodaesMain.Arcs.size();j++){ 
			if(TodaesMain.Arcs.get(j).getOpDestination().equals(this)){ 
				TodaesMain.todaesAlgo.chemins=new ArrayList<Chemin>(); // Remise a 0
				TodaesMain.todaesAlgo.liens_temp=new ArrayList<Lien>();
				TodaesMain.todaesAlgo.MinChemin(TodaesMain.Arcs.get(j).getOpSource().getPosteExecutant(),TodaesMain.operateurs[indicePoste]);
				if((TodaesMain.Arcs.get(j).DureeArcSurLien(TodaesMain.todaesAlgo.chemins)+TodaesMain.Arcs.get(j).getOpSource().getFin())>fin){ // le max des (fin+duree sur l'arc)
					fin=TodaesMain.Arcs.get(j).DureeArcSurLien(TodaesMain.todaesAlgo.chemins)+TodaesMain.Arcs.get(j).getOpSource().getFin();
					arc = TodaesMain.Arcs.get(j);
					chemins_final = new ArrayList<Chemin>(TodaesMain.todaesAlgo.chemins);
				}
			}
		}
		TodaesMain.todaesAlgo.chemins=new ArrayList<Chemin>(chemins_final);
		if(fin==0){//if(libOp1.equals(libOp2) || fin==0){ // libOp1.equals(libOp2) si c le meme poste
			arc=new Arc();
			return arc;
		}
		return arc; //  deja place
	}
*/
	public void AfficheDurees(){
		System.out.println("(");
		for(int i=0;i<Durees.length;i++)
			System.out.println(Durees[i]+",");
		System.out.println(")");
	}
	

	public double average(double[] tab){
		double temp = 0;
		int i = 0;
		for (i=0; i < tab.length; i++){
			temp = temp + tab[i];
		}
		return temp / i;
	}
	
    @Override
	public String toString(){
		String s=libelle+"(";
		for(int i=0;i<Durees.length;i++)
			s=s+", "+Durees[i];
		return s+"), Placée : "+place+", exec "+PosteExecutant+", Début : "+debut+", Fin : "+fin+")";
	}
	  public boolean equals(Object obj) {
	      return libelle.equals(((TodaesOperation)obj).getLibelle());
	    }
		
		
public class MinCout{ 
	int IdPoste;
	double Cout;
	double CoutPuissance;
	double CoutEnEtapen;
	int f;
	public MinCout(){
		IdPoste=0;
		Cout=0;
	}
	public int getIdPoste() {
		return IdPoste;
	}
	public void setIdPoste(int idOperateur) {
		IdPoste = idOperateur;
	}
	public double getCout() {
		return Cout;
	}
	public double getCoutPuissance() {
		return CoutPuissance;
	}
	public double getenEtapen() {
		return CoutEnEtapen;
	}
	public void setCouts(double coutPression, double coutPuissance, double enEtapen) {
		Cout = coutPression;
		CoutPuissance = coutPuissance;
		CoutEnEtapen = enEtapen;
	}
	public void setIdFreq(int m2){
		f=m2;
	}
	public  int getIdFreq(){
		return f;
	}
}	
}
