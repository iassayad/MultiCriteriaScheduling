
import java.util.ArrayList;

public class Operateur {
	private String Libelle;
	 private static double[] Frequences = new double[4];
	private ArrayList<TodaesOperation> ListOpExecute = new ArrayList<TodaesOperation>(); 
	public Operateur(){
				
		Libelle = null;
	}
	public Operateur(double[] frequences, String libelle) {
		Frequences = frequences;
		Libelle = libelle;
	}
	public Operateur(String libelle) {
		Libelle = libelle;
	}
	public static double[] getFrequences() {
		
			
		return Frequences;
	}
	
	public static void check () {
	for(int i=0;i<4;i++){
				
     			assert(Frequences[i] > 0);
		}
	
	}
	
	public void setFrequences(double[] frequences) {
		Frequences = frequences;
		//System.out.println(toString()); 
	}
	
	public void AjouterOperation(TodaesOperation op){
		ListOpExecute.add(op);
	}

	public Operateur(Operateur op){

		//Frequences = new double [op.getFrequences().length];	
		assert(Frequences != null);	
		for(int i=0;i<op.getFrequences().length;i++){

     			Frequences[i]=op.getFrequences()[i];
			
			}
		Libelle = op.getLibelle();
		check();


	}
	public void setLibelle(String libelle) {
		this.Libelle = libelle;
	}
	public String getLibelle() {
		return Libelle;
	}
    @Override
	public String toString(){
		String s=Libelle+"(";
		for(int i=0;i<Frequences.length;i++)
			s=s+Frequences[i]+",";
		return s+")";
		
	}
	  public boolean equals(Object obj) {
	      return Libelle.equals(((Operateur)obj).getLibelle());
	    }
	public ArrayList<TodaesOperation> getListOpExecute() {
		return ListOpExecute;
	}
	public void setListOpExecute(ArrayList<TodaesOperation> listOpExecute) {
		assert(listOpExecute != null);
		ListOpExecute = listOpExecute;
	}
}
