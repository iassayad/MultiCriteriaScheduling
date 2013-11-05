
import java.util.ArrayList;


public class Chemin {

	ArrayList<Lien> chemin;

	public Chemin(ArrayList<Lien> chemin) {
		super();
		this.chemin=new ArrayList<Lien>();
		for(int i=0;i<chemin.size();i++){
			this.chemin.add(chemin.get(i));
		}
	}
	public void AddLien(Lien l){
		chemin.add(l);
	}
	public Chemin(){
		chemin=new ArrayList<Lien>();
	}
	public ArrayList<Lien> getChemin() {
		return chemin;
	}

	public void setChemin(ArrayList<Lien> chemin) {
		this.chemin = chemin;
	}

}
