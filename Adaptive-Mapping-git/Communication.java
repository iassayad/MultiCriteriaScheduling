


	public class Communication{
		Arc arc;
		double debut,fin;
		public Communication(){
			arc = new Arc();
			debut=0;
			fin=0;
		}
		public Communication(Communication c){
			arc=c.getArc();
			debut=c.getDebut();
			fin=c.getFin();
		}
		public Communication(Arc comm, double debut) {
			super();
			arc = comm;
			this.debut = debut;
			this.fin = debut+arc.DureeArcSurLien(TodaesAlgorithme.chemins); //DureeArcSurLien cherche le chemin avec le min des des sommes des durees mais retroune le max entre les durees des liens
		}
		public Communication(Arc arc) {
			super();
			this.arc = arc;
		}
		public Arc getArc() {
			return arc;
		}
		public void setArc(Arc comm) {
			arc = comm;
		}
		public double getDebut() {
			return debut;
		}
		public void setDebut(double debut) {
			this.debut = debut;
		}
		public double getFin() {
			return fin;
		}
		public void setFin(double fin) {
			this.fin = fin;
		}
                @Override
		public String toString(){
			return arc+"(debut:"+debut+",fin:"+fin+")";
		}
		

	}
