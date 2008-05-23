package data_structures;

import data_structures.VerificationResult.validityT;

public class Conjunct{
	public Conjunct(String str,validityT status,Boolean inInductiveCore,Location loc){
		this.str = str;
		this.inInductiveCore = inInductiveCore;
		this.status = status;
		this.loc = loc;
	}
	public final String str;
	public final Boolean inInductiveCore; //null if N/A
	public final validityT status; //null if N/A
	public final Location loc;
	private String displayHTML;
	
	@Override
	public String toString(){
		return str;
	}

	public String getDisplayHTML(){
		return displayHTML;
	}
	
	public void setDisplayHTML(String displayHTML){
		this.displayHTML = displayHTML;
	}
	
	public Location getLocation(){
		return loc;
	}
	
}