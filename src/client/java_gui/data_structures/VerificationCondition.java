package data_structures;

import java.util.ArrayList;
import java.util.List;

public class VerificationCondition {
	
	private Conjunct[][] conjuncts;
	private VerificationResult.validityT validity;
	
	public VerificationCondition(Conjunct[][] conjuncts, VerificationResult.validityT validity) {
		this.conjuncts = conjuncts;
		this.validity = validity;
	}
	
	private VerificationCondition() {
	}	
	
	public Conjunct[][] getConjuncts() {
		return conjuncts;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public String sanitizeHTML(String html){
		html = html.replaceAll("<", "&lt;");
		html = html.replaceAll(">", "&gt;");		
		return html;
	}
	
	public Location getLocation(){
		List<Location> locs = new ArrayList<Location>();
		for(Conjunct[] rows:conjuncts){
			for(Conjunct col:rows){
				locs.add(col.getLocation());
			}
		}		
		return Location.mergeLocations(locs);
	}
}
