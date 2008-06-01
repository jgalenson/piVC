package data_structures;

import java.util.ArrayList;

public class VerificationAtom {
	
	private BasicPath bp;
	private VerificationCondition vc;
	private VerificationResult.validityT validity;
	private Counterexample counterexample;
	private String identifier;
	
	public VerificationAtom(BasicPath bp, VerificationCondition vc, VerificationResult.validityT validity, Counterexample counterexample, String identifier) {
		this.bp = bp;
		this.vc = vc;
		this.validity = validity;
		this.counterexample = counterexample;
		this.identifier = identifier;
	}
	

	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public VerificationCondition getVC() {
		return vc;
	}

	public BasicPath getBP() {
		return bp;
	}	
	
	public ArrayList<Location> getLocations() {
		if(getBP()!=null){
			return getBP().getLocations();
		}else{
			ArrayList<Location> locs = new ArrayList<Location>();
			locs.add(vc.getLocation());
			return locs;
		}
	}
	
	public Counterexample getCounterexample() {
		return counterexample;
	}
	
	public String getIdentifier(){
		return identifier;
	}

}
