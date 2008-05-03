package data_structures;

import java.util.ArrayList;

public class Correctness implements BasicPathHolder {
	
	private VerificationResult.validityT validity;
	private ArrayList<BasicPath> basicPaths;
	
	public Correctness(VerificationResult.validityT validity, ArrayList<BasicPath> basicPaths) {
		this.validity = validity;
		this.basicPaths = basicPaths;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public int getNumBasicPaths() {
		return basicPaths.size();
	}
	
	public BasicPath getBasicPath(int index) {
		return basicPaths.get(index);
	}

}
