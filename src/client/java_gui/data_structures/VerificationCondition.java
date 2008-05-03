package data_structures;

public class VerificationCondition {
	
	private String vc;
	private VerificationResult.validityT validity;
	
	public VerificationCondition(String vc, VerificationResult.validityT validity) {
		this.vc = vc;
		this.validity = validity;
	}
	
	public String getVerificationCondition() {
		return vc;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}

}
