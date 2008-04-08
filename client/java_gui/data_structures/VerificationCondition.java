package data_structures;

public class VerificationCondition {
	
	private String vc;
	private int validity;
	
	public VerificationCondition(String vc, int validity) {
		this.vc = vc;
		this.validity = validity;
	}
	
	public String getVerificationCondition() {
		return vc;
	}
	
	public int getValidity() {
		return validity;
	}

}
