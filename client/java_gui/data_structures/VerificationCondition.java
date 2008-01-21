package data_structures;

public class VerificationCondition {
	
	private String vc;
	private boolean isValid;
	
	public VerificationCondition(String vc, boolean isValid) {
		this.vc = vc;
		this.isValid = isValid;
	}
	
	public String getVerificationCondition() {
		return vc;
	}
	
	public boolean isValid() {
		return isValid;
	}

}
