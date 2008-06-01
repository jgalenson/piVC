package data_structures;

public class Termination {
	
	private VerificationResult.validityT validity;
	private VerificationAtomCollection decreasing;
	private VerificationAtomCollection nonnegative;
	
	public Termination(VerificationResult.validityT validity, VerificationAtomCollection decreasing, VerificationAtomCollection nonnegative) {
		this.validity = validity;
		this.decreasing = decreasing;
		this.nonnegative = nonnegative;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public VerificationAtomCollection getDecreasing() {
		return decreasing;
	}
	
	public VerificationAtomCollection getNonnegative() {
		return nonnegative;
	}
	
}
