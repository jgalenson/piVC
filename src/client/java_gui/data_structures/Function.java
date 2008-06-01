package data_structures;

public class Function {
	
	private String name;
	private VerificationResult.validityT validity;
	private VerificationAtomCollection correctness;
	private Termination termination;
	private Location location;
	
	public Function(String name, VerificationResult.validityT validity, VerificationAtomCollection correctness, Termination termination, Location location) {
		this.name = name;
		this.validity = validity;
		this.correctness = correctness;
		this.termination = termination;
		this.location = location;
	}
	
	public String getName() {
		return name;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public VerificationAtomCollection getCorrectness() {
		return correctness;
	}
	
	public Termination getTermination() {
		return termination;
	}
	
	public Location getLocation() {
		return location;
	}

}
