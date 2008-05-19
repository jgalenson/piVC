package data_structures;

public class RHSConjunct extends Conjunct{
	public RHSConjunct(String str, boolean inInductiveCore, Location loc, VerificationResult.validityT status){
		super(str, inInductiveCore, loc);
		this.status = status;
	}
	public VerificationResult.validityT status;
}