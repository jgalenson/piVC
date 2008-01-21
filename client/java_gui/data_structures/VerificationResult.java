package data_structures;

import java.util.ArrayList;

public class VerificationResult {
	
	private boolean isValid;
	private ArrayList<Function> functions;
	
	public VerificationResult(boolean isValid, ArrayList<Function> functions) {
		this.isValid = isValid;
		this.functions = functions;
	}

}
