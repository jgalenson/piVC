package data_structures;

import java.util.ArrayList;

public class VerificationResult {
	
	public enum validityT {VALID, INVALID, UNKNOWN };
	
	private String filename;
	private validityT validity;
	private ArrayList<Function> functions;
	
	public VerificationResult(String filename, validityT validity, ArrayList<Function> functions) {
		this.filename = filename;
		this.validity = validity;
		this.functions = functions;
	}
	
	public String getFilename() {
		return filename;
	}
	
	public validityT getValidity() {
		return validity;
	}
	
	public int getNumFunctions() {
		return functions.size();
	}
	
	public Function getFunction(int index) {
		return functions.get(index);
	}

}
