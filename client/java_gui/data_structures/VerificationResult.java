package data_structures;

import java.util.ArrayList;

public class VerificationResult {
	
	public static final int VALID = 0;
	public static final int INVALID = 1;
	public static final int UNKNOWN = 2;
	
	private String filename;
	private int validity;
	private ArrayList<Function> functions;
	
	public VerificationResult(String filename, int validity, ArrayList<Function> functions) {
		this.filename = filename;
		this.validity = validity;
		this.functions = functions;
	}
	
	public String getFilename() {
		return filename;
	}
	
	public int getValidity() {
		return validity;
	}
	
	public int getNumFunctions() {
		return functions.size();
	}
	
	public Function getFunction(int index) {
		return functions.get(index);
	}

}
