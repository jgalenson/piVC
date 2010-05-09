package data_structures;

import java.util.ArrayList;

public class VerificationResult {
	
	public enum validityT {VALID, INVALID, UNKNOWN, TIMEOUT };
	
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

	public static validityT parseValidity(String str) {
		if (str.equalsIgnoreCase("valid")){
			return validityT.VALID;
		}
		else if(str.equalsIgnoreCase("invalid")){
			return validityT.INVALID;
		}
		else if(str.equalsIgnoreCase("unknown")){
			return validityT.UNKNOWN;
		}else if(str.equalsIgnoreCase("timeout")){
			return validityT.TIMEOUT;
		}else{
			throw new RuntimeException("Unregonized validity: " + str);
		}
	}

}
