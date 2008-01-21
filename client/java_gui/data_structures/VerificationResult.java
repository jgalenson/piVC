package data_structures;

import java.util.ArrayList;

public class VerificationResult {
	
	private String filename;
	private boolean isValid;
	private ArrayList<Function> functions;
	
	public VerificationResult(String filename, boolean isValid, ArrayList<Function> functions) {
		this.filename = filename;
		this.isValid = isValid;
		this.functions = functions;
	}
	
	public String getFilename() {
		return filename;
	}
	
	public boolean isValid() {
		return isValid;
	}
	
	public int getNumFunctions() {
		return functions.size();
	}
	
	public Function getFunction(int index) {
		return functions.get(index);
	}

}
