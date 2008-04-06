package data_structures;

public class Counterexample {
	
	private String[] variables;
	
	public Counterexample(String vars) {
		variables = vars.split("\n");
	}
	
	public String getVariable(int index) {
		return variables[index];
	}
	
	public int getNumVariables() {
		return variables.length;
	}

}
