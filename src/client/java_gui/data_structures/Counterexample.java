package data_structures;

import java.util.ArrayList;

public class Counterexample {
	
	public static class Variable {
		
		public Variable(String text, Location loc) {
			this.text = text;
			this.location = loc;
		}
		
		public String text;
		public Location location;
		
		public String getText() {
			return text;
		}
		
		public Location getLocation() {
			return location;
		}
		
	}
	
	private ArrayList<Variable> variables;
	
	public Counterexample(ArrayList<Variable> variables) {
		this.variables = variables;
	}
	
	public Variable getVariable(int index) {
		return variables.get(index);
	}
	
	public int getNumVariables() {
		return variables.size();
	}

}
