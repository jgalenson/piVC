package data_structures;

import java.util.ArrayList;

public class BasicPath {
	
	private ArrayList<Step> steps;
	private VerificationCondition vc;
	private boolean isValid;
	private Counterexample counterexample;
	
	public BasicPath(ArrayList<Step> steps, VerificationCondition vc, boolean isValid, Counterexample counterexample) {
		this.steps = steps;
		this.vc = vc;
		this.isValid = isValid;
		this.counterexample = counterexample;
	}
	
	public int getNumSteps() {
		return steps.size();
	}
	
	public Step getStep(int index) {
		return steps.get(index);
	}
	
	public boolean isValid() {
		return isValid;
	}
	
	public VerificationCondition getVC() {
		return vc;
	}
	
	public ArrayList<Location> getLocations() {
		ArrayList<Location> locations = new ArrayList<Location>();
		for (Step step: steps)
			locations.add(step.getLocation());
		return locations;
	}
	
	public Counterexample getCounterexample() {
		return counterexample;
	}

}
