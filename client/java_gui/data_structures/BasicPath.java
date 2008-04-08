package data_structures;

import java.util.ArrayList;

public class BasicPath {
	
	private ArrayList<Step> steps;
	private VerificationCondition vc;
	private int validity;
	private Counterexample counterexample;
	
	public BasicPath(ArrayList<Step> steps, VerificationCondition vc, int validity, Counterexample counterexample) {
		this.steps = steps;
		this.vc = vc;
		this.validity = validity;
		this.counterexample = counterexample;
	}
	
	public int getNumSteps() {
		return steps.size();
	}
	
	public Step getStep(int index) {
		return steps.get(index);
	}
	
	public int getValidity() {
		return validity;
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
