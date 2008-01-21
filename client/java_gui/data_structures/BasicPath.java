package data_structures;

import java.util.ArrayList;

public class BasicPath {
	
	private ArrayList<Step> steps;
	private VerificationCondition vc;
	private boolean isValid;
	
	public BasicPath(ArrayList<Step> steps, VerificationCondition vc, boolean isValid) {
		this.steps = steps;
		this.vc = vc;
		this.isValid = isValid;
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

}
