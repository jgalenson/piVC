package data_structures;

import java.util.ArrayList;

public class BasicPath {
	
	private ArrayList<Step> steps;
	private String vc;
	private boolean isValid;
	
	public BasicPath(ArrayList<Step> steps, String vc, boolean isValid) {
		this.steps = steps;
		this.vc = vc;
		this.isValid = isValid;
	}

}
