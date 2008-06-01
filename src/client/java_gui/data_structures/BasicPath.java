package data_structures;
import java.util.ArrayList;
public class BasicPath {
	private ArrayList<Step> steps;
	
	public BasicPath(ArrayList<Step> steps){
		this.steps = steps;
	}
	
	public int getNumSteps() {
		return steps.size();
	}
	
	public Step getStep(int index) {
		return steps.get(index);
	}
	
	public ArrayList<Location> getLocations() {
		ArrayList<Location> locations = new ArrayList<Location>();
		for (Step step: steps)
			locations.add(step.getLocation());
		return locations;
	}	
	
}
