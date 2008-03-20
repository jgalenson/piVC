package data_structures;

import java.util.ArrayList;

public class Function {
	
	private String name;
	private boolean isValid;
	private ArrayList<BasicPath> basicPaths;
	private Location location;
	
	public Function(String name, boolean isValid, ArrayList<BasicPath> basicPaths, Location location) {
		this.name = name;
		this.isValid = isValid;
		this.basicPaths = basicPaths;
		this.location = location;
	}
	
	public String getName() {
		return name;
	}
	
	public boolean isValid() {
		return isValid;
	}
	
	public int getNumBasicPaths() {
		return basicPaths.size();
	}
	
	public BasicPath getBasicPath(int index) {
		return basicPaths.get(index);
	}
	
	public Location getLocation() {
		return location;
	}

}
