package data_structures;

import java.util.ArrayList;

public class Function {
	
	private String name;
	private int validity;
	private ArrayList<BasicPath> basicPaths;
	private Location location;
	
	public Function(String name, int validity, ArrayList<BasicPath> basicPaths, Location location) {
		this.name = name;
		this.validity = validity;
		this.basicPaths = basicPaths;
		this.location = location;
	}
	
	public String getName() {
		return name;
	}
	
	public int getValidity() {
		return validity;
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
