package data_structures;

import java.util.ArrayList;

public class Function {
	
	private String name;
	private ArrayList<BasicPath> basicPaths;
	
	public Function(String name, ArrayList<BasicPath> basicPaths) {
		this.name = name;
		this.basicPaths = basicPaths;
	}
	
	public String getName() {
		return name;
	}

}
