package data_structures;

public class Step {
	
	private String type;
	private String text;
	private Location location;

	public Step(String type, String text, Location location) {
		this.type = type;
		this.text = text;
		this.location = location;
	}
	
	public String getText() {
		return text;
	}
	
	public Location getLocation() {
		return location;
	}

}
