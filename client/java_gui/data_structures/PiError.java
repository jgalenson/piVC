package data_structures;

/**
 * A compiler error.
 */
public class PiError {
	
	private enum errorTypeT {Syntax, Semantic};
	
	private errorTypeT errorType;
	private String msg;
	private Location location;
	
	private PiError(errorTypeT errorType, String msg, Location location) {
		this.errorType = errorType;
		this.msg = msg;
		this.location = location;
	}
	
	/**
	 * Use this to create an error.
	 */
	public static PiError makeError(String type, String msg, Location location) {
		if ("syntax_error".equals(type))
			return new PiError(errorTypeT.Syntax, msg, location);
		else if ("semantic_error".equals(type))
			return new PiError(errorTypeT.Semantic, msg, location);
		else
			throw new IllegalArgumentException("Illegal type passed to PiError: " + type);
	}
	
	public String getMessage() {
		return (getTypeString() + "<p>" + msg);
	}
	
	private String getTypeString() {
		if (errorType == errorTypeT.Syntax)
			return "Syntax error:";
		else
			return "Semantic error:";
	}
	
	public Location getLocation() {
		return location;
	}

}
