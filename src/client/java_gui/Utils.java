import java.net.URL;


public class Utils {

	/*
	 * This function works for both the JAR and when run in Eclipse with the project dir as the working dir
	 */
	public static URL getURL(String location){
		return Utils.class.getResource(location);
	}
	
}
