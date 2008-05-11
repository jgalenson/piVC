import java.net.URL;


public class Utils {

	public static URL getURL(String location){
		return Utils.class.getResource(location);
	}
	
}
