import java.net.URL;

public class Utils {

	/*
	 * This function works for both the JAR and when run in Eclipse with the project dir as the working dir
	 */
	public static URL getURL(String location){
		return Utils.class.getResource(location);
	}
	
	public static boolean emailAddressIsWellFormatted(String emailAddress){		
		//I assemble a regexp to recognize valid email addresses
		//Note that escape sequences must be double-escaped. We want to a '\' character fed to the reg-ex, but this
		//requires a '\\' in the string to escape the escape character.		
		String validAddrSection = "[a-zA-Z0-9\\!\\#\\$\\%\\*\\/\\?\\|\\^\\{\\}\\`\\~\\&\\'\\+\\-\\=\\_]+";
		String validLHSOrRHS = "("+validAddrSection + "\\.)*"+validAddrSection;		
		String emailAddressRegExp = validLHSOrRHS + "\\@" + validLHSOrRHS;		
		return emailAddress.matches(emailAddressRegExp);
	}

	public static boolean nameFieldIsWellFormatted(String name){
		return true;
	}	
	
	
	
}
