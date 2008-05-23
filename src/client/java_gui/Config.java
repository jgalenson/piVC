import java.io.*;
import java.util.*;

public class Config {
	
	private static final String CONFIG_FILE_LOCATION = System.getProperty("user.home")+"/.pivc-client";
	
	//All possible keys must be included in the DEFAULTS array
	//If a key is not included here, that key/value pair will
	//be purged from the config file upon program startup,
	//and an error will be thrown when that key's value is requested or set.
	private static final String[][] DEFAULTS = 	{
								{"server_address","127.0.0.1:4242"},
								{"pi_files_location","."},
								{"generate_runtime_assertions", "false"},
								{"find_inductive_core", "true"}
	};
	
	private static Map<String,String> settings;
	private static Set<String> validKeys;
	

	public static String getValue(String key){
		if(isValidKey(key)){
			return settings.get(key);
		}else{
			throw new RuntimeException("Invalid key: "+key+". Every key needs to be included in the defaults list.");
		}
	}
	
	public static boolean getBooleanValue(String key) {
		String val = getValue(key);
		if ("true".equals(val))
			return true;
		else if ("false".equals(val))
			return false;
		else
			throw new RuntimeException("The hey "+key+" should be either 'true' or 'false' but is currently " + val);
	}
	
	public static void setValue(String key, String newValue){
		if(!isValidKey(key)){
			throw new RuntimeException("Invalid key: "+key+". Every key needs to be included in the defaults list.");
		}else if(newValue.indexOf('=')!=-1 || newValue.indexOf('\n')!=-1 || newValue.indexOf('\r')!=-1){
			throw new RuntimeException("Invalid value: "+newValue+". Values cannot have an equals signs or line seperators.");			
		}
		else{
			settings.put(key, newValue);
			write();
		}
	}

	public static void setBooleanValue(String key, boolean value) {
		if (value)
			setValue(key, "true");
		else
			setValue(key, "false");
	}
	
	public static void initConfig(){
		settings = new HashMap<String,String>();
		validKeys = new HashSet<String>();
		
		boolean shouldWriteNewConfigFile = false;
		
		//Step 0: populate the map of valid keys
		for(String[] keyAndValue: DEFAULTS){
			if(keyAndValue.length!=2){
				throw new RuntimeException("Error: the default specification must be an array of length 2. The first element is the key and the second element is the default value.");
			}
			validKeys.add(keyAndValue[0]);
		}		
		
		//Step 1: load the key/value pairs from the file
		File configFile = getConfigFile();
		if(configFile.exists()){
			try {
				BufferedReader input = null;
				input = new BufferedReader(new FileReader(configFile));
				while(true){
					String line = input.readLine();				
					if(line==null)
						break;
					if(line.charAt(0)=='#' || line.length()==0)
						continue;
					int index = line.indexOf('=');
					if(index==-1)
						throw new RuntimeException("Malformatted config file");
					String lhs = line.substring(0,index).trim();
					String rhs = line.substring(index+1,line.length()).trim();
					if(isValidKey(lhs)){
						settings.put(lhs,rhs);
					}
					else{
						shouldWriteNewConfigFile = true;
					}
				}
				input.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		//Step 2: see if there are any keys that were not included in the file
		//If there were, we'll add them to the file
		for(String[] keyAndValue: DEFAULTS){
			if(keyAndValue.length!=2){
				throw new RuntimeException("Error: the default specification must be an array of length 2. The first element is the key and the second element is the default value.");
			}
			if(!settings.containsKey(keyAndValue[0])){
				settings.put(keyAndValue[0], keyAndValue[1]);
				shouldWriteNewConfigFile = true;
			}
		}
		
		//Step 3: rewrite the config file to remove deprecated keys, or to add new keys
		if(shouldWriteNewConfigFile){
			write();
		}		
	}
		
	

	private static boolean isValidKey(String key){
		return validKeys.contains(key);
	}
	
	private static File getConfigFile() {
		return new File(CONFIG_FILE_LOCATION);
	}	
	
	private static void write() {
		try {
			File f = getConfigFile();
			BufferedWriter output = new BufferedWriter(new FileWriter(f));
			output.write("#This is the configuration file for the piVC client.");
			output.newLine();
			output.write("#Each line is of the form key=value.");
			output.newLine();
			output.write("#Lines beginning with the # character are comments.");
			output.newLine();
			for(Map.Entry<String, String> setting:settings.entrySet()){
				output.write(setting.getKey() + "=" + setting.getValue());
				output.newLine();
			}
			output.close();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
