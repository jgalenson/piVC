
import java.io.File;
import java.io.FileNotFoundException;

import javax.swing.JOptionPane;
import javax.xml.parsers.*;
import org.w3c.dom.*;

public class Config {
	private static final String CONFIG_FILE_NAME = "config.xml";
	private Document xml;
	
	public Config(){
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = null;
		try {
			builder = factory.newDocumentBuilder();
			xml = builder.parse(new File(CONFIG_FILE_NAME));
		} catch (FileNotFoundException e){
			JOptionPane.showMessageDialog(null, "The file '"+CONFIG_FILE_NAME+"' could not be found.\n\nIf you are checking this project out from SVN, then you should copy the file 'config_template.xml' to 'config.xml'.\n\nThis is a critical error; the program will now close.", "Configuration file not found", JOptionPane.ERROR_MESSAGE);
			System.exit(1);
		}catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	/* 
	 * Used for simple key/value config pairs. The key
	 * is the name of the xml node, and the value is the
	 * node's text.
	 */
	public String getValue(String key){
		Node root = xml.getChildNodes().item(0);
		NodeList nodes = root.getChildNodes();
		for(int i=0; i<nodes.getLength(); ++i){
			Node currNode = nodes.item(i);
			if(currNode.getNodeName().equals(key)){
				return pruneNewLines(currNode.getTextContent());
			}
		}
		return null;
	}
	
	private String pruneNewLines(String value){
		if(value.charAt(0)=='\n'){
			value = value.substring(1);
		}
		if(value.charAt(value.length()-1)=='\n'){
			value = value.substring(0, value.length()-1);
		}
		return value;
	}
	
}
