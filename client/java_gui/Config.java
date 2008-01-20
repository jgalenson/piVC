
import java.io.File;
import javax.xml.parsers.*;
import org.w3c.dom.*;

public class Config {
	private Document xml;
	
	public Config(){
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		DocumentBuilder builder = null;
		try {
			builder = factory.newDocumentBuilder();
			xml = builder.parse(new File("config.xml"));
		} catch (Exception e) {
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
