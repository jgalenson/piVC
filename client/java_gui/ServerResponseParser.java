import java.io.StringReader;
import java.util.ArrayList;

import javax.swing.JOptionPane;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;

import data_structures.BasicPath;
import data_structures.Function;
import data_structures.Location;
import data_structures.Step;
import data_structures.VerificationCondition;
import data_structures.VerificationResult;

public class ServerResponseParser {
	
	private PiGui piGui;
	private DocumentBuilder builder;
	
	public ServerResponseParser(PiGui piGui) {
		this.piGui = piGui;
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		try {
			builder = factory.newDocumentBuilder();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
			//JOptionPane.showMessageDialog(null, e.getMessage(), "Parse creator error", JOptionPane.ERROR_MESSAGE);
		}
	}

	public void parse(String text) {
		Document xml = null;
		StringReader reader = new StringReader(text);
		InputSource inputSource = new InputSource(reader);
		try {
			xml = builder.parse(inputSource);
			reader.close();
		} catch (Exception e) {
			e.printStackTrace();
			//JOptionPane.showMessageDialog(null, e.getMessage(), "Parsing error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		Node root = xml.getFirstChild();
		Node result = root.getChildNodes().item(1);
		String status = result.getAttributes().getNamedItem("status").getTextContent();
		if (status.equals("valid") || status.equals("invalid"))
			parseNormal(result);
		else if (status.equals("semantic_error") || status.equals("syntax_error"))
			parseError(result);
		//System.out.println("Finished parsing");
	}

	private void parseNormal(Node result) {
		String valid = result.getAttributes().getNamedItem("status").getTextContent();
		boolean isValid = valid.equals("valid") ? true : false;
		ArrayList<Function> functions = new ArrayList<Function>();
		NodeList children = result.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("function".equals(child.getNodeName()))  // Element node
				functions.add(parseFunction(child));
		}
		VerificationResult verificationResult = new VerificationResult(isValid, functions);
		piGui.handleVerificationResult(verificationResult);
	}

	private Function parseFunction(Node function) {
		String name = function.getAttributes().getNamedItem("name").getTextContent();
		ArrayList<BasicPath> basicPaths = new ArrayList<BasicPath>();
		NodeList children = function.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("basic_path".equals(child.getNodeName()))
				basicPaths.add(parseBasicPath(child));
		}
		if (basicPaths.size() == 0)
			throw new RuntimeException("Function has no basic paths");
		return new Function(name, basicPaths);
	}

	private BasicPath parseBasicPath(Node basicPath) {
		String valid = basicPath.getAttributes().getNamedItem("status").getTextContent();
		boolean isValid = valid.equals("valid") ? true : false;
		ArrayList<Step> steps = null;
		VerificationCondition vc = null;
		NodeList children = basicPath.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("path".equals(child.getNodeName()))
				steps = parsePath(child);
			if ("vc".equals(child.getNodeName()))
				vc = new VerificationCondition(child.getTextContent(), isValid);
		}
		if (steps == null || vc == null)
			throw new RuntimeException("Invalid basic_path tag");
		return new BasicPath(steps, vc, isValid);
	}

	private ArrayList<Step> parsePath(Node path) {
		ArrayList<Step> steps = new ArrayList<Step>();
		NodeList children = path.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("step".equals(child.getNodeName()))
				steps.add(parseStep(child));
		}
		return steps;
	}

	private Step parseStep(Node step) {
		String type = step.getAttributes().getNamedItem("type").getTextContent();
		String text = null;
		Location location = null;
		NodeList children = step.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("location".equals(child.getNodeName()))
				location = parseLocation(child);
			if ("text".equals(child.getNodeName()))
				text = child.getTextContent();
		}
		if (type == null || text == null || location == null)
			throw new RuntimeException("Invalid step tag");
		return new Step(type, text, location);
	}

	private Location parseLocation(Node location) {
		int startRow = -1, startCol = -1, endRow = -1, endCol = -1;
		NodeList children = location.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("start".equals(child.getNodeName())) {
				startRow = Integer.valueOf(child.getAttributes().getNamedItem("row").getTextContent());
				startCol = Integer.valueOf(child.getAttributes().getNamedItem("col").getTextContent());
			}
			if ("end".equals(child.getNodeName())) {
				endRow = Integer.valueOf(child.getAttributes().getNamedItem("row").getTextContent());
				endCol = Integer.valueOf(child.getAttributes().getNamedItem("col").getTextContent());
			}
		}
		if (startRow == -1 || startCol == -1 || endRow == -1 || endCol == -1)
			throw new RuntimeException("Invalid location tag");
		return new Location(startRow, startCol, endRow, endCol);
	}

	private void parseError(Node result) {
		// TODO Auto-generated method stub
		
	}

}
