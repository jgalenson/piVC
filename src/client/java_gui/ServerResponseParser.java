import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import data_structures.BasicPath;
import data_structures.VerificationAtom;
import data_structures.Conjunct;
import data_structures.Counterexample;
import data_structures.Function;
import data_structures.Location;
import data_structures.PiError;
import data_structures.Step;
import data_structures.Termination;
import data_structures.VerificationAtomCollection;
import data_structures.VerificationCondition;
import data_structures.VerificationResult;
import data_structures.VerificationResult.validityT;

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

	/**
	 * Parse the given text from the server into our own data structures.
	 * @param filename the name of the currently-opened file
	 * or null if it has no name.
	 */
	public String[] parse(String text, String filename) {
		Document xml = null;
		StringReader reader = new StringReader(text);
		InputSource inputSource = new InputSource(reader);
		try {
			xml = builder.parse(inputSource);
			reader.close();
		} catch (Exception e) {
			e.printStackTrace();
			//JOptionPane.showMessageDialog(null, e.getMessage(), "Parsing error", JOptionPane.ERROR_MESSAGE);
			return null;
		}
		Node root = xml.getFirstChild();
		NodeList children = root.getChildNodes();		
		Node result = null;
		String[] messages = null;
		for(int i=0; i<children.getLength(); ++i){
			Node child = children.item(i);
			if (child.getNodeName().equals("result")){
				result = child;
			}
			if (child.getNodeName().equals("messages")){
				messages = parseMessages(child);
			}			
		}		
		if(result!=null){
			String status = result.getAttributes().getNamedItem("status").getTextContent();
			if (status.equals("valid") || status.equals("invalid") || status.equals("unknown") || status.equals("timeout")){
				parseNormal(result, filename);
			}
			else if (status.equals("error")){
				parseErrors(result);
			}
			else if (status.equals("compiler_error")){
				parseCompilerError(result);
			}
		}
		return messages;
	}

	/**
	 * Makes a VerificationResult object from a <result> tag
	 * and passes it onto the main GUI.
	 */
	private void parseNormal(Node result, String filename) {
		String valid = result.getAttributes().getNamedItem("status").getTextContent();
		ArrayList<Function> functions = new ArrayList<Function>();
		NodeList children = result.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("function".equals(child.getNodeName()))  // Element node
				functions.add(parseFunction(child));
		}
		VerificationResult verificationResult = new VerificationResult(filename, validityStringToValidity(valid), functions);
		piGui.handleVerificationResult(verificationResult);
	}

	private VerificationResult.validityT validityStringToValidity(String validity){
		if(validity.equals("valid"))
			return VerificationResult.validityT.VALID;
		else if (validity.equals("invalid"))
			return VerificationResult.validityT.INVALID;
		else if (validity.equals("unknown"))
			return VerificationResult.validityT.UNKNOWN;
		else if (validity.equals("timeout"))
			return VerificationResult.validityT.TIMEOUT;
		else
			throw new RuntimeException("Unrecognized validity type.");
	}
	
	
	/**
	 * Makes and returns a Function object from a <function> tag.
	 */
	private String[] parseMessages(Node messagesNode) {
		List<String> messages = new ArrayList<String>();
		NodeList children = messagesNode.getChildNodes();
		for(int i=0; i<children.getLength(); ++i){
			Node child = children.item(i);
			if (child.getNodeName().equals("message")){
				messages.add(child.getTextContent());
			}		
		}			
		return messages.toArray(new String[0]);
	}	
	
	/**
	 * Makes and returns a Function object from a <function> tag.
	 */
	private Function parseFunction(Node function) {
		String name = function.getAttributes().getNamedItem("name").getTextContent();
		String valid = function.getAttributes().getNamedItem("status").getTextContent();
		VerificationAtomCollection correctness = null;
		Termination termination = null;
		Location location = null;
		NodeList children = function.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("correctness".equals(child.getNodeName()))
				correctness = parseVerificationAtomCollection(child, "Correctness");
			if ("termination".equals(child.getNodeName()))
				termination = parseTermination(child);
			if ("location".equals(child.getNodeName()))
				location = parseLocation(child);
		}
		if (correctness == null || location == null)
			throw new RuntimeException("Invalid function tag");
		return new Function(name, validityStringToValidity(valid), correctness, termination, location);
	}
	
	/**
	 * Makes and returns a Correctness object from a <correctness> tag.
	 */
	private VerificationAtomCollection parseVerificationAtomCollection(Node collection, String label) {
		String valid = collection.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		ArrayList<VerificationAtom> atoms = new ArrayList<VerificationAtom>();
		NodeList children = collection.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("verification_atom".equals(child.getNodeName()))
				atoms.add(parseVerificationAtom(child));
		}
		if (atoms.size() == 0)
			throw new RuntimeException("List of atoms is empty");
		return new VerificationAtomCollection(validity, atoms, label);
	}
	
	/**
	 * Makes and returns a Correctness object from a <termination> tag.
	 */
	private Termination parseTermination(Node termination) {
		String valid = termination.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		VerificationAtomCollection decreasing = null;
		VerificationAtomCollection nonnegative = null;
		NodeList children = termination.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("decreasing".equals(child.getNodeName()))
				decreasing = parseVerificationAtomCollection(child, "Decreasing");
			if ("nonnegative".equals(child.getNodeName()))
				nonnegative = parseVerificationAtomCollection(child, "Nonnegative");
		}
		if (decreasing == null || nonnegative == null)
			throw new RuntimeException("Invalid termination tag");
		return new Termination(validity, decreasing, nonnegative);
	}

	private VerificationCondition parseVerificationCondition(Node vc, VerificationResult.validityT validity){
		NodeList impliesNodes = vc.getChildNodes();
		ArrayList<Conjunct[]> implies = new ArrayList<Conjunct[]>();
		for (int i = 0; i < impliesNodes.getLength(); i++) {
			Node impliesNode = impliesNodes.item(i);
			if(impliesNode.getNodeName().equals("implies")){
				NodeList conjunctNodes = impliesNode.getChildNodes();
				ArrayList<Conjunct> conjuncts = new ArrayList<Conjunct>();
				for (int c = 0; c < conjunctNodes.getLength(); c++) {
					Node conjunctNode = conjunctNodes.item(c);
					if(conjunctNode.getNodeName().equals("conjunct")){
						String str=null;
						Location loc=null;
						Boolean inInductiveCore=null;
						validityT status=null;
						NodeList children = conjunctNode.getChildNodes();
						for(int i2=0; i2<children.getLength(); ++i2){
							Node child = children.item(i2);
							if(child.getNodeName().equals("text")){
								str = child.getTextContent();
							}
							if(child.getNodeName().equals("location")){
								loc = parseLocation(child);
							}
						}
						NamedNodeMap attributes = conjunctNode.getAttributes();
						for(int i2=0; i2<attributes.getLength(); ++i2){
							Node child = attributes.item(i2);
							if(child.getNodeName().equals("status")){
								status = VerificationResult.parseValidity(child.getNodeValue());
							}
							if(child.getNodeName().equals("in_inductive_core")){
								inInductiveCore = new Boolean(Boolean.parseBoolean(child.getNodeValue()));
							}
						}						

						if(str==null){
							throw new RuntimeException("No text node in VC conjunct xml");
						}
						if(loc==null){
							throw new RuntimeException("No location node in VC conjunct xml");
						}
						Conjunct curr = new Conjunct(str,status,inInductiveCore,loc);
						conjuncts.add(curr);
					}
				}
				implies.add(conjuncts.toArray(new Conjunct[0]));
			}
		}
		return new VerificationCondition(implies.toArray(new Conjunct[0][]),validity);
	}
	
	
	/**
	 * Makes and returns a BasicPath object from a <verification_atom> tag.
	 */
	private VerificationAtom parseVerificationAtom(Node atom) {
		String valid = atom.getAttributes().getNamedItem("status").getTextContent();
		String name = atom.getAttributes().getNamedItem("name").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		BasicPath bp = null;
		VerificationCondition vc = null;
		Counterexample counterexample = null;
		Location location = null;
		NodeList children = atom.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("basic_path".equals(child.getNodeName()))
				bp = parseBasicPath(child);
			if ("vc".equals(child.getNodeName()))
				vc = parseVerificationCondition(child, validity);
			if ("counterexample".equals(child.getNodeName()))
				counterexample = parseCounterexample(child);
			if("location".equals(child.getNodeName())){
				location = parseLocation(child);
			}
		}
		if (vc == null || (vc.getValidity() == VerificationResult.validityT.INVALID && counterexample == null))
			throw new RuntimeException("Invalid verification_atom tag");
		return new VerificationAtom(bp, vc, validity, counterexample, name, location);
	}

	/**
	 * Makes and returns a list of Step objects from a <path> tag.
	 */
	private BasicPath parseBasicPath(Node path) {
		ArrayList<Step> steps = new ArrayList<Step>();
		NodeList children = path.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("step".equals(child.getNodeName()))
				steps.add(parseStep(child));
		}
		return new BasicPath(steps);
	}

	/**
	 * Makes and returns a Step object from a <step> tag.
	 */
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
	
	/**
	 * Makes and returns a Counterexample object from a <counterexample> tag.
	 */
	private Counterexample parseCounterexample(Node counterexample) {
		ArrayList<Counterexample.Variable> variables = new ArrayList<Counterexample.Variable>();
		NodeList children = counterexample.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("var".equals(child.getNodeName()))
				variables.add(parseVariable(child));
		}
		return new Counterexample(variables);
	}
	
	/**
	 * Makes and returns a Counterexample.Variable object from a <var> tag.
	 */
	private Counterexample.Variable parseVariable(Node var) {
		String text = var.getAttributes().getNamedItem("text").getTextContent();;
		Location location = null;
		NodeList children = var.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("location".equals(child.getNodeName()))
				location = parseLocation(child);
		}
		if (text == null)
			throw new RuntimeException("Invalid var tag");
		return new Counterexample.Variable(text, location);
	}

	/**
	 * Makes and returns a Location object from a <location> tag.
	 */
	private Location parseLocation(Node location) {
		int startRow = -1, startCol = -1, endRow = -1, endCol = -1, startByte = -1, endByte = -1;
		NodeList children = location.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("start".equals(child.getNodeName())) {
				startRow = Integer.valueOf(child.getAttributes().getNamedItem("row").getTextContent());
				startCol = Integer.valueOf(child.getAttributes().getNamedItem("col").getTextContent());
				startByte = Integer.valueOf(child.getAttributes().getNamedItem("byte").getTextContent());
			}
			if ("end".equals(child.getNodeName())) {
				endRow = Integer.valueOf(child.getAttributes().getNamedItem("row").getTextContent());
				endCol = Integer.valueOf(child.getAttributes().getNamedItem("col").getTextContent());
				endByte = Integer.valueOf(child.getAttributes().getNamedItem("byte").getTextContent());
			}
		}
		if (startRow == -1 || startCol == -1 || endRow == -1 || endCol == -1 || startByte == -1 || endByte == -1)
			throw new RuntimeException("Invalid location tag");
		return new Location(startByte, startRow, startCol, endByte, endRow, endCol);
	}

	/**
	 * Parses errors from a <result> tag
	 * and passes them onto the main GUI.
	 */
	private void parseErrors(Node result) {
		ArrayList<PiError> errors = new ArrayList<PiError>();
		NodeList children = result.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("error".equals(child.getNodeName()))  // Element node
				errors.add(parseError(child));
		}
		piGui.handleError(errors);
	}
	
	/**
	 * Makes and returns a PiError object from an <error> tag.
	 */
	private PiError parseError(Node error) {
		String type = error.getAttributes().getNamedItem("type").getTextContent();
		String msg = null;
		Location location = null;
		NodeList children = error.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("location".equals(child.getNodeName()))
				location = parseLocation(child);
			if ("message".equals(child.getNodeName()))
				msg = child.getTextContent();
		}
		if (type == null || msg == null || (!type.equals("compiler_error") && location == null))
			throw new RuntimeException("Invalid error tag");
		return PiError.makeError(type, msg, location);
	}
	
	/**
	 * Parses a compiler error from a <result> tag
	 * and passes it onto the main GUI.
	 */
	private void parseCompilerError(Node result) {
		PiError error = null;
		NodeList children = result.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("error".equals(child.getNodeName()))  // Element node
				error = parseError(child);
		}
		if (error == null)
			throw new RuntimeException("Invalid compiler_error tag");
		piGui.handleCompilerError(error);
	}

}
