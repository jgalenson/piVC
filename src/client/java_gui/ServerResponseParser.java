import java.io.StringReader;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import data_structures.BasicPath;
import data_structures.Conjunct;
import data_structures.Correctness;
import data_structures.Counterexample;
import data_structures.Function;
import data_structures.Location;
import data_structures.PiError;
import data_structures.RHSConjunct;
import data_structures.Step;
import data_structures.Termination;
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
	public void parse(String text, String filename) {
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
		if (status.equals("valid") || status.equals("invalid") || status.equals("unknown"))
			parseNormal(result, filename);
		else if (status.equals("error"))
			parseErrors(result);
		else if (status.equals("compiler_error"))
			parseCompilerError(result);
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
		else
			throw new RuntimeException("Unrecognized validity type.");
	}
	
	/**
	 * Makes and returns a Function object from a <function> tag.
	 */
	private Function parseFunction(Node function) {
		String name = function.getAttributes().getNamedItem("name").getTextContent();
		String valid = function.getAttributes().getNamedItem("status").getTextContent();
		Correctness correctness = null;
		Termination termination = null;
		Location location = null;
		NodeList children = function.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("correctness".equals(child.getNodeName()))
				correctness = parseCorrectness(child);
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
	private Correctness parseCorrectness(Node correctness) {
		String valid = correctness.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		ArrayList<BasicPath> basicPaths = new ArrayList<BasicPath>();
		NodeList children = correctness.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("basic_path".equals(child.getNodeName()))
				basicPaths.add(parseBasicPath(child));
		}
		if (basicPaths.size() == 0)
			throw new RuntimeException("Function has no basic paths");
		return new Correctness(validity, basicPaths);
	}
	
	/**
	 * Makes and returns a Correctness object from a <termination> tag.
	 */
	private Termination parseTermination(Node termination) {
		String valid = termination.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		Termination.Decreasing decreasing = null;
		Termination.Nonnegative nonnegative = null;
		NodeList children = termination.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("decreasing".equals(child.getNodeName()))
				decreasing = parseDecreasing(child);
			if ("nonnegative".equals(child.getNodeName()))
				nonnegative = parseNonnegative(child);
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
					if(conjunctNode.getNodeName().equals("conjunct") || conjunctNode.getNodeName().equals("rhs_conjunct")){
						String str=null;
						Location loc=null;
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
						if(str==null){
							throw new RuntimeException("No text node in VC conjunct xml");
						}
						if(loc==null){
							throw new RuntimeException("No location node in VC conjunct xml");
						}
						NamedNodeMap attributes = conjunctNode.getAttributes();
						boolean inInductiveCore = Boolean.parseBoolean(attributes.getNamedItem("in_inductive_core").getNodeValue());
						Conjunct curr;
						if(conjunctNode.getNodeName().equals("rhs_conjunct")){
							validityT status = VerificationResult.parseValidity(attributes.getNamedItem("status").getNodeValue());					
							curr = new RHSConjunct(str,inInductiveCore,loc,status);
						}else{
							curr = new Conjunct(str,inInductiveCore,loc);
						}
						conjuncts.add(curr);
					}
				}
				implies.add(conjuncts.toArray(new Conjunct[0]));
			}
		}
		return new VerificationCondition(implies.toArray(new Conjunct[0][]),validity);
	}
	
	
	/**
	 * Makes and returns a BasicPath object from a <basic_path> tag.
	 */
	private BasicPath parseBasicPath(Node basicPath) {
		String valid = basicPath.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		ArrayList<Step> steps = null;
		VerificationCondition vc = null;
		Counterexample counterexample = null;
		NodeList children = basicPath.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("path".equals(child.getNodeName()))
				steps = parsePath(child);
			if ("vc".equals(child.getNodeName()))
				vc = parseVerificationCondition(child, validity);
			if ("counterexample".equals(child.getNodeName()))
				counterexample = parseCounterexample(child);
		}
		if (steps == null || vc == null || (vc.getValidity() == VerificationResult.validityT.INVALID && counterexample == null))
			throw new RuntimeException("Invalid basic_path tag");
		return new BasicPath(steps, vc, validity, counterexample);
	}

	/**
	 * Makes and returns a list of Step objects from a <path> tag.
	 */
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
		if (text == null || location == null)
			throw new RuntimeException("Invalid var tag");
		return new Counterexample.Variable(text, location);
	}

	/**
	 * Makes and returns a Termination.Decreasing object from a <decreasing> tag.
	 */
	private Termination.Decreasing parseDecreasing(Node decreasing) {
		String valid = decreasing.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		ArrayList<BasicPath> basicPaths = new ArrayList<BasicPath>();
		NodeList children = decreasing.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("basic_path".equals(child.getNodeName()))
				basicPaths.add(parseBasicPath(child));
		}
		if (basicPaths.size() == 0)
			throw new RuntimeException("Decreasing node has no basic paths");
		return new Termination.Decreasing(validity, basicPaths);
	}

	/**
	 * Makes and returns a Termination.Decreasing object from a <decreasing> tag.
	 */
	private Termination.Nonnegative parseNonnegative(Node nonnegative) {
		String valid = nonnegative.getAttributes().getNamedItem("status").getTextContent();
		VerificationResult.validityT validity = validityStringToValidity(valid);
		ArrayList<Termination.Nonnegative.NonnegativeVerificationCondition> nonnegativeVCs = new ArrayList<Termination.Nonnegative.NonnegativeVerificationCondition>();
		NodeList children = nonnegative.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("nonnegative_vc".equals(child.getNodeName()))
				nonnegativeVCs.add(parseNonnegativeVerificationCondition(child));
		}
		if (nonnegativeVCs.size() == 0)
			throw new RuntimeException("Nonnegative node has no VCs");
		return new Termination.Nonnegative(validity, nonnegativeVCs);
	}
	
	/**
	 * Makes and returns a Termination.Nonnegative.NonnegativeVerificationCondition object from a <nonnegative_vc> tag.
	 */
	private Termination.Nonnegative.NonnegativeVerificationCondition parseNonnegativeVerificationCondition(Node vcNode) {
		VerificationResult.validityT validity = validityStringToValidity(vcNode.getAttributes().getNamedItem("status").getTextContent());
		VerificationCondition vc = null;
		Counterexample counterexample = null;
		Location location = null;
		NodeList children = vcNode.getChildNodes();
		for (int i = 0; i < children.getLength(); i++) {
			Node child = children.item(i);
			if ("vc".equals(child.getNodeName()))
				vc = parseVerificationCondition(child, validity);
			if ("counterexample".equals(child.getNodeName()))
				counterexample = parseCounterexample(child);
			if ("location".equals(child.getNodeName()))
				location = parseLocation(child);
		}
		if (vc == null || (validity == VerificationResult.validityT.INVALID && counterexample == null) || location == null)
			throw new RuntimeException("Invalid nonnegative_vc tag");
		return new Termination.Nonnegative.NonnegativeVerificationCondition(validity, vc, counterexample, location);
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
		return new Location(startByte, endByte, startRow, startCol, endRow, endCol);
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
