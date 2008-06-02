import java.awt.Color;
import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import data_structures.BasicPath;
import data_structures.VerificationAtom;
import data_structures.Counterexample;
import data_structures.Function;
import data_structures.Step;
import data_structures.Termination;
import data_structures.VerificationAtomCollection;
import data_structures.VerificationCondition;
import data_structures.VerificationResult;

public class PiTree extends JPanel {
	
	/* Number of pixels to the left of the selectable object you
	 * can click without unselecting it.
	 */
	private static final int ROW_LEFT_GRACE_SPACE = 17; 
	
	private DefaultTreeModel treeModel;
	private JTree tree;
	private DefaultMutableTreeNode root;
	private PiGui piGui;
	private PiCode piCode;
	private DefaultMutableTreeNode selectedNode;
	
	public PiTree(PiGui piGui, PiCode piCode) {
		super();
	    root = null;
	    treeModel = new DefaultTreeModel(root);
	    tree = new JTree(treeModel);
		this.piGui = piGui;
	    this.piCode = piCode;
	    selectedNode = null;
		initTree();
	}
	
	private void initTree() {
		// Listen to clicks for selection and unselection
		tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.getButton() != MouseEvent.BUTTON1)
					return;
				int closestRow = tree.getClosestRowForLocation(e.getX(), e.getY());
				if (closestRow == -1) // nothing in tree
					return;
				Rectangle bounds = tree.getRowBounds(closestRow);
				// If they clicked on something already selected, we re-highlight if necessary
				if (bounds.contains(e.getPoint())) {  // Clicked on something
			        DefaultMutableTreeNode node = (DefaultMutableTreeNode)tree.getPathForLocation(e.getX(), e.getY()).getLastPathComponent();
			        if (node == selectedNode)
			        	nodeSelected(node.getUserObject());
				}
				// If they clicked elsewhere, we unselect and unhighlight.
				else if (e.getX() > bounds.getMaxX() || e.getY() > bounds.getMaxY() || e.getX() < bounds.getMinX() - ROW_LEFT_GRACE_SPACE) {
					selectedNode = null;
					tree.clearSelection();
					nodeSelected(null);
					piGui.nodeSelected(null);
				}
			}
		});
		// Selecting something highlights it.
		tree.addTreeSelectionListener(new TreeSelectionListener() {
			public void valueChanged(TreeSelectionEvent e) {
		        DefaultMutableTreeNode node = (DefaultMutableTreeNode)tree.getLastSelectedPathComponent();
		        selectedNode = node;
		        Object obj = (node == null ? null : node.getUserObject());
		        piGui.nodeSelected(obj);
		        if (node == null)
		        	return;
		        nodeSelected(obj);
		    }
		});
		// Tree expansion listener
		tree.addTreeExpansionListener(new TreeExpansionListener() {
			public void treeExpanded(TreeExpansionEvent e) {
				TreePath path = e.getPath();
				DefaultMutableTreeNode expandedNode = (DefaultMutableTreeNode)path.getLastPathComponent();
				Object expandedObj = expandedNode.getUserObject();
				// If a Function has no termination arguments, automatically expand the "Correctness" node when we expand the function.
				if (expandedObj instanceof Function && expandedNode.getChildCount() == 1) {
					DefaultMutableTreeNode child = (DefaultMutableTreeNode)expandedNode.getChildAt(0);
					tree.expandPath(new TreePath(child.getPath()));
				}
				// Automatically expand basic paths and nonnegative VCs.
				else if (expandedObj instanceof VerificationAtom) {
					for (int i = 0; i < expandedNode.getChildCount(); i++) {
						DefaultMutableTreeNode child = (DefaultMutableTreeNode)expandedNode.getChildAt(i);
						tree.expandPath(new TreePath(child.getPath()));
					}
				}
			}
			// ignore collapsed messages.
			public void treeCollapsed(TreeExpansionEvent e) {}
		});
		// Draw icons next to things
		tree.setCellRenderer(new MyTreeCellRenderer());
		tree.setShowsRootHandles(true);
		tree.setRootVisible(true);
		tree.setEditable(false);
		tree.setRowHeight(0);  // For counterexamples, so we can have differently-sized rows.
	}
	
	/**
	 * Add the return of this into the tabbed pane:
	 * do not add the PiTree object itself.
	 */
	public JScrollPane getTreeInScrollPane() {
		return new JScrollPane(tree);
	}
	
	/**
	 * Fills out the tree based on the compiler's information, basic
	 * paths, VCs, and all.
	 */
	public void handleVerificationResult(VerificationResult verificationResult) {
		root = new DefaultMutableTreeNode(verificationResult);
		treeModel.setRoot(root);
		addFunctions(verificationResult);
	}
	
	/**
	 * Adds the functions inside the VerificationResult to the tree as children of the root.
	 */
	private void addFunctions(VerificationResult verificationResult) {
		for (int i = 0; i < verificationResult.getNumFunctions(); i++) {
			Function function = verificationResult.getFunction(i);
			DefaultMutableTreeNode fnNode = new DefaultMutableTreeNode(function);
			treeModel.insertNodeInto(fnNode, root, root.getChildCount());
			VerificationAtomCollection correctness = function.getCorrectness();
			DefaultMutableTreeNode correctnessNode = new DefaultMutableTreeNode(correctness);
			treeModel.insertNodeInto(correctnessNode, fnNode, fnNode.getChildCount());
			addVerificationAtomCollection(correctness, correctnessNode);
			Termination termination = function.getTermination();
			if (termination != null) {
				DefaultMutableTreeNode terminationNode = new DefaultMutableTreeNode(termination);
				treeModel.insertNodeInto(terminationNode, fnNode, fnNode.getChildCount());
				addTermination(termination, terminationNode);
			}
			tree.makeVisible(new TreePath(fnNode.getPath()));
		}
	}

	/**
	 * Adds the basic paths and VCs inside a function to the tree as its children.
	 */
	private void addVerificationAtomCollection(VerificationAtomCollection parent, DefaultMutableTreeNode parentNode) {
		for (int i = 0; i < parent.getNumAtoms(); i++) {
			VerificationAtom atom = parent.getAtom(i);
			DefaultMutableTreeNode basicPathNode = new DefaultMutableTreeNode(atom);
			treeModel.insertNodeInto(basicPathNode, parentNode, parentNode.getChildCount());
			//DefaultMutableTreeNode vcNode = new DefaultMutableTreeNode(basicPath.getVC());
			//treeModel.insertNodeInto(vcNode, basicPathNode, basicPathNode.getChildCount());
			BasicPath bp = atom.getBP();
			if(bp!=null){
				addSteps(bp, basicPathNode);
			}
			if (atom.getValidity() == VerificationResult.validityT.INVALID)
				addCounterexample(atom.getCounterexample(), basicPathNode);
		}		
	}

	/**
	 * Adds the steps inside a basic path to the tree as its children.
	 */
	private void addSteps(BasicPath basicPath, DefaultMutableTreeNode atomNode) {
		DefaultMutableTreeNode parentStepNode = new DefaultMutableTreeNode("Steps");
		treeModel.insertNodeInto(parentStepNode, atomNode, atomNode.getChildCount());
		for (int i = 0; i < basicPath.getNumSteps(); i++) {
			Step step = basicPath.getStep(i);
			DefaultMutableTreeNode stepNode = new DefaultMutableTreeNode(step);
			treeModel.insertNodeInto(stepNode, parentStepNode, parentStepNode.getChildCount());
		}		
	}
	
	/**
	 * Adds a counterexample (a list of variables and their values) inside
	 * a basic path in the tree.
	 */
	private void addCounterexample(Counterexample counterexample, DefaultMutableTreeNode basicPathNode) {
		DefaultMutableTreeNode parentCounterexampleNode = new DefaultMutableTreeNode("Counterexample");
		treeModel.insertNodeInto(parentCounterexampleNode, basicPathNode, basicPathNode.getChildCount());
		for (int i = 0; i < counterexample.getNumVariables(); i++) {
			Counterexample.Variable variable = counterexample.getVariable(i);
			DefaultMutableTreeNode varNode = new DefaultMutableTreeNode(variable);
			treeModel.insertNodeInto(varNode, parentCounterexampleNode, parentCounterexampleNode.getChildCount());
		}		
	}
	
	private void addTermination(Termination termination, DefaultMutableTreeNode terminationNode) {
		VerificationAtomCollection decreasing = termination.getDecreasing();
		DefaultMutableTreeNode decreasingNode = new DefaultMutableTreeNode(decreasing);
		treeModel.insertNodeInto(decreasingNode, terminationNode, terminationNode.getChildCount());
		addVerificationAtomCollection(decreasing, decreasingNode);
		
		VerificationAtomCollection nonnegative = termination.getNonnegative();
		DefaultMutableTreeNode nonnegativeNode = new DefaultMutableTreeNode(nonnegative);
		treeModel.insertNodeInto(nonnegativeNode, terminationNode, terminationNode.getChildCount());
		addVerificationAtomCollection(nonnegative, nonnegativeNode);
		
		/*
		for (int i = 0; i < nonnegative.getNumAtoms(); i++) {
			Termination.Nonnegative.NonnegativeVerificationCondition nonnegativeVC = nonnegative.getNonnegativeVC(i);
			DefaultMutableTreeNode nonnegativeVCNode = new DefaultMutableTreeNode(nonnegativeVC);
			treeModel.insertNodeInto(nonnegativeVCNode, nonnegativeNode, nonnegativeNode.getChildCount());
			//DefaultMutableTreeNode vcNode = new DefaultMutableTreeNode(nonnegativeVC.getVC());
			//treeModel.insertNodeInto(vcNode, nonnegativeVCNode, nonnegativeVCNode.getChildCount());
			if (nonnegativeVC.getCounterexample() != null)
				addCounterexample(nonnegativeVC.getCounterexample(), nonnegativeVCNode);
		}*/
	}
	
	/**
	 * When a node is selected, we highlight it
	 * depending on what type of node it is.
	 * We also highlight the corresponding VC.
	 */
	private void nodeSelected(Object obj) {
		if (obj instanceof Step) {
			Step step = (Step)obj;
			piCode.highlight(step.getLocation(), PiCode.yellowHP);
		} else if (obj instanceof VerificationAtom) {
			VerificationAtom basicPath = (VerificationAtom)obj;
			piCode.highlight(basicPath.getLocations(), PiCode.yellowHP);
		} else if (obj instanceof Function) {
			Function function = (Function)obj;
			piCode.highlight(function.getLocation(), PiCode.yellowHP);
		} else if (obj instanceof Counterexample.Variable) {
			Counterexample.Variable variable = (Counterexample.Variable)obj;
			piCode.highlight(variable.getLocation(), PiCode.yellowHP);
		} else if (obj instanceof String) {
			String str = (String)obj;
			if ("Steps".equals(str))
				nodeSelected(((DefaultMutableTreeNode)selectedNode.getParent()).getUserObject());
			else
				piCode.removeAllHighlights();
		} else
			piCode.removeAllHighlights();
		// highlight vc
		VerificationCondition currVC = getCorrespondingVC();
		if (currVC == null)
			piGui.getVCPane().setNothing();
		else
			piGui.getVCPane().setVC(currVC);
	}
	
	private VerificationCondition getCorrespondingVC(){
		DefaultMutableTreeNode curr = selectedNode;
		while(true){
			if(curr==null){
				return null;
			}
			Object obj = curr.getUserObject();
			if(obj instanceof VerificationAtom){
				return ((VerificationAtom)obj).getVC();
			}
			else{
				curr=(DefaultMutableTreeNode)curr.getParent();
			}
		}
	}
	
	/**
	 * Reselects (i.e. highlights) the currently-selected node.
	 * We use this in the BasicPathHighlighter in case the 
	 * user has selected something new while we are displaying
	 * the old path.
	 */
	public void reselectSelectedNode() {
		nodeSelected(selectedNode.getUserObject());
	}
	
	/**
	 * Returns the currently-selected object.
	 */
	public Object getSelectedObject() {
		if (selectedNode == null)
			return null;
		else
			return selectedNode.getUserObject();
	}
	
	/**
	 * Empties out the tree.
	 */
	public void clear() {
		root = null;
		treeModel.setRoot(root);
		selectedNode = null;
	}
	
	/**
	 * A class that lets us customize how we draw nodes.
	 * We can specify a node's text and image.
	 */
	private class MyTreeCellRenderer extends DefaultTreeCellRenderer {

		@Override
		public Color getBackgroundNonSelectionColor(){
			return null;
		}

		@Override
		public Color getBackground(){
			return null;
		}		
		
		private ImageIcon valid, invalid, unknown;
		
		public MyTreeCellRenderer() {
			valid = new ImageIcon(Utils.getURL("images/valid.jpg"));
			invalid = new ImageIcon(Utils.getURL("images/invalid.jpg"));
			unknown = new ImageIcon(Utils.getURL("images/unknown.jpg"));
	        //setBackgroundNonSelectionColor(Color.WHITE);//this did the trick
		}
		
		private ImageIcon getProperIcon(VerificationResult.validityT validity) {
			if (validity == VerificationResult.validityT.VALID)
				return valid;
			else if (validity == VerificationResult.validityT.INVALID)
				return invalid;
			else if (validity == VerificationResult.validityT.UNKNOWN)
				return unknown;
			else throw new RuntimeException("Unrecognized validity");
		}
		
		/**
		 * Does the actual specification.
		 */
		@Override
		@SuppressWarnings("hiding")
		public Component getTreeCellRendererComponent(JTree tree, Object value, 
				boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
			super.getTreeCellRendererComponent(tree, value, sel,expanded, leaf, row, hasFocus);
			Object obj = ((DefaultMutableTreeNode)value).getUserObject();
			if (obj instanceof VerificationResult) {
				VerificationResult verificationResult = (VerificationResult)obj;
				setIcon(getProperIcon(verificationResult.getValidity()));
				String name = verificationResult.getFilename() != "" ? verificationResult.getFilename() : "Program";
				setText(name);
			} else if (obj instanceof Function) {
				Function function = (Function)obj;
				setIcon(getProperIcon(function.getValidity()));
				setText(function.getName());
			} else if (obj instanceof VerificationAtomCollection) {
				VerificationAtomCollection vcCollection = (VerificationAtomCollection)obj;
				setIcon(getProperIcon(vcCollection.getValidity()));
				setText(vcCollection.getLabel());
			} else if (obj instanceof Termination) {
				Termination termination = (Termination)obj;
				setIcon(getProperIcon(termination.getValidity()));
				setText("Termination");
			} else if (obj instanceof VerificationAtom) {
				VerificationAtom atom = (VerificationAtom)obj;
				setIcon(getProperIcon(atom.getValidity()));
				String name = atom.getIdentifier().replace("\\u2192", '\u2192' + "");
				setText(name);
			} else if (obj instanceof Step) {
				Step step = (Step)obj;
				setIcon(null);
				setText(step.getText());
			} /*else if (obj instanceof VerificationCondition) { //VCs no longer go in tree, so this is commented out
				VerificationCondition vc = (VerificationCondition)obj;
				setIcon(getProperIcon(vc.getValidity()));
			}*/ else if (obj instanceof Counterexample.Variable) {
				Counterexample.Variable variable = (Counterexample.Variable)obj;
				setIcon(null);
				setText(variable.getText());
			} /*else if (obj instanceof Termination.Decreasing) {
				Termination.Decreasing decreasing = (Termination.Decreasing)obj;
				setIcon(getProperIcon(decreasing.getValidity()));
				setText("Decreasing");
			} else if (obj instanceof Termination.Nonnegative) {
				Termination.Nonnegative nonnegative = (Termination.Nonnegative)obj;
				setIcon(getProperIcon(nonnegative.getValidity()));
				setText("Nonnegative");
			} else if (obj instanceof Termination.Nonnegative.NonnegativeVerificationCondition) {
				Termination.Nonnegative.NonnegativeVerificationCondition nonnegativeVC = (Termination.Nonnegative.NonnegativeVerificationCondition)obj;
				setIcon(getProperIcon(nonnegativeVC.getValidity()));
				setText("Nonnegative VC");
			}*/ else if (obj instanceof String) {
				setIcon(null);
				setText((String)obj);
			}
			//setForeground(Color.PINK);
			//setBackground(Color.BLUE);

			return this;
		}
	}

}
