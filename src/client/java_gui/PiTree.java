import java.awt.Color;
import java.awt.Component;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Comparator;
import java.util.TreeSet;

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
import javax.swing.tree.TreeSelectionModel;

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
	private DefaultMutableTreeNode selectedNode, prevSelectedNode;
	private TreeSet<TreePath> viewableObjects; // All of a viewable nodes parents are expanded, but may or may not be displayed
	private boolean isExpandingNewlyAddedObjects;
	private static PiObjectComparator piObjectComparator = new PiObjectComparator();
	
	public PiTree(PiGui piGui, PiCode piCode) {
		super();
	    root = null;
	    treeModel = new DefaultTreeModel(root);
	    tree = new JTree(treeModel);
		this.piGui = piGui;
	    this.piCode = piCode;
	    selectedNode = prevSelectedNode = null;
	    viewableObjects = new TreeSet<TreePath>(piObjectComparator);
	    isExpandingNewlyAddedObjects = false;
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
					prevSelectedNode = selectedNode;
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
		        prevSelectedNode = selectedNode;
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
				viewableObjects.add(path);
				if (isExpandingNewlyAddedObjects)  // Don't magically expand things if we're just filling in what used to be expanded after a compile
					return;
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
			// On a collapse, this object and all its children from the list of expanded objects.
			public void treeCollapsed(TreeExpansionEvent e) {
				TreePath path = e.getPath();
				hideObject((DefaultMutableTreeNode)path.getLastPathComponent());
			}
			
			private void hideObject(DefaultMutableTreeNode node) {
				viewableObjects.remove(new TreePath(node.getPath()));
				for (int i = 0; i < node.getChildCount(); i++)
					hideObject((DefaultMutableTreeNode)node.getChildAt(i));
			}
		});
		// Draw icons next to things
		tree.setCellRenderer(new MyTreeCellRenderer());
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
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
		expandPreviouslyExpandedNodes();
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
			DefaultMutableTreeNode atomNode = new DefaultMutableTreeNode(atom);
			treeModel.insertNodeInto(atomNode, parentNode, parentNode.getChildCount());
			BasicPath bp = atom.getBP();
			if(bp!=null){
				addSteps(bp, atomNode);
			}
			if (atom.getValidity() == VerificationResult.validityT.INVALID)
				addCounterexample(atom.getCounterexample(), atomNode);
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
	}
	
	/**
	 * After we compile, ensure that all previously-expanded nodes
	 * are still expanded.  We also try to select the node that
	 * was selected before, if we can.
	 * Note that we make a copy of the set so that we can clear out
	 * nodes that were expanded but are now removed (such as Counterexamples
	 * for things we verified successfully).
	 */
	private void expandPreviouslyExpandedNodes() {
		isExpandingNewlyAddedObjects = true;
		TreeSet<TreePath> oldViewableObjects = new TreeSet<TreePath>(viewableObjects);
		viewableObjects.clear();
		recExpandPreviouslyExpandedNodes(root, oldViewableObjects);
		isExpandingNewlyAddedObjects = false;
	}
	
	/**
	 * Recursively expand this node if it used to be expanded
	 * and call ourself on its children.  Select the node that
	 * used to be selected.
	 */
	private void recExpandPreviouslyExpandedNodes(DefaultMutableTreeNode node, TreeSet<TreePath> oldViewableObjects) {
		TreePath pathToNode = new TreePath(node.getPath());
		if (oldViewableObjects.contains(pathToNode))
			tree.expandPath(pathToNode);
		if (prevSelectedNode != null && piObjectComparator.compare(new TreePath(prevSelectedNode.getPath()), pathToNode) == 0) {
			prevSelectedNode = selectedNode;
			selectedNode = node;
			nodeSelected(node.getUserObject());
			piGui.nodeSelected(node.getUserObject());
			tree.getSelectionModel().addSelectionPath(pathToNode);
		}
		for (int i = 0; i < node.getChildCount(); i++)
			recExpandPreviouslyExpandedNodes((DefaultMutableTreeNode)node.getChildAt(i), oldViewableObjects);
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
			if (variable.getLocation() != null) // some variables have null locations
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
		prevSelectedNode = selectedNode;
		selectedNode = null;
	}
	
	/**
	 * Call when we open a new file.
	 */
	public void openedNewFile() {
		clear();
		viewableObjects.clear();
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
			} else if (obj instanceof Counterexample.Variable) {
				Counterexample.Variable variable = (Counterexample.Variable)obj;
				setIcon(null);
				setText(variable.getText());
			} else if (obj instanceof String) {
				setIcon(null);
				setText((String)obj);
			}
			//setForeground(Color.PINK);
			//setBackground(Color.BLUE);

			return this;
		}
	}
	
	/**
	 * Compares our data structures.
	 */
	private static class PiObjectComparator implements Comparator<TreePath> {
		
		public int compare(TreePath aPath, TreePath bPath) {
			Object a = ((DefaultMutableTreeNode)aPath.getLastPathComponent()).getUserObject();
			Object b = ((DefaultMutableTreeNode)bPath.getLastPathComponent()).getUserObject();
			return compare(a, b, aPath, bPath);
		}

		public int compare(Object a, Object b, TreePath aPath, TreePath bPath) {
			String aClassName = a.getClass().getCanonicalName();
			String bClassName = b.getClass().getCanonicalName();
			if (aClassName.equals(bClassName)) {
				if (a instanceof VerificationResult)
					return ((VerificationResult)a).getFilename().compareTo(((VerificationResult)b).getFilename()); 
				else if (a instanceof Function)
					return ((Function)a).getName().compareTo(((Function)b).getName()); 
				else if (a instanceof VerificationAtomCollection) {
					Function aFunc = (Function)getLastObject(getParentPathFunction(aPath));
					Function bFunc = (Function)getLastObject(getParentPathFunction(bPath));
					String aIdent = aFunc.getName() + "." + ((VerificationAtomCollection)a).getLabel();
					String bIdent = bFunc.getName() + "." + ((VerificationAtomCollection)b).getLabel();
					return aIdent.compareTo(bIdent);
				} else if (a instanceof Termination) {
					TreePath aFunc = getParentPathFunction(aPath);
					TreePath bFunc = getParentPathFunction(bPath);
					return compare(getLastObject(aFunc), getLastObject(bFunc), aFunc, bFunc);
				} else if (a instanceof VerificationAtom) {
					int nameCmp = ((VerificationAtom)a).getIdentifier().compareTo(((VerificationAtom)b).getIdentifier());
					if (nameCmp != 0)
						return nameCmp;
					else {
						VerificationAtomCollection aParent = (VerificationAtomCollection)((DefaultMutableTreeNode)aPath.getParentPath().getLastPathComponent()).getUserObject();
						VerificationAtomCollection bParent = (VerificationAtomCollection)((DefaultMutableTreeNode)bPath.getParentPath().getLastPathComponent()).getUserObject();
						return aParent.getLabel().compareTo(bParent.getLabel());
					}
				} else if (a instanceof Step) {
					TreePath aVa = getParentPathVerificationAtom(aPath);
					TreePath bVa = getParentPathVerificationAtom(bPath);
					int parentCmp = compare(getLastObject(aVa), getLastObject(bVa), aVa, bVa);
					if (parentCmp != 0)
						return parentCmp;
					else
						return ((Step)a).getText().compareTo(((Step)b).getText());
				} else if (a instanceof Counterexample.Variable) {
					int strCmp = ((Counterexample.Variable)a).getText().compareTo(((Counterexample.Variable)b).getText());
					if (strCmp != 0)
						return strCmp;
					else
						return compare(aPath.getParentPath(), bPath.getParentPath());
				}else if (a instanceof String) {
					int strCmp = ((String)a).compareTo((String)b);
					if (strCmp != 0)
						return strCmp;
					else
						return compare(aPath.getParentPath(), bPath.getParentPath());
				} else
					throw new RuntimeException("Invalid object in the tree.");
			} else
				return aClassName.compareTo(bClassName);
		}
		
		private TreePath getParentPathFunction(TreePath path) {
			if (path == null)
				throw new RuntimeException("This path has no parent function.");
			Object obj = getLastObject(path);
			if (obj instanceof Function)
				return path;
			else
				return getParentPathFunction(path.getParentPath());
		}
		
		private TreePath getParentPathVerificationAtom(TreePath path) {
			if (path == null)
				throw new RuntimeException("This path has no parent verification atom.");
			Object obj = getLastObject(path);
			if (obj instanceof VerificationAtom)
				return path;
			else
				return getParentPathVerificationAtom(path.getParentPath());
		}
		
		private Object getLastObject(TreePath path) {
			return ((DefaultMutableTreeNode)path.getLastPathComponent()).getUserObject();
		}
		
	}

}
