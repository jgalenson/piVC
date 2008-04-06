import java.awt.Component;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import data_structures.BasicPath;
import data_structures.Counterexample;
import data_structures.Function;
import data_structures.Step;
import data_structures.VerificationCondition;
import data_structures.VerificationResult;

public class PiTree extends JPanel {
	
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
		// Listen to clicks
		/*tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.getButton() != MouseEvent.BUTTON1)
					return;
				int selRow = tree.getRowForLocation(e.getX(), e.getY());
				TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
				if(selRow != -1) {
					if(e.getClickCount() == 1) {
						DefaultMutableTreeNode node = (DefaultMutableTreeNode)selPath.getLastPathComponent();
						nodeSelected(node.getUserObject());
					} else if(e.getClickCount() == 2)
						;// do something?
				}
			}
		});*/
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
			addBasicPaths(function, fnNode);
			tree.makeVisible(new TreePath(fnNode.getPath()));
		}
	}

	/**
	 * Adds the basic paths and VCs inside a function to the tree as its children.
	 */
	private void addBasicPaths(Function function, DefaultMutableTreeNode fnNode) {
		for (int i = 0; i < function.getNumBasicPaths(); i++) {
			BasicPath basicPath = function.getBasicPath(i);
			DefaultMutableTreeNode basicPathNode = new DefaultMutableTreeNode(basicPath);
			treeModel.insertNodeInto(basicPathNode, fnNode, fnNode.getChildCount());
			DefaultMutableTreeNode vcNode = new DefaultMutableTreeNode(basicPath.getVC());
			treeModel.insertNodeInto(vcNode, basicPathNode, basicPathNode.getChildCount());
			addSteps(basicPath, basicPathNode);
			if (!basicPath.isValid())
				addCounterexample(basicPath.getCounterexample(), basicPathNode);
		}		
	}

	/**
	 * Adds the steps inside a basic path to the tree as its children.
	 */
	private void addSteps(BasicPath basicPath, DefaultMutableTreeNode basicPathNode) {
		DefaultMutableTreeNode parentStepNode = new DefaultMutableTreeNode("Steps");
		treeModel.insertNodeInto(parentStepNode, basicPathNode, basicPathNode.getChildCount());
		for (int i = 0; i < basicPath.getNumSteps(); i++) {
			Step step = basicPath.getStep(i);
			DefaultMutableTreeNode stepNode = new DefaultMutableTreeNode(step);
			treeModel.insertNodeInto(stepNode, parentStepNode, parentStepNode.getChildCount());
		}		
	}
	
	private void addCounterexample(Counterexample counterexample, DefaultMutableTreeNode basicPathNode) {
		DefaultMutableTreeNode parentCounterexampleNode = new DefaultMutableTreeNode("Counterexample");
		treeModel.insertNodeInto(parentCounterexampleNode, basicPathNode, basicPathNode.getChildCount());
		for (int i = 0; i < counterexample.getNumVariables(); i++) {
			String variable = counterexample.getVariable(i);
			DefaultMutableTreeNode varNode = new DefaultMutableTreeNode(variable);
			treeModel.insertNodeInto(varNode, parentCounterexampleNode, parentCounterexampleNode.getChildCount());
		}		
	}
	
	/**
	 * When a node is selected, we highlight it
	 * depending on what type of node it is.
	 */
	private void nodeSelected(Object obj) {
		if (obj instanceof Step) {
			Step step = (Step)obj;
			piCode.highlight(step.getLocation(), PiCode.yellowHP);
		} else if (obj instanceof BasicPath) {
			BasicPath basicPath = (BasicPath)obj;
			piCode.highlight(basicPath.getLocations(), PiCode.yellowHP);
		} else if (obj instanceof Function) {
			Function function = (Function)obj;
			piCode.highlight(function.getLocation(), PiCode.yellowHP);
		} else
			piCode.removeAllHighlights();
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
		
		private ImageIcon valid, invalid;
		
		public MyTreeCellRenderer() {
			valid = new ImageIcon("images/valid.jpg");
			invalid = new ImageIcon("images/invalid.jpg");
		}
		
		private ImageIcon getProperIcon(boolean isValid) {
			if (isValid)
				return valid;
			else
				return invalid;
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
				setIcon(getProperIcon(verificationResult.isValid()));
				String name = verificationResult.getFilename() != null ? verificationResult.getFilename() : "Program";
				setText(name);
			} else if (obj instanceof Function) {
				Function function = (Function)obj;
				setIcon(getProperIcon(function.isValid()));
				setText(function.getName());
			} else if (obj instanceof BasicPath) {
				BasicPath basicPath = (BasicPath)obj;
				setIcon(getProperIcon(basicPath.isValid()));
				setText("Basic path identifier goes here");
			} else if (obj instanceof Step) {
				Step step = (Step)obj;
				setIcon(null);
				setText(step.getText());
			} else if (obj instanceof VerificationCondition) {
				VerificationCondition vc = (VerificationCondition)obj;
				setIcon(getProperIcon(vc.isValid()));
				setText("VC: " + vc.getVerificationCondition());
			} else if (obj instanceof String) {
				setIcon(null);
				setText((String)obj);
			}
			return this;
		}
	}

}
