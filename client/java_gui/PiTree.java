import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.ImageIcon;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import data_structures.BasicPath;
import data_structures.Function;
import data_structures.Step;
import data_structures.VerificationCondition;
import data_structures.VerificationResult;

public class PiTree extends JPanel {
	
	private DefaultTreeModel treeModel;
	private JTree tree;
	private DefaultMutableTreeNode root;
	private PiCode piCode;
	
	public PiTree(PiCode piCode) {
		super();
	    root = null;
	    treeModel = new DefaultTreeModel(root);
	    tree = new JTree(treeModel);
	    this.piCode = piCode;
		initTree();
	}
	
	private void initTree() {
		// Listen to clicks
		tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.getButton() != MouseEvent.BUTTON1)
					return;
				int selRow = tree.getRowForLocation(e.getX(), e.getY());
				TreePath selPath = tree.getPathForLocation(e.getX(), e.getY());
				if(selRow != -1) {
					if(e.getClickCount() == 1)
						singleClicked(selPath);
					else if(e.getClickCount() == 2)
						;// do something?
				}
			}
		});
		// Draw icons next to things
		tree.setCellRenderer(new MyTreeCellRenderer());
		tree.setShowsRootHandles(true);
		tree.setRootVisible(true);
		tree.setEditable(false);
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
		}		
	}

	/**
	 * Adds the steps inside a basic path to the tree as its children.
	 */
	private void addSteps(BasicPath basicPath, DefaultMutableTreeNode basicPathNode) {
		for (int i = 0; i < basicPath.getNumSteps(); i++) {
			Step step = basicPath.getStep(i);
			DefaultMutableTreeNode stepNode = new DefaultMutableTreeNode(step);
			treeModel.insertNodeInto(stepNode, basicPathNode, basicPathNode.getChildCount());
		}		
	}

	/**
	 * Called when a node is clicked.
	 */
	private void singleClicked(TreePath path) {
		DefaultMutableTreeNode node = (DefaultMutableTreeNode)path.getLastPathComponent();
		Object obj = node.getUserObject();
		if (obj instanceof Step) {
			Step step = (Step)obj;
			piCode.highlight(step.getLocation());
		} else if (obj instanceof BasicPath) {
			BasicPath basicPath = (BasicPath)obj;
			piCode.highlight(basicPath.getLocations());
		} else if (obj instanceof Function) {
			Function function = (Function)obj;
			// TODO: Also highlight functions
		} else
			piCode.removeAllHighlights();
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
			}
			return this;
		}
	}

}
