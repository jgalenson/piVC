import java.awt.Component;
import java.awt.Dimension;

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
	DefaultMutableTreeNode root;
	
	public PiTree() {
		super();
	    root = null;
	    treeModel = new DefaultTreeModel(root);
	    tree = new JTree(treeModel);
		initTree();
	}
	
	private void initTree() {
		// TODO: Handle clicks here
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
	
	public void handleVerificationResult(VerificationResult verificationResult) {
		// TODO: Images (green or red)
		//clear();
		root = new DefaultMutableTreeNode(verificationResult);
		treeModel.setRoot(root);
		addFunctions(verificationResult);
	}
	
    public void clear() {
        root.removeAllChildren();
        treeModel.reload();
    }
	
	private void addFunctions(VerificationResult verificationResult) {
		for (int i = 0; i < verificationResult.getNumFunctions(); i++) {
			Function function = verificationResult.getFunction(i);
			DefaultMutableTreeNode fnNode = new DefaultMutableTreeNode(function);
			treeModel.insertNodeInto(fnNode, root, root.getChildCount());
			addBasicPaths(function, fnNode);
			tree.makeVisible(new TreePath(fnNode.getPath()));
		}
	}

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

	private void addSteps(BasicPath basicPath, DefaultMutableTreeNode basicPathNode) {
		for (int i = 0; i < basicPath.getNumSteps(); i++) {
			Step step = basicPath.getStep(i);
			DefaultMutableTreeNode stepNode = new DefaultMutableTreeNode(step);
			treeModel.insertNodeInto(stepNode, basicPathNode, basicPathNode.getChildCount());
		}		
	}
	
	private class MyTreeCellRenderer extends DefaultTreeCellRenderer {
		
		private ImageIcon validIcon, invalidIcon;
		
		public MyTreeCellRenderer() {
			validIcon = new ImageIcon("client/java_gui/images/valid.jpg");
			invalidIcon = new ImageIcon("client/java_gui/images/invalid.jpg");
		}
		
		private ImageIcon getProperIcon(boolean isValid) {
			if (isValid)
				return validIcon;
			else
				return invalidIcon;
		}
		
		@Override
		@SuppressWarnings("hiding")
		public Component getTreeCellRendererComponent(JTree tree, Object value, 
				boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
			super.getTreeCellRendererComponent(tree, value, sel,expanded, leaf, row, hasFocus);
			Object obj = ((DefaultMutableTreeNode)value).getUserObject();
			if (obj instanceof VerificationResult) {
				VerificationResult verificationResult = (VerificationResult)obj;
				setIcon(getProperIcon(verificationResult.isValid()));
				setText("Program");
			} else if (obj instanceof Function) {
				Function function = (Function)obj;
				//setIcon(getProperIcon(function.isValid()));
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
