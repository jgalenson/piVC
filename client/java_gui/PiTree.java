import java.awt.Dimension;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import data_structures.Function;
import data_structures.VerificationResult;

public class PiTree extends JPanel {
	
	private DefaultTreeModel treeModel;
	private JTree tree;
	DefaultMutableTreeNode root;
	
	public PiTree() {
		super();
	    root = new DefaultMutableTreeNode("Root");
	    treeModel = new DefaultTreeModel(root);
	    tree = new JTree(treeModel);
		initTree();
	}
	
	private void initTree() {
		// TODO: Handle clicks here
		tree.setPreferredSize(getPreferredSize());
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
		for (int i = 0; i < verificationResult.getNumFunctions(); i++) {
			Function function = verificationResult.getFunction(i);
			System.out.println("Adding node " + function.getName());
			DefaultMutableTreeNode fnNode = new DefaultMutableTreeNode(function.getName());
			treeModel.insertNodeInto(fnNode, root, root.getChildCount());
		}
	}

}
