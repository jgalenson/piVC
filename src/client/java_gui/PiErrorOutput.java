import java.awt.Component;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import data_structures.Location;
import data_structures.PiError;

public class PiErrorOutput extends JPanel {
	
	private JList list;
	private DefaultListModel model;
	private PiCode piCode;
	
	public PiErrorOutput(PiCode pC) {
		super();
		this.piCode = pC;
		model = new DefaultListModel();
		list = new JList(model);
		initList();
	}
	
	private void initList() {
		list.setCellRenderer(new MyListCellRenderer());
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		// Catch selection clicks and clicks on selected object
		list.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				if (e.getButton() != MouseEvent.BUTTON1)
					return;
				int index = list.locationToIndex(e.getPoint());
				Object obj = list.getModel().getElementAt(index);
				errorClicked(obj);
			}
		});
		// Catch unselection
		list.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {  // multiple events get thrown: we only care about one of them.
					Object obj = list.getSelectedValue();
					if (obj == null)
						errorClicked(obj);
				}
			}
		});
	}
	
	/**
	 * Handle selecting/unselecting an error.
	 */
	private void errorClicked(Object obj) {
		if (obj == null)  // unselecting
			piCode.removeAllHighlights();
		else {
			Location loc = ((PiError)obj).getLocation();
			if (loc != null) {  // Compiler errors don't have locations.
				piCode.highlight(loc, PiCode.redHP);
				piCode.setCaretPosition(loc.getStartByte());
				piCode.requestFocusInWindow();
			}
		}	
	}
	
	/**
	 * Add the return of this into the tabbed pane:
	 * do not add the PiErrorOutput object itself.
	 */
	public JScrollPane getErrorOutputInScrollPane() {
		return new JScrollPane(list);
	}
	
	/**
	 * Sets the error output to display the selected errors.
	 */
	public void setErrors(ArrayList<PiError> errors) {
		clear();
		for (PiError error: errors)
			model.addElement(error);
	}
	
	public void setCompilerError(PiError compilerError) {
		clear();
		model.addElement(compilerError);
	}
	
	/**
	 * Clears everything from this display.
	 */
	public void clear() {
		model.clear();
	}
	
	/**
	 * A class that lets us customize how we draw errors in the list.
	 * We use HTML so we can get multi-line displays.
	 */
	private static class MyListCellRenderer extends DefaultListCellRenderer {

		@Override
	     public Component getListCellRendererComponent(JList list, Object value,
	    		 int index, boolean isSelected, boolean cellHasFocus) {
	    	 super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
	         setText("<html>"+((PiError)value).getMessage() + "</html>");
	         return this;
	     }
	 }

}
