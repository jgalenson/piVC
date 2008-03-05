import java.awt.Component;
import java.util.ArrayList;

import javax.swing.DefaultListCellRenderer;
import javax.swing.DefaultListModel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

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
		// Highlight an error in the code when it is selected.
		list.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {  // multiple events get thrown: we only care about one of them.
					Object obj = list.getSelectedValue();
					if (obj == null)  // unselecting
						piCode.removeAllHighlights();
					else
						piCode.highlight(((PiError)obj).getLocation());
				}
			}
		});	
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
