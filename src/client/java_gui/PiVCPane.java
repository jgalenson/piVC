import java.awt.Component;

import javax.swing.*;
import javax.swing.event.*;

import data_structures.Conjunct;
import data_structures.Location;
import data_structures.VerificationCondition;
import data_structures.VerificationResult.validityT;

public class PiVCPane extends JPanel {
	
	private JList list;
	private DefaultListModel model;
	private PiCode piCode;
	
	public PiVCPane(PiCode piCode) {
		super();
		this.piCode = piCode;
		model = new DefaultListModel();
		list = new JList(model);
		initList();
	}
	
	public void setNothing(){
		clear();
		model.addElement("<html><i>Use the above pane to select a VC</i></html>");
	}
	
	private void initList() {
		list.setCellRenderer(new MyListCellRenderer());
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.addListSelectionListener(new ListSelectionListener() {
			public void valueChanged(ListSelectionEvent e) {
				if (!e.getValueIsAdjusting()) {  // multiple events get thrown: we only care about one of them.
					Object obj = list.getSelectedValue();
					if (obj != null)
						conjunctClicked(obj);
				}
			}
		});
	}
	

	private void conjunctClicked(Object obj) {
		if (obj == null || !(obj instanceof Conjunct))  // unselecting
			piCode.removeAllHighlights();
		else {
			Location loc = ((Conjunct)obj).getLocation();
			if (loc != null) {  // Compiler errors don't have locations.
				piCode.highlight(loc, PiCode.yellowHP);
			}
		}	
	}
	
	/**
	 * Add the return of this into the tabbed pane:
	 * do not add the PiErrorOutput object itself.
	 */
	public JScrollPane getPiVCPaneInScrollPane() {
		return new JScrollPane(list);
	}
	
	/**
	 * Sets the error output to display the selected errors.
	 */

	public void setVC(VerificationCondition vc){		
		clear();
		Conjunct[][] conjs = vc.getConjuncts();
		for(int implies = 0; implies<conjs.length; ++implies){
			for(int conj = 0; conj<conjs[implies].length; ++conj){
				Conjunct curr = conjs[implies][conj];
				String str = sanitizeHTML(curr.str);
				String color = "black";
				if(curr.status!=null){
					if(curr.status.equals(validityT.VALID)){
						color = "green";
					}
					else if(curr.status.equals(validityT.INVALID)){
						color = "red";
					}
					else{
						color = "yellow";
					}
				}
				str = "<font color='"+color+"'>" + str + "</font>";
				if(curr.inInductiveCore!=null && !curr.inInductiveCore.booleanValue()){
					str = "<i>" + str + "</i>";
				}
				if(curr.inInductiveCore!=null && curr.inInductiveCore.booleanValue()){
					str = "<b>" + str + "</b>";
				}				
				if(conj!=conjs[implies].length-1){
					str = str + " &&";
				}				
				str = "&nbsp;&nbsp;&nbsp;" + str;
				curr.setDisplayHTML(str);
				model.addElement(curr);
			}
			if(implies!=conjs.length-1){
				model.addElement("-&gt;");
			}
		}
	}	
	
	/**
	 * Clears everything from this display.
	 */
	public void clear() {
		model.clear();
	}
	
	public static String sanitizeHTML(String html){
		html = html.replaceAll("<", "&lt;");
		html = html.replaceAll(">", "&gt;");		
		return html;
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
	    	 String str = "";
	    	 if(value instanceof String){
	    		 str = (String)value;
	    	 }
	    	 else if(value instanceof Conjunct){
	    		 str = ((Conjunct)value).getDisplayHTML();
	    	 }else{
	    		 throw new RuntimeException("unexpected list elem type");
	    	 }
	         setText("<html>"+str+"</html>");
	         return this;
	     }
	 }

}
