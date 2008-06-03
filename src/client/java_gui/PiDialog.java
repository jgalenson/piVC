import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.JButton;
import javax.swing.JDialog;


public class PiDialog extends JDialog {
	
	protected PiGui parent;
	
	public PiDialog(PiGui parent, String title){
		super(parent, title, true);		
		this.parent = parent;
		setLocationRelativeTo(parent);
		addWindowListener(new WindowListener() {
			public void windowActivated(WindowEvent arg0) {
			}
			public void windowClosed(WindowEvent arg0) {
			}
			public void windowClosing(WindowEvent arg0) {
				close();
			}
			public void windowDeactivated(WindowEvent arg0) {
			}
			public void windowDeiconified(WindowEvent arg0) {
			}
			public void windowIconified(WindowEvent arg0) {
			}
			public void windowOpened(WindowEvent arg0) {	
			}
		});			
		setLocation((parent.getLocation().x+parent.getSize().width)/2-getSize().width/2,(parent.getLocation().y+parent.getSize().height)/2 - getSize().height/2);
		setResizable(false);
	}
	
	protected void launch(){
		pack();
		int xLocation = parent.getLocation().x+parent.getSize().width/2-getSize().width/2;
		int yLocation = parent.getLocation().y+parent.getSize().height/2-getSize().height/2;
		setLocation(xLocation,yLocation);
		setResizable(false);
		setVisible(true);		
	}
	
	protected void close(){
		setVisible(false);
	}
	
	protected JButton getCancelButton(){
        JButton cancel = new JButton("Cancel");
        cancel.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				close();		
			}
        });
        return cancel;
	}
	
}
