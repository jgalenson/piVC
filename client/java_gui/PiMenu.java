import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.io.File;
import java.io.IOException;

import javax.swing.JFileChooser;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;


public class PiMenu extends JMenuBar {
	
	private PiGui piGui;
	
	public PiMenu(PiGui piGui) {
		super();
		this.piGui = piGui;
		
		addFileMenu();
	}
	
	private void addFileMenu() {
		JMenu file = new JMenu("File");
		file.setMnemonic(KeyEvent.VK_F);
		
		JMenuItem open = new JMenuItem("Open");
		open.setMnemonic(KeyEvent.VK_O);
		open.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
		open.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				openFileChooser();
			}
		});
		file.add(open);
		
		JMenuItem save = new JMenuItem("Save");
		save.setMnemonic(KeyEvent.VK_S);
		save.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		save.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.save();
			}
		});
		file.add(save);		

		JMenuItem quit = new JMenuItem("Quit");
		quit.setMnemonic(KeyEvent.VK_Q);
		quit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Q, ActionEvent.CTRL_MASK));
		quit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doExit();
			}
		});
		file.add(quit);
		
		add(file);
	}
	
	private void openFileChooser() {
		JFileChooser chooser;
		try {
			chooser = new JFileChooser(new File(".").getCanonicalPath());
		} catch (IOException ex) {
			ex.printStackTrace();
			return;
		}
		int status = chooser.showOpenDialog(piGui);
		if (status == JFileChooser.APPROVE_OPTION)
			piGui.load(chooser.getSelectedFile());
	}
	
}
