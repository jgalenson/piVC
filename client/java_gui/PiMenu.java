import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public class PiMenu extends JMenuBar implements DirtyChangedListener {
	
	private PiGui piGui;
	private JMenuItem save;
	
	public PiMenu(PiGui piGui) {
		super();
		this.piGui = piGui;
		piGui.addDirtyChangedListener(this);
		addFileMenu();
		addCompileMenu();
	}
	
	/**
	 * Adds the File menu.
	 */
	private void addFileMenu() {
		JMenu file = new JMenu("File");
		file.setMnemonic(KeyEvent.VK_F);
		
		JMenuItem open = new JMenuItem("Open");
		open.setMnemonic(KeyEvent.VK_O);
		open.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
		open.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.open();
			}
		});
		file.add(open);
		
		save = new JMenuItem("Save");
		save.setMnemonic(KeyEvent.VK_S);
		save.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
		save.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.save();
			}
		});
		file.add(save);		
		
		JMenuItem saveAs = new JMenuItem("Save As...");
		saveAs.setMnemonic(KeyEvent.VK_A);
		saveAs.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.saveAs();
			}
		});
		file.add(saveAs);		

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
	
	private void addCompileMenu() {
		JMenu compileMenu = new JMenu("Compile");
		compileMenu.setMnemonic(KeyEvent.VK_F);
		
		JMenuItem compile = new JMenuItem("Compile");
		compile.setMnemonic(KeyEvent.VK_C);
		compile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		compile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doCompile();
			}
		});
		compileMenu.add(compile);
		
		add(compileMenu);
	}

	/**
	 * When the dirty bit changes, enable or disable
	 * the Save menu item.
	 */
	public void dirtyChanged(boolean dirty) {
		save.setEnabled(dirty);
	}
	
}
