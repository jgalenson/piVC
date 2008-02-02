import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.Action;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.undo.UndoManager;

public class PiMenu extends JMenuBar implements DirtyChangedListener {
	
	private PiGui piGui;
	private Config config;
	private JMenuItem save;
	private JMenuItem undo, redo;
	
	public PiMenu(PiGui piGui, Config config) {
		super();
		this.piGui = piGui;
		this.config = config;
		piGui.addDirtyChangedListener(this);
		addFileMenu();
		addEditMenu();
		addCompileMenu();
		addSettingsMenu();
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
	
	private void addEditMenu() {
		JMenu editMenu = new JMenu("Edit");
		editMenu.setMnemonic(KeyEvent.VK_F);
		
		undo = new JMenuItem("Undo");
		undo.setMnemonic(KeyEvent.VK_U);
		undo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, ActionEvent.CTRL_MASK));
		undo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.undo();
			}
		});
		undo.setEnabled(false);
		editMenu.add(undo);
		

		
		redo = new JMenuItem("Redo");
		redo.setMnemonic(KeyEvent.VK_R);
		redo.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y, ActionEvent.CTRL_MASK));
		redo.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.redo();
			}
		});
		redo.setEnabled(false);
		editMenu.add(redo);
		
		add(editMenu);
	}
	
	private void addCompileMenu() {
		JMenu compileMenu = new JMenu("Compile");
		compileMenu.setMnemonic(KeyEvent.VK_O);
		
		JMenuItem compile = new JMenuItem("Compile");
		compile.setMnemonic(KeyEvent.VK_C);
		compile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.ALT_MASK));
		compile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doCompile();
			}
		});
		compileMenu.add(compile);
		
		add(compileMenu);
	}
	
	private void addSettingsMenu() {
		JMenu settingsMenu = new JMenu("Settings");
		settingsMenu.setMnemonic(KeyEvent.VK_S);
		
		JMenuItem serverAddress = new JMenuItem("Change server address");
		serverAddress.setMnemonic(KeyEvent.VK_S);
		serverAddress.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String result = JOptionPane.showInputDialog("New server host:port", config.getValue("default_server_address"));
				config.setValue("default_server_address", result);
				config.writeOutConfig();
			}
		});
		settingsMenu.add(serverAddress);
		
		add(settingsMenu);
	}
	
	/**
	 * Called when some kind of change relating to undo/redo happened.
	 * We enable/disable the undo/redo menus and change the name as
	 * appropriate,
	 */
	public void undoChangeHappened(UndoManager undoManager) {
		undo.setEnabled(undoManager.canUndo());
        if (undoManager.canUndo())
            undo.setText(undoManager.getUndoPresentationName());
        else
            undo.setText("Undo");
		redo.setEnabled(undoManager.canRedo());
        if (undoManager.canRedo())
        	redo.setText(undoManager.getRedoPresentationName());
        else
        	redo.setText("Redo");
	}

	/**
	 * When the dirty bit changes, enable or disable
	 * the Save menu item.
	 */
	public void dirtyChanged(boolean dirty) {
		save.setEnabled(dirty);
	}
	
}
