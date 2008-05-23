import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.KeyStroke;
import javax.swing.undo.UndoManager;

public class PiMenu extends JMenuBar implements DirtyChangedListener {
	
	private PiGui piGui;
	private JMenuItem save;
	private JMenuItem undo, redo;
	private JMenuItem displayPath;
	private JMenuItem compileMenuItem, cancelCompileMenuItem;
	private JCheckBoxMenuItem runtimeAssertions;
	private JCheckBoxMenuItem findInductiveCore;
	
	public PiMenu(PiGui piGui) {
		super();
		this.piGui = piGui;
		piGui.addDirtyChangedListener(this);
		addFileMenu();
		addEditMenu();
		addCompileMenu();
		addAnalyzeMenu();
		addSettingsMenu();
	}
	
	/**
	 * Adds the File menu.
	 */
	private void addFileMenu() {
		JMenu file = new JMenu("File");
		file.setMnemonic(KeyEvent.VK_F);
		
		JMenuItem newMenuItem = new JMenuItem("New");
		newMenuItem.setMnemonic(KeyEvent.VK_N);
		newMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
		newMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.newFile();
			}
		});
		file.add(newMenuItem);
		
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
		
		compileMenuItem = new JMenuItem("Compile");
		compileMenuItem.setMnemonic(KeyEvent.VK_C);
		compileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
		compileMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doCompile();
			}
		});
		compileMenu.add(compileMenuItem);
		
		cancelCompileMenuItem = new JMenuItem("Cancel compile");
		cancelCompileMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.cancelCompile();
			}
		});
		compileMenu.add(cancelCompileMenuItem);
		
		compileMenu.addSeparator();
		
		runtimeAssertions = new JCheckBoxMenuItem("Generate runtime assertions");
		runtimeAssertions.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Config.setBooleanValue("generate_runtime_assertions", runtimeAssertions.getState());
			}
		});
		runtimeAssertions.setState(Config.getBooleanValue("generate_runtime_assertions"));
		compileMenu.add(runtimeAssertions);

		findInductiveCore = new JCheckBoxMenuItem("Find inductive core");
		findInductiveCore.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Config.setBooleanValue("find_inductive_core", findInductiveCore.getState());
			}
		});
		findInductiveCore.setState(Config.getBooleanValue("find_inductive_core"));
		compileMenu.add(findInductiveCore);		
				
		add(compileMenu);
	}
	
	private void addAnalyzeMenu() {
		JMenu analyzeMenu = new JMenu("Analyze");
		analyzeMenu.setMnemonic(KeyEvent.VK_A);
		
		displayPath = new JMenuItem("Display selected basic path");
		displayPath.setMnemonic(KeyEvent.VK_D);
		displayPath.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_D, ActionEvent.CTRL_MASK));
		displayPath.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.displaySelectedBasicPath();
			}
		});
		displayPath.setEnabled(false);
		analyzeMenu.add(displayPath);
		
		add(analyzeMenu);
	}
	
	private void addSettingsMenu() {
		JMenu settingsMenu = new JMenu("Settings");
		settingsMenu.setMnemonic(KeyEvent.VK_S);
		
		JMenuItem serverAddress = new JMenuItem("Change server address");
		serverAddress.setMnemonic(KeyEvent.VK_S);
		serverAddress.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String result = JOptionPane.showInputDialog("New server host:port", Config.getValue("server_address"));
				if(result!=null){
					Config.setValue("server_address", result);
				}
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
	 * Enables or disables the button that highlights
	 * a basic path.
	 */
	public void enableBasicPathHighlighter(boolean b) {
		displayPath.setEnabled(b);
	}

	/**
	 * When the dirty bit changes, enable or disable
	 * the Save menu item.
	 */
	public void dirtyChanged(boolean dirty) {
		save.setEnabled(dirty);
	}
	
	/**
	 * Enables or disables the compile menu item.
	 */
	public void isCompiling(boolean isCompiling) {
		compileMenuItem.setEnabled(!isCompiling);
		cancelCompileMenuItem.setEnabled(isCompiling);
	}
	
}
