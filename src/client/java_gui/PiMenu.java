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
	private JMenuItem cut, copy, paste;
	private JMenuItem displayPath;
	private JMenuItem submit;
	private JMenuItem compileMenuItem, cancelCompileMenuItem;
	private JCheckBoxMenuItem runtimeAssertions;
	private JCheckBoxMenuItem findInductiveCore;
	private JCheckBoxMenuItem showRawXml;
	
	public PiMenu(PiGui piGui) {
		super();
		this.piGui = piGui;
		piGui.addDirtyChangedListener(this);
		addFileMenu();
		addEditMenu();
		addActionsMenu();
		addAnalyzeMenu();
		addSettingsMenu();
		addHelpMenu();
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
		editMenu.setMnemonic(KeyEvent.VK_E);
		
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
		
		editMenu.addSeparator();
		
		cut = new JMenuItem("Cut");
		cut.setMnemonic(KeyEvent.VK_T);
		cut.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_X, ActionEvent.CTRL_MASK));
		cut.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.cut();
			}
		});
		cut.setEnabled(false);
		editMenu.add(cut);
		
		copy = new JMenuItem("Copy");
		copy.setMnemonic(KeyEvent.VK_C);
		copy.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_C, ActionEvent.CTRL_MASK));
		copy.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.copy();
			}
		});
		copy.setEnabled(false);
		editMenu.add(copy);
		
		paste = new JMenuItem("Paste");
		paste.setMnemonic(KeyEvent.VK_P);
		paste.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_V, ActionEvent.CTRL_MASK));
		paste.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.paste();
			}
		});
		editMenu.add(paste);
		
		add(editMenu);
	}
	
	private void addActionsMenu() {
		JMenu actionsMenu = new JMenu("Actions");
		actionsMenu.setMnemonic(KeyEvent.VK_A);
		
		compileMenuItem = new JMenuItem("Compile");
		compileMenuItem.setMnemonic(KeyEvent.VK_C);
		compileMenuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
		compileMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doCompile();
			}
		});
		actionsMenu.add(compileMenuItem);
		
		cancelCompileMenuItem = new JMenuItem("Cancel compile");
		cancelCompileMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.cancelCompile();
			}
		});
		cancelCompileMenuItem.setEnabled(false);
		actionsMenu.add(cancelCompileMenuItem);
		
		actionsMenu.addSeparator();
		
		submit = new JMenuItem("Submit");
		submit.setMnemonic(KeyEvent.VK_S);
		submit.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_T, ActionEvent.CTRL_MASK));
		submit.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doSubmit();
			}
		});
		actionsMenu.add(submit);	
				
		add(actionsMenu);
	}
	
	private void addAnalyzeMenu() {
		JMenu analyzeMenu = new JMenu("Analyze");
		analyzeMenu.setMnemonic(KeyEvent.VK_Z);
		
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
		
		runtimeAssertions = new JCheckBoxMenuItem("Generate runtime assertions");
		runtimeAssertions.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Config.setBooleanValue("generate_runtime_assertions", runtimeAssertions.getState());
			}
		});
		runtimeAssertions.setState(Config.getBooleanValue("generate_runtime_assertions"));
		settingsMenu.add(runtimeAssertions);

		findInductiveCore = new JCheckBoxMenuItem("Find inductive core");
		findInductiveCore.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				Config.setBooleanValue("find_inductive_core", findInductiveCore.getState());
			}
		});
		findInductiveCore.setState(Config.getBooleanValue("find_inductive_core"));
		settingsMenu.add(findInductiveCore);	
		
		settingsMenu.addSeparator();
		
		showRawXml = new JCheckBoxMenuItem("Show raw XML");
		showRawXml.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.showHideRawXml(showRawXml.getState());
			}
		});
		showRawXml.setState(Config.getBooleanValue("show_raw_xml"));
		settingsMenu.add(showRawXml);	
		
		if(!Config.environmentKeyExists("server_address")){
			JMenuItem serverAddress = new JMenuItem("Change server address");
			serverAddress.setMnemonic(KeyEvent.VK_S);
			serverAddress.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					String result = (String)JOptionPane.showInputDialog(piGui,"Enter the address in the form \'host:port\'.", "Change server address", JOptionPane.QUESTION_MESSAGE, null, null, Config.getValue("server_address"));
					if(result!=null){
						Config.setValue("server_address", result);
					}
				}
			});
			settingsMenu.add(serverAddress);
		}
		
		add(settingsMenu);
	}
	
	private void addHelpMenu() {
		JMenu helpMenu = new JMenu("Help");
		helpMenu.setMnemonic(KeyEvent.VK_H);
		
		
		JMenuItem reportBug = new JMenuItem("Report a Bug");
		reportBug.setMnemonic(KeyEvent.VK_R);
		reportBug.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doReport(PiReport.ReportType.bug);
			}
		});
		helpMenu.add(reportBug);

		JMenuItem feedback = new JMenuItem("Give Feedback on PiVC");
		feedback.setMnemonic(KeyEvent.VK_F);
		feedback.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				piGui.doReport(PiReport.ReportType.feedback);
			}
		});
		helpMenu.add(feedback);		
		
		helpMenu.addSeparator();
		
		JMenuItem about = new JMenuItem("About PiVC");
		about.setMnemonic(KeyEvent.VK_A);
		about.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String aboutString = 
					"<html>"
					+ "PiVC" + "<p />"
					+ "<a href='http://theory.stanford.edu/~arbrad/pivc/'>http://theory.stanford.edu/~arbrad/pivc/</a>" + "<p /><p />"
					+ "Jason Auerbach, Aaron Bradley," + "<br />"
					+ "Joel Galenson, Zohar Manna" + "<p /><p />"
					+ "Licensed under the GPL."
					+ "</html>";
				JOptionPane.showMessageDialog(piGui, aboutString, "About PiVC", JOptionPane.INFORMATION_MESSAGE, PiGui.getIcon());
			}
		});
		helpMenu.add(about);
		
		

				
		add(helpMenu);
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
	
	/**
	 * Called when we select or unselect code.
	 * We use this to enable/disable the cut/copy
	 * menu items.
	 * @ isSelected - true if something is selected
	 * (i.e. we can cut/copy); false otherwise.
	 */
	public void codeIsSelected(boolean isSelected) {
		cut.setEnabled(isSelected);
		copy.setEnabled(isSelected);
	}
	
}
