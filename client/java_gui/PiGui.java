import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.net.Socket;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.undo.UndoManager;

import data_structures.BasicPath;
import data_structures.PiError;
import data_structures.Step;
import data_structures.VerificationResult;

public class PiGui extends JFrame {
	
	private static final int DEFAULT_WIDTH = 800;
	private static final int DEFAULT_HEIGHT = 800;
	
	private PiCode piCode;
	private PiErrorOutput piErrorOutput;
	private PiCompilerOutput piCompilerOutput;
	private PiTree piTree;
	private PiMenu piMenu;
	private JTabbedPane rightTabbedPane;
	private Config config;
	private ServerResponseParser serverResponseParser;
	private JFileChooser fileChooser;
	private File curFile;
	private boolean dirty;
	private ArrayList<DirtyChangedListener> dirtyChangedListeners;

	public PiGui() {
		super("PiVC");
		setLayout(new BorderLayout());
		useSystemLookAndFeel();
		
		initDataPre();
		installMain();
		installMenu();
		installTop();
		initDataPost();
		setupWindow();
	}

	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new PiGui();
			}
		});
	}
	
	/**
	 * Opens a file.  We first prompt the user to save
	 * the current file if it is dirty and then let
	 * them choose a file to open.
	 */
	public void open() {
		if (dirty) {
			boolean ok = askToSave();
			if (!ok)
				return;
		}
		if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
			loadFile(fileChooser.getSelectedFile());
            curFile = fileChooser.getSelectedFile();
            setDirty(false);
		}
	}

	/**
	 * Save the current file.  We save directly if a file
	 * is opened and otherwise call Save As.
	 */
	public void save() {
		if (curFile == null)
			saveAs();
		else
			saveFile(curFile);
	}

	/**
	 * Saves a file after prompting the user for the name.
	 */
	public void saveAs () {
		if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION)
			saveFile(fileChooser.getSelectedFile());
	}

	/**
	 * Cleanup and exit the program.  We prompt the user
	 * to save first if it is dirty.
	 */
	public void doExit() {
		if (dirty) {
			boolean ok = askToSave();
			if (!ok)
				return;
		}
		System.exit(0);
	}

	/**
	 * Loads the given file into the code panel.
	 */
	public void loadFile(File selectedFile) {
        try {
            BufferedReader in = new BufferedReader(new FileReader(selectedFile));
            piCode.read(in, null);
            piCode.openedNewFile();
        }
        catch (IOException e) {
            e.printStackTrace();
        }
	}
	
	/**
	 * Saves what's in the code panel into the
	 * given file.
	 */
	private void saveFile(File selectedFile) {
        try {
            FileWriter out = new FileWriter(selectedFile);
            out.write(piCode.getText());
            out.close();
        }
        catch (IOException e) {
        	e.printStackTrace();
        	return;
        }
		curFile = selectedFile;
		setDirty(false);
	}

	/**
	 * Asks if the user wants to save the file and saves
	 * if they do.
	 * @return <tt>true</tt> if the user did not hit Cancel
	 * and so wants to continue.
	 */
	private boolean askToSave() {
		int result = JOptionPane.showConfirmDialog(this, "Save changes first?", "Save?", JOptionPane.YES_NO_CANCEL_OPTION);
		if (result == JOptionPane.YES_OPTION)
			save();
		return (result != JOptionPane.CANCEL_OPTION);
	}

	/**
	 * Sets the dirty bit to the specified value and
	 * notifies anyone who cares about the dirty bit
	 * changing.
	 */
	public void setDirty(boolean b) {
		dirty = b;
		fireDirtyChanged();
	}
	
	/**
	 * Handles a call to compile the code by sending it
	 * off to the server and handling the response.
	 */
	public void doCompile() {
		String result = config.getValue("default_server_address");
		if (result != null) {
			piCode.removeAllHighlights();
			String[] parts = result.split(":");
			String name = parts[0].trim();
			int port = Integer.parseInt(parts[1].trim());
			try {
				Socket toServer = new Socket(name, port);
				DataOutputStream out = new DataOutputStream(toServer.getOutputStream());
				String code = piCode.getText();
				out.writeInt(code.length());
				out.writeBytes(code);
				out.flush();
				DataInputStream in = new DataInputStream(toServer.getInputStream());
				int len = in.readInt();
				byte[] bytes = new byte[len];
				in.readFully(bytes, 0, len);
				String text = new String(bytes);
				handleServerResponse(text);
			} catch (java.net.ConnectException ex){
				JOptionPane.showMessageDialog(null, ex.getMessage() + "\n\nEnsure that a server is running and that the server address in the Settings menu is set to the proper address.", "Connection Error.", JOptionPane.ERROR_MESSAGE);
			} catch (Exception ex) {
				ex.printStackTrace();
			}
		}
	}

	/**
	 * Handles a response from the server by parsing it.
	 */
	private void handleServerResponse(String text) {
		piTree.clear();
		piErrorOutput.clear();
		piCompilerOutput.setText(text);
		serverResponseParser.parse(text, getFilename());
		rightTabbedPane.repaint();
	}
	
	/**
	 * Handles a response from the server that contains
	 * verification conditions and basic paths.
	 */
	public void handleVerificationResult(VerificationResult verificationResult) {
		piTree.handleVerificationResult(verificationResult);
		rightTabbedPane.setSelectedIndex(0);
	}
	
	/**
	 * Handles a response from the server that contains
	 * a list of errors.
	 */
	public void handleError(ArrayList<PiError> errors) {
		piErrorOutput.setErrors(errors);
		rightTabbedPane.setSelectedIndex(1);
	}
	
	/**
	 * Gets the name of the currently-opened file.
	 */
	private String getFilename() {
		if (curFile == null)
			return null;
		else
			return curFile.getName();
	}
	
	/**
	 * Called when some kind of change relating to undo/redo happened.
	 * We let the menus update themselves as they please.
	 */
	public void undoChangeHappened(UndoManager undoManager) {
		piMenu.undoChangeHappened(undoManager);
	}
	
	/**
	 * Undo the last change made and set the dirty
	 * bit to false if there are no more things
	 * that can be undone (i.e. we have undone everything
	 * we did).
	 */
	public void undo() {
		boolean canUndoMore = piCode.undo();
		if (!canUndoMore)
			setDirty(false);
	}
	
	/**
	 * Redo the last change made.
	 */
	public void redo() {
		piCode.redo();
	}
	
	/**
	 * Called when a new node in the tree is selected.
	 * We make sure to enable/disable any menu items
	 * that depend on having a basic path selected.
	 */
	public void nodeSelected(Object obj) {
		piMenu.enableBasicPathHighlighter(obj instanceof BasicPath);
	}
	
	/**
	 * Display the selected basic path over time.
	 */
	public void displaySelectedBasicPath() {
		BasicPath basicPath = (BasicPath)piTree.getSelectedObject();
		BasicPathHighlighter basicPathHighlighter = new BasicPathHighlighter(basicPath);
		basicPathHighlighter.start();
	}

	/**
	 * Inits some data before we install the GUI elements. 
	 */
	private void initDataPre() {
		config = new Config();
		serverResponseParser = new ServerResponseParser(this);
		initFileChooser();
		curFile = null;
		dirtyChangedListeners = new ArrayList<DirtyChangedListener>();
	}
	
	/**
	 * Inits some data after we install the GUI elements. 
	 */
	private void initDataPost() {
		setDirty(false);
	}
	
	/**
	 * Create the file chooser we'll use.
	 */
	private void initFileChooser() {
		try {
			fileChooser = new JFileChooser(new File(".").getCanonicalPath());
		} catch (IOException ex) {
			ex.printStackTrace();
		}
		fileChooser.addChoosableFileFilter(new PiFileFilter());
	}
	
	/**
	 * Creates the main part of the window.
	 */
	private void installMain() {
		JPanel codePanel = new JPanel();
		piCode = new PiCode(this);
		JScrollPane codeScrollPane = new JScrollPane(piCode);
		codeScrollPane.setPreferredSize(new Dimension(DEFAULT_WIDTH / 2, DEFAULT_HEIGHT));
		codePanel.setLayout(new GridLayout(1, 1));
		codePanel.add(codeScrollPane);
        codePanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Code"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
		
        rightTabbedPane = new JTabbedPane();
        rightTabbedPane.setPreferredSize(new Dimension(DEFAULT_WIDTH / 2, DEFAULT_HEIGHT));
		piTree = new PiTree(this, piCode);
		piTree.setPreferredSize(new Dimension(DEFAULT_WIDTH / 2, DEFAULT_HEIGHT));
		//piTree.setLayout(new GridLayout(1, 1));
		piTree.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Tree"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
		piErrorOutput = new PiErrorOutput(piCode);
		piErrorOutput.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Errors"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
		piCompilerOutput = new PiCompilerOutput();
		piCompilerOutput.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Compiler output"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
		rightTabbedPane.addTab("Tree", piTree.getTreeInScrollPane());
		rightTabbedPane.setMnemonicAt(0, KeyEvent.VK_D);
		rightTabbedPane.addTab("Errors", piErrorOutput.getErrorOutputInScrollPane());
		rightTabbedPane.setMnemonicAt(1, KeyEvent.VK_E);
		rightTabbedPane.addTab("Compiler output", new JScrollPane(piCompilerOutput));
		rightTabbedPane.setMnemonicAt(2, KeyEvent.VK_R);
		
		JSplitPane sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, codePanel, rightTabbedPane);
		sp.setOneTouchExpandable(true);
		sp.setContinuousLayout(true);
		//sp.setDividerLocation(.5);
		
		add(sp);
	}
	
	/**
	 * Creates the menu.
	 */
	private void installMenu() {
		piMenu = new PiMenu(this, config);
		setJMenuBar(piMenu);
	}
	
	private void installTop() {
		Box box = Box.createHorizontalBox();
		
		JButton compile = new JButton("Compile");
		compile.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				doCompile();
			}
		});
		box.add(compile);
		
		add(box, BorderLayout.NORTH);
	}
	
	/**
	 * Sets up the window.  Sets it to use the system look
	 * and feel, sets the close operation, sets is to
	 * maximized, and makes it visible.
	 */
	private void setupWindow() {
		setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent event) {
				doExit();
			}
		});
		pack();
		setVisible(true);
	}
	
	/**
	 * Uses the system look and feel rather than Java's.
	 */
	private void useSystemLookAndFeel() {
		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception ignored) { }
	}
	
	/**
	 * Adds a listener to the list of people who want to
	 * know when the dirty bit changes.
	 */
	public void addDirtyChangedListener(DirtyChangedListener listener) {
		dirtyChangedListeners.add(listener);
	}
	
	/**
	 * Notify listeners when the dirty bit changes.
	 */
	private void fireDirtyChanged() {
		for (DirtyChangedListener listener: dirtyChangedListeners)
			listener.dirtyChanged(dirty);
	}
	
	/**
	 * A filter for a JFileChooser that selects .pi files.
	 */
	private static class PiFileFilter extends FileFilter {

		@Override
		public boolean accept(File file) {
            if (file.isDirectory()) return true;
            String name = file.getName();
            if (name.length() < 3) return true;
            String extension = name.substring(name.length() - 2).toLowerCase();
            return (extension != null && extension.equals("pi"));

		}

		@Override
		public String getDescription() {
			return "Pi programs";
		}
		
	}
	
	/**
	 * A thread that highlights a basic path one step at a
	 * time, in order, pausing slightly at each step so the user
	 * can tell the direction.  We always take the same total time
	 * to highlight the path, regardless of how many steps there are.
	 */
	private class BasicPathHighlighter extends Thread {
		
		// In milliseconds.
		private static final int TOTAL_BASIC_PATH_HIGHLIGHT_TIME = 500;
		
		private BasicPath basicPath;
		
		public BasicPathHighlighter(BasicPath basicPath) {
			this.basicPath = basicPath;
		}
		
		@Override
		public void run() {
			// First, remove all highlights
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					piCode.removeAllHighlights();
				}
			});
			// Now highlight each step one at a time.
			long pauseTime = TOTAL_BASIC_PATH_HIGHLIGHT_TIME / ((long)basicPath.getNumSteps());
			for (int i = 0; i < basicPath.getNumSteps(); i++) {
				final Step step = basicPath.getStep(i);
				// Highlight the step
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						piCode.highlight(step.getLocation());
					}
				});
				// Sleep for a bit
				try {
					Thread.sleep(pauseTime);
				} catch (InterruptedException e) {}
				// Unhighlight it
				SwingUtilities.invokeLater(new Runnable() {
					public void run() {
						piCode.removeAllHighlights();
					}
				});
			}
			/* At end, highlight whatever is currently selected.
			 * We do this in case the user selected something different
			 * as we were highlighting this path.
			 */
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					piTree.reselectSelectedNode();
				}
			});
		}
		
	}

}