import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;

public class PiGui extends JFrame {
	
	private static final int DEFAULT_WIDTH = 500;
	private static final int DEFAULT_HEIGHT = 500;
	
	private PiCode piCode;
	private JFileChooser fileChooser;
	private File curFile;
	private boolean dirty;
	private ArrayList<DirtyChangedListener> dirtyChangedListeners;

	public PiGui() {
		super("PiVC");
		
		initData();
		installMain();
		installMenu();
		setupWindow();
	}

	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new PiGui();
			}
		});
	}
	
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

	public void save() {
		if (curFile == null)
			saveAs();
		else
			saveFile(curFile);
	}

	public void saveAs () {
		if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION)
			saveFile(fileChooser.getSelectedFile());
	}

	public void doExit() {
		if (dirty) {
			boolean ok = askToSave();
			if (!ok)
				return;
		}
		System.exit(0);
	}

	public void loadFile(File selectedFile) {
        try {
            BufferedReader in = new BufferedReader(new FileReader(selectedFile));
            piCode.read(in, null);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
		
	}
	
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

	private boolean askToSave() {
		int result = JOptionPane.showConfirmDialog(this, "Save changes first?", "Save?", JOptionPane.YES_NO_CANCEL_OPTION);
		if (result == JOptionPane.YES_OPTION)
			save();
		return (result != JOptionPane.CANCEL_OPTION);
	}

	public void setDirty(boolean b) {
		dirty = b;
		fireDirtyChanged();
	}

	private void initData() {
		initFileChooser();
		curFile = null;
		dirty = false;
		dirtyChangedListeners = new ArrayList<DirtyChangedListener>();
	}
	
	private void initFileChooser() {
		try {
			fileChooser = new JFileChooser(new File(".").getCanonicalPath());
		} catch (IOException ex) {
			ex.printStackTrace();
		}
		fileChooser.addChoosableFileFilter(new PiFileFilter());
	}
	
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
		
		PiTree piTree = new PiTree();
		piTree.setPreferredSize(new Dimension(DEFAULT_WIDTH / 2, DEFAULT_HEIGHT));
		piTree.setLayout(new GridLayout(1, 1));
		piTree.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Tree"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
		
		JSplitPane sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, codePanel, piTree);
		sp.setOneTouchExpandable(true);
		//sp.setDividerLocation(.5);
		
		add(sp);
	}
	
	private void installMenu() {
		PiMenu menu = new PiMenu(this);
		setJMenuBar(menu);
	}
	
	/**
	 * Sets up the window.  Sets it to use the system look
	 * and feel, sets the close operation, sets is to
	 * maximized, and makes it visible.
	 */
	private void setupWindow() {
		useSystemLookAndFeel();
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
	
	public void addDirtyChangedListener(DirtyChangedListener listener) {
		dirtyChangedListeners.add(listener);
	}
	
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

}