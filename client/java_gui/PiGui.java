import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.*;

public class PiGui extends JFrame {
	
	private static final int DEFAULT_WIDTH = 500;
	private static final int DEFAULT_HEIGHT = 500;
	
	private PiCode piCode;

	public PiGui() {
		super("PiVC");
		
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

	public void save() {
		// TODO Auto-generated method stub
		
	}

	public void doExit() {
		// TODO Auto-generated method stub
		System.exit(0);
	}

	public void load(File selectedFile) {
        try {
            BufferedReader in = new BufferedReader(new FileReader(selectedFile));
            piCode.read(in, null);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
		
	}
	
	private void installMain() {
		JPanel codePanel = new JPanel();
		piCode = new PiCode();
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

}