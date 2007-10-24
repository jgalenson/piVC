import java.io.IOException;
import java.io.Reader;
import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

public class PiCode extends JTextPane implements DocumentListener, DirtyChangedListener {
	
	private PiGui piGui;
	
	public PiCode(PiGui piGui) {
		super();
		this.piGui = piGui;
		piGui.addDirtyChangedListener(this);
		addDocumentChangeListener();
	}
	
	// TODO: Bug: Start program, load file, try to quit.  The dirty bit is true.
	@Override
	public void read(Reader in, Object desc) throws IOException {
		removeDocumentChangeListener();
		super.read(in, desc);
		//addDocumentChangeListener();
	}

	public void changedUpdate(DocumentEvent arg0) {
		System.out.println("changed: " + arg0);
		piGui.setDirty(true);
		removeDocumentChangeListener();
	}

	public void insertUpdate(DocumentEvent e) {
		piGui.setDirty(true);
		removeDocumentChangeListener();
	}

	public void removeUpdate(DocumentEvent e) {
		piGui.setDirty(true);
		removeDocumentChangeListener();
	}

	public void dirtyChanged(boolean dirty) {
		if (!dirty)
			addDocumentChangeListener();
		// Also add if dirty?
	}

	private void addDocumentChangeListener() {
		getDocument().addDocumentListener(this);
	}

	private void removeDocumentChangeListener() {
		getDocument().removeDocumentListener(this);
	}

}
