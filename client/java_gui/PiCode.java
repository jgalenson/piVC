import java.awt.Color;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;

import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;

import data_structures.Location;

/**
 * A class for the code section on the left-hand side.
 */
public class PiCode extends JTextPane implements DocumentListener, DirtyChangedListener {
	
	private PiGui piGui;
	private boolean justLoaded;
	private DefaultHighlighter.DefaultHighlightPainter hlp = new DefaultHighlighter.DefaultHighlightPainter(Color.YELLOW);
	
	public PiCode(PiGui piGui) {
		super();
		this.piGui = piGui;
		justLoaded = false;
		piGui.addDirtyChangedListener(this);
	}
	
	/**
	 * Overloaded as a hack to avoid the following bug:
	 * Start program, load file, try to quit.  The dirty bit is true.
	 */
	@Override
	public void read(Reader in, Object desc) throws IOException {
		super.read(in, desc);
		justLoaded = true;
	}
	
	/**
	 * Clears all current highlights and highlights the given location.
	 */
	public void highlight(Location location) {
		Highlighter hl = getHighlighter();
        hl.removeAllHighlights();
		highlightSingleLocation(location);
	}
	
	/**
	 * Clears all current highlights and highlights the given locations.
	 */
	public void highlight(ArrayList<Location> locations) {
		Highlighter hl = getHighlighter();
        hl.removeAllHighlights();
		for (Location location: locations)
			highlightSingleLocation(location);
	}
	
	/**
	 * Highlights a single location.
	 */
	private void highlightSingleLocation(Location location) {
		highlightRange(location.getStartByte(), location.getEndByte());
	}

	/**
	 * Highlights a single location.
	 */
	private void highlightRange(int start, int end) {
        try {
        	getHighlighter().addHighlight(start, end, hlp);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}

	/**
	 * When something has changed, set the dirty bit to true
	 * and remove ourselves as listening for more changes.
	 * The justLoaded bit is a hack to avoid the bug mentioned
	 * in the read() comment.  After we load a file, this method
	 * gets called, and we want to ignore that.
	 */
	public void changedUpdate(DocumentEvent e) {
		if (justLoaded) {
			justLoaded = false;
		} else {
			piGui.setDirty(true);
			removeDocumentChangeListener();
		}
	}

	/**
	 * When something has changed, set the dirty bit to true
	 * and remove ourselves as listening for more changes.
	 */
	public void insertUpdate(DocumentEvent e) {
		piGui.setDirty(true);
		removeDocumentChangeListener();
	}

	/**
	 * When something has changed, set the dirty bit to true
	 * and remove ourselves as listening for more changes.
	 */
	public void removeUpdate(DocumentEvent e) {
		piGui.setDirty(true);
		removeDocumentChangeListener();
	}

	public void dirtyChanged(boolean dirty) {
		if (!dirty)
			addDocumentChangeListener();
		// Also add if dirty?  Maybe only need once we add undo.
	}

	/**
	 * We want to listen to changes in the document
	 * so we can set the dirty bit.
	 */
	private void addDocumentChangeListener() {
		getDocument().addDocumentListener(this);
	}

	/**
	 * We don't need to listen to changes in the document
	 * after the first change for efficiency's sake.
	 */
	private void removeDocumentChangeListener() {
		getDocument().removeDocumentListener(this);
	}

}
