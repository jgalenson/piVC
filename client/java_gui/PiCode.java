import java.awt.Color;
import java.awt.Font;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;

import javax.swing.JTextArea;
import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.undo.UndoManager;

import data_structures.Location;

/**
 * A class for the code section on the left-hand side.
 */
public class PiCode extends JTextArea implements DocumentListener, DirtyChangedListener {

	public static DefaultHighlighter.DefaultHighlightPainter yellowHP = new DefaultHighlighter.DefaultHighlightPainter(Color.YELLOW);
	public static DefaultHighlighter.DefaultHighlightPainter redHP = new DefaultHighlighter.DefaultHighlightPainter(Color.RED);
	
	private static final int TAB_SIZE = 2;
	private static final Font DEFAULT_FONT = new JTextPane().getFont();
	
	private PiGui piGui;
	private boolean justLoaded;
	private UndoManager undo;
	
	public PiCode(PiGui pGui) {
		super();
		this.piGui = pGui;
		justLoaded = false;
		undo = new UndoManager();
		piGui.addDirtyChangedListener(this);
		addUndoableEditListener();
		setTabSize(TAB_SIZE);
		setFont(DEFAULT_FONT);
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
	
	private void addUndoableEditListener() {
		getDocument().addUndoableEditListener(new UndoableEditListener() {
		    public void undoableEditHappened(UndoableEditEvent e) {
		        undo.addEdit(e.getEdit());
		        piGui.undoChangeHappened(undo);
		    }
		});
	}
	
	/**
	 * Clears all current highlights and highlights the given location.
	 */
	public void highlight(Location location, DefaultHighlighter.DefaultHighlightPainter hlp) {
		removeAllHighlights();
		highlightSingleLocation(location, hlp);
	}
	
	/**
	 * Clears all current highlights and highlights the given locations.
	 */
	public void highlight(ArrayList<Location> locations, DefaultHighlighter.DefaultHighlightPainter hlp) {
		removeAllHighlights();
		for (Location location: locations)
			highlightSingleLocation(location, hlp);
	}
	
	/**
	 * Highlights a single location.
	 */
	private void highlightSingleLocation(Location location, DefaultHighlighter.DefaultHighlightPainter hlp) {
		highlightRange(location.getStartByte(), location.getEndByte(), hlp);
	}

	/**
	 * Highlights a single location.
	 */
	private void highlightRange(int start, int end, DefaultHighlighter.DefaultHighlightPainter hlp) {
        try {
        	getHighlighter().addHighlight(start, end, hlp);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Removes all current highlighting.
	 */
	public void removeAllHighlights() {
		getHighlighter().removeAllHighlights();
	}
	
	/**
	 * Undo the last change made and notify the gui
	 * that we have made a change.
	 * We return whether or not there are more edits
	 * that could be undone.
	 */
	public boolean undo() {
		undo.undo();
		piGui.undoChangeHappened(undo);
		return undo.canUndo();
	}
	
	/**
	 * Redo the last change made and notify the gui
	 * that we have made a change.
	 */
	public void redo() {
		undo.redo();
		piGui.undoChangeHappened(undo);
	}
	
	/**
	 * Called after we open a new file.  We want to clear
	 * highlighting and undo information.  We also have
	 * to reregister the undo change listener since that
	 * seems to get clearned on a call to read().
	 */
	public void openedNewFile() {
		removeAllHighlights();
		undo.discardAllEdits();
		piGui.undoChangeHappened(undo);
		addUndoableEditListener();
		setTabSize(TAB_SIZE);
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
