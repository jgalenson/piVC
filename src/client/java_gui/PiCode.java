import java.awt.Color;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;

import javax.swing.JEditorPane;
import javax.swing.event.CaretEvent;
import javax.swing.event.CaretListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.TabSet;
import javax.swing.text.TabStop;
import javax.swing.undo.UndoManager;

import org.syntax.jedit.JEditTextArea;
import org.syntax.jedit.tokenmarker.JavaTokenMarker;
import org.syntax.jedit.tokenmarker.PiTokenMarker;

import data_structures.Location;

/**
 * A class for the code section on the left-hand side.
 */
//Note: syntax highlighting has temporarily been disabled. Replace the line with the commented-out line to reenable
//public class PiCode extends JTextPane implements DocumentListener, DirtyChangedListener {
//public class PiCode extends TextPaneWithSyntaxHighlighting implements DocumentListener, DirtyChangedListener {

public class PiCode extends JEditTextArea implements DocumentListener, DirtyChangedListener {

	
	public static Color yellowHP = Color.YELLOW;
	public static Color redHP = Color.RED;
	
	private PiGui piGui;
	private boolean justLoaded;
	private UndoManager undo;
	
	public PiCode(PiGui pGui) {
		super();
		setTokenMarker(new PiTokenMarker());
		this.piGui = pGui;
		justLoaded = false;
		undo = new UndoManager();
		piGui.addDirtyChangedListener(this);
		initCodePane();
		//setBackground(Color.YELLOW);
		//setTabSize(4);
	}
	
	//Note: this function is only here temporarily, for debugging purposes
	//It was taken from http://forum.java.sun.com/thread.jspa?forumID=57&threadID=585006
	//It should be removed before the actual piVC release
	/*
	public void setTabSize(int charactersPerTab)
	{
		FontMetrics fm = getFontMetrics(getFont());
		int charWidth = fm.charWidth( 'w' );
		int tabWidth = charWidth * charactersPerTab;
 
		TabStop[] tabs = new TabStop[10];
 
		for (int j = 0; j < tabs.length; j++)
		{
			int tab = j + 1;
			tabs[j] = new TabStop( tab * tabWidth );
		}
 
		TabSet tabSet = new TabSet(tabs);
		SimpleAttributeSet attributes = new SimpleAttributeSet();
		StyleConstants.setTabSet(attributes, tabSet);
		int length = getDocument().getLength();
		getStyledDocument().setParagraphAttributes(0, length, attributes, false);
	}	*/
	
	/**
	 * Overloaded as a hack to avoid the following bug:
	 * Start program, load file, try to quit.  The dirty bit is true.
	 */
	//@Override
	//TODO-J: uncomment?
	public void read(BufferedReader in, Object desc) throws IOException {
		//super.read(in, desc);
		String text="";
		while(true){
			String currLine=in.readLine();
			if(currLine==null){
				break;
			}
			text+=currLine+"\n";
		}
		setText(text);
		justLoaded = true;
		//setTabSize(4);
	}
	
	/**
	 * Initializes the text pane.
	 */
	private void initCodePane() {
		// Listen for edits for undo.
		
		getDocument().addUndoableEditListener(new UndoableEditListener() {
		    public void undoableEditHappened(UndoableEditEvent e) {
		    	//style changes are done automatically by the text pane. it doesn't make sense to undo them
		    	if(!e.getEdit().getPresentationName().equals("style change")){
		    		undo.addEdit(e.getEdit());
		    		piGui.undoChangeHappened(undo);
		    	}
		    }
		});
		// Listen for selection changes so we can enable/disable cut/copy/paste.
		addCaretListener(new CaretListener() {
			public void caretUpdate(CaretEvent e) {
			    int dot = e.getDot();
			    int mark = e.getMark();
			    if (dot == mark){ // no selection
			    	piGui.codeIsSelected(false);
			    }
			    else{ // selection
			    	piGui.codeIsSelected(true);
			    }
		    	removeAllHighlights();
			}
		});
		/*
		// Listen for clicks so we can remove highlighting.
		addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				System.out.println("Here3");
				if (e.getButton() != MouseEvent.BUTTON1)
					return;
				removeAllHighlights();
			}
		});*/
	}
	
	/**
	 * Clears all current highlights and highlights the given location.
	 */
	public void highlight(Location location, Color c) {
		highlights.clear();
		highlights.add(new HighlightLocation(location,c));
		repaint();
	}
	
	/**
	 * Clears all current highlights and highlights the given locations.
	 */
	public void highlight(ArrayList<Location> locations, Color c) {
		highlights.clear();
		for (Location location: locations){
			highlights.add(new HighlightLocation(location,c));
		}
		repaint();
	}

	/**
	 * Removes all current highlighting.
	 */
	public void removeAllHighlights() {
		if(highlights.size()>0){
			highlights.clear();
			repaint();
		}
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
	}
	
	/**
	 * Clears the text.
	 */
	public void clear() {
		setText("");
	}

	/**
	 * When something has changed, set the dirty bit to true
	 * and remove ourselves as listening for more changes.
	 * The justLoaded bit is a hack to avoid the bug mentioned
	 * in the read() comment.  After we load a file, this method
	 * gets called, and we want to ignore that.
	 */
	public void changedUpdate(DocumentEvent e) {
		updateScrollBars();
		if (justLoaded) {
			justLoaded = false;
		} else {
			piGui.setDirty(true);
		}
	}		

	/**
	 * When something has changed, set the dirty bit to true
	 * and remove ourselves as listening for more changes.
	 */
	public void insertUpdate(DocumentEvent e) {
		updateScrollBars();
		piGui.setDirty(true);
	}

	/**
	 * When something has changed, set the dirty bit to true
	 * and remove ourselves as listening for more changes.
	 */
	public void removeUpdate(DocumentEvent e) {
		updateScrollBars();
		//TODO: this is a clumsy way to update the scroll bars because it iterates over ever line. if piVC is
		//ever used with large files, this might pose a problem
		piGui.setDirty(true);
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
	
	//TODO-J: uncomment?
	/**
	 * Disable line wrapping.
	 */
	//@Override
	//public boolean getScrollableTracksViewportWidth() {
	//	return false;
	//}
	
	/**
	 * Fixes ugly bug with an empty text pane and no line wrapping.
	 */
	@Override
	public void setSize(Dimension d)
	{
		if (d.width < getParent().getSize().width)
			d.width = getParent().getSize().width;
		super.setSize(d);
	}
	
	public void increaseFont() {
		painter.increaseFont();
	}
	
	public void decreaseFont() {
		painter.decreaseFont();
	}
	

}
