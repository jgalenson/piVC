import javax.swing.DefaultListSelectionModel;

/**
 * A SelectionModel for JLists to fix the bug that
 * clicking below all the elements selects the bottom element.
 * We disable all normal JList-generates selections.  Instead,
 * you must manually call select with an index to select something.
 */
class ListSelector extends DefaultListSelectionModel {

	/**
	 * Ignore default selection events because they
	 * get triggered when you click below the last element.
	 */
	@Override
	public void setSelectionInterval(int index0, int index1) {
	}

	/**
	 * Let us select things.
	 */
	public void select(int index) {
		super.setSelectionInterval(index, index);
	}

}