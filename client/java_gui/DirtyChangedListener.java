
/**
 * Interface for things that want to know when
 * the dirty bit is changed.
 */
public interface DirtyChangedListener {
	
	/**
	 * Called when the dirty bit is changed.
	 * @param dirty the new value of the dirty bit.
	 */
	public void dirtyChanged(boolean dirty);

}
