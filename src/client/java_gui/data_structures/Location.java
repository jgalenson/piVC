package data_structures;

public class Location {
	
	private int startByte, endByte, startRow, startCol, endRow, endCol;

	public Location(int startByte, int endByte, int startRow, int startCol, int endRow, int endCol) {
		this.startByte = startByte;
		this.endByte = endByte;
		this.startRow = startRow;
		this.startCol = startCol;
		this.endRow = endRow;
		this.endCol = endCol;
	}
	
	public int getStartByte() {
		return startByte;
	}
	
	public int getEndByte() {
		return endByte;
	}

}
