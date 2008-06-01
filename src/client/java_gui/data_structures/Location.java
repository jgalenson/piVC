package data_structures;

import java.util.List;

public class Location{
	
	private class Position implements Comparable{
		private int byteNum, rowNum, colNum;

		public Position(int byteNum, int rowNum, int colNum){
			this.byteNum = byteNum;
			this.rowNum = rowNum;
			this.colNum = colNum;
		}
				
		public int compareTo(Object arg0) {
			return this.byteNum - ((Position)arg0).byteNum;
		}
		
		public int getByte(){
			return byteNum;
		}

		public int getRow(){
			return rowNum;
		}
		
		public int getCol(){
			return colNum;
		}		
	}
	
	private Position start, end;

	public Location(int startByte, int startRow, int startCol, int endByte, int endRow, int endCol) {
		start = new Position(startByte,startRow,startCol);
		end = new Position(endByte,endRow,endCol);
	}

	//hah, a private constructor
	private Location(Position start, Position end) {
		this.start = start;
		this.end = end;
	}	
	
	public static Location mergeLocations(List<Location> locations){
		Position min = null;
		Position max = null;		
		for(Location loc: locations){
			if(!loc.isDummy()){
				if(min==null){
					min = loc.start;
					max = loc.end;
				}else{
					if(loc.start.compareTo(min)<0){
						min = loc.start;
					}
					if(loc.end.compareTo(max)>0){
						max = loc.end;
					}
				}
			}
		}
		if(min==null){
			return getDummy();
		}
		return new Location(min,max);		
	}
	
	public int getStartByte() {
		return start.getByte();
	}
	
	public int getEndByte() {
		return end.getByte();
	}
	
	public int getStartRow() {
		return start.getRow();
	}
	
	public int getEndRow() {
		return end.getRow();
	}
	
	public int getStartCol() {
		return start.getCol();
	}
	
	public int getEndCol() {
		return end.getCol();
	}
	
	public boolean isDummy(){
		return start.getByte()==0 && end.getByte()==0;
	}

	public static Location getDummy(){
		return new Location(0,0,0,0,0,0);
	}
}
