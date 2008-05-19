package data_structures;

public class Conjunct{
	public Conjunct(String str, boolean inInductiveCore, Location loc){
		this.str = str;
		this.inInductiveCore = inInductiveCore;
		this.loc = loc;
	}
	public final String str;
	public final boolean inInductiveCore;
	public final Location loc;
	
	@Override
	public String toString(){
		return str;
	}
}