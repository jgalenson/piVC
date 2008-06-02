package data_structures;

import java.util.ArrayList;

public class VerificationAtomCollection {
		
	private VerificationResult.validityT validity;
	private ArrayList<VerificationAtom> atoms;
	private String label;
	
	public VerificationAtomCollection(VerificationResult.validityT validity, ArrayList<VerificationAtom> atoms, String label) {
		this.validity = validity;
		this.atoms = atoms;
		this.label = label;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public int getNumAtoms() {
		return atoms.size();
	}
	
	public VerificationAtom getAtom(int index) {
		return atoms.get(index);
	}
	
	public String getLabel(){
		return label;
	}
	
}
