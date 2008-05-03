package data_structures;

import java.util.ArrayList;

import data_structures.VerificationResult.validityT;

public class Termination {
	
	private VerificationResult.validityT validity;
	private Decreasing decreasing;
	private Nonnegative nonnegative;
	
	public Termination(VerificationResult.validityT validity, Decreasing decreasing, Nonnegative nonnegative) {
		this.validity = validity;
		this.decreasing = decreasing;
		this.nonnegative = nonnegative;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public Decreasing getDecreasing() {
		return decreasing;
	}
	
	public Nonnegative getNonnegative() {
		return nonnegative;
	}
	
	public static class Decreasing implements BasicPathHolder {

		private VerificationResult.validityT validity;
		private ArrayList<BasicPath> decreasingPaths;

		public Decreasing(validityT validity, ArrayList<BasicPath> decreasingPaths) {
			this.decreasingPaths = decreasingPaths;
			this.validity = validity;
		}
		
		public VerificationResult.validityT getValidity() {
			return validity;
		}
		
		public int getNumBasicPaths() {
			return decreasingPaths.size();
		}
		
		public BasicPath getBasicPath(int index) {
			return decreasingPaths.get(index);
		}
		
	}
	
	public static class Nonnegative {

		private VerificationResult.validityT validity;
		private ArrayList<NonnegativeVerificationCondition> nonnegativeVCs;

		public Nonnegative(validityT validity, ArrayList<NonnegativeVerificationCondition> nonnegativeVCs) {
			this.nonnegativeVCs = nonnegativeVCs;
			this.validity = validity;
		}
		
		public VerificationResult.validityT getValidity() {
			return validity;
		}
		
		public int getNumVCs() {
			return nonnegativeVCs.size();
		}
		
		public NonnegativeVerificationCondition getNonnegativeVC(int index) {
			return nonnegativeVCs.get(index);
		}
		
		public static class NonnegativeVerificationCondition {
			private VerificationResult.validityT validity;
			private VerificationCondition vc;
			private Counterexample counterexample;
			private Location location;
			
			public NonnegativeVerificationCondition(VerificationResult.validityT validity, VerificationCondition vc, Counterexample counterexample, Location location) {
				this.validity = validity;
				this.vc = vc;
				this.counterexample = counterexample;
				this.location = location;
			}
			
			public VerificationResult.validityT getValidity() {
				return validity;
			}

			public VerificationCondition getVC() {
				return vc;
			}

			public Counterexample getCounterexample() {
				return counterexample;
			}
			
			public Location getLocation() {
				return location;
			}
		}
		
	}

}
