package data_structures;

import data_structures.VerificationResult.validityT;

public class VerificationCondition {
	
	private Conjunct[][] conjuncts;
	private VerificationResult.validityT validity;
	
	public VerificationCondition(Conjunct[][] conjuncts, VerificationResult.validityT validity) {
		this.conjuncts = conjuncts;
		this.validity = validity;
	}
	
	private VerificationCondition() {
	}	
	
	public Conjunct[][] getConjuncts() {
		return conjuncts;
	}
	
	public VerificationResult.validityT getValidity() {
		return validity;
	}
	
	public String sanitizeHTML(String html){
		html = html.replaceAll("<", "&lt;");
		html = html.replaceAll(">", "&gt;");		
		return html;
	}
	
	public String toHTML(){
		String html = "";
		for(int implies = 0; implies < conjuncts.length; ++implies){
			for(int conjunct = 0; conjunct < conjuncts[implies].length; ++conjunct){
				Conjunct curr = conjuncts[implies][conjunct];
				html+="&nbsp;&nbsp;&nbsp;&nbsp;";
				if(!curr.inInductiveCore){
					html+="<i>";
				}
				if(curr instanceof RHSConjunct){
					RHSConjunct currRHS = (RHSConjunct)curr;
					if(currRHS.status == validityT.VALID){
						html+="<font color='green'>";
					}else if(currRHS.status == validityT.INVALID){
						html+="<font color='red'>";						
					}
					else{
						html+="<font color='black'>";						
					}
				}else{
					html+="<font color='black'>";											
				}
				html+=sanitizeHTML(curr.str);				
				if(!curr.inInductiveCore){
					html+="</i>";
				}				
				html += "</font>";
				if(conjunct!=conjuncts[implies].length-1){
					html+=" && <br/> ";
				}
			}	
			if(implies!=conjuncts.length-1){
				html+="<br/>-&gt;<br/>";
			}			
		}
		html+="</html>";
		return html;
	}
}
