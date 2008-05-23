package data_structures;

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
	
/*	DEPRECATED. MAY NOT WORK. THIS SHOULD BE REMOVED. IT'S JUST BEING KEPT FOR A COUPLE OF DAYS
	public String toHTML(){
		String html = "";
		for(int implies = 0; implies < conjuncts.length; ++implies){
			for(int conjunct = 0; conjunct < conjuncts[implies].length; ++conjunct){
				Conjunct curr = conjuncts[implies][conjunct];
				html+="&nbsp;&nbsp;&nbsp;&nbsp;";
				
				boolean markAsNotInCore = false;
				if(curr.inInductiveCore!=null && !curr.inInductiveCore.booleanValue()){
					markAsNotInCore = true;
				}				
				if(markAsNotInCore){
					html+="<del>";
				}
				if(curr.status!=null){
					if(curr.status.equals(validityT.VALID)){
						html+="<font color='green'>";
					}else if(curr.status.equals(validityT.INVALID)){
						html+="<font color='red'>";						
					}
					else{
						html+="<font color='black'>";						
					}
				}else{
					html+="<font color='black'>";											
				}
				html+=sanitizeHTML(curr.str);				
				if(markAsNotInCore){
					html+="</del>";
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
	*/
}
