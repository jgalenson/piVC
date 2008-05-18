import java.awt.*;
import java.util.*;
import javax.swing.*;
import data_structures.Conjunct;
import data_structures.RHSConjunct;
import data_structures.VerificationCondition;
import data_structures.VerificationResult.validityT;


public class PiVCPane extends JPanel{
	

	public int PADDING = 1;
	public int CONJ_INDENT = 20;
	private PiGui gui;
	private Color VALID_COLOR = new Color(51,153,51);
	private Color INVALID_COLOR = new Color(204,51,0);
	private Color UNKNOWN_COLOR = new Color(205,153,0);	
	
	public PiVCPane(PiGui gui){
		this.gui = gui;
		setBackground(Color.WHITE);
		setLayout(null);
		unsetVC();
	}
	
	private void ourRepaint(){
		JScrollPane scrollPane = gui.getVCPaneScrollPane();
		if(scrollPane!=null){
			gui.repaint();
		}else{
			repaint();
		}
	}
	
	public void setVC(VerificationCondition vc){		
		clear();
		Conjunct[][] conjs = vc.getConjuncts();
		int currY = PADDING;
		for(int implies = 0; implies<conjs.length; ++implies){
			for(int conj = 0; conj<conjs[implies].length; ++conj){
				Conjunct curr = conjs[implies][conj];
				String str = curr.str;
				JLabel l = new JLabel(str);
				if(curr instanceof RHSConjunct){
					RHSConjunct currRHS = (RHSConjunct)curr;
					if(currRHS.status==validityT.VALID){
						l.setForeground(VALID_COLOR);
					}
					else if(currRHS.status==validityT.INVALID){
						l.setForeground(INVALID_COLOR);
					}
					else{
						l.setForeground(UNKNOWN_COLOR);
					}
				}
				addJLabel(l,PADDING+CONJ_INDENT,currY);				
				if(conj!=conjs[implies].length-1){
					JLabel l2 = new JLabel(" &&");
					addJLabel(l2,PADDING+CONJ_INDENT+getWidthOfJLabel(l),currY);
				}				
				currY+=getHeightOfJLabel(l);
			}
			if(implies!=conjs.length-1){
				JLabel l = new JLabel("->");
				addJLabel(l,PADDING,currY);
				currY+=getHeightOfJLabel(l);
			}
		}
		ourRepaint();	
	}
	
	@Override
	public Dimension getMinimumSize(){
		Component[] components = getComponents();
		int maxX = 0;
		int maxY = 0;
		for(Component c: components){
			Rectangle bounds = c.getBounds();
			int thisX = bounds.x + bounds.width;
			int thisY = bounds.y + bounds.height;
			maxX = Math.max(thisX, maxX);
			maxY = Math.max(thisY, maxY);
		}		
		return new Dimension(maxX+PADDING, maxY+PADDING);		
	}

	@Override	
	public Dimension getPreferredSize(){
		return getMinimumSize();		
	}
	
	public void unsetVC(){
		clear();
		JLabel message = new JLabel("Click on a basic path to view its VC");
		addJLabel(message,0,0);
		ourRepaint();
	}
	
	public void clear(){
		Component[] components = getComponents();
		for(Component c: components){
			remove(c);
		}			
	}

	public void addJLabel(JLabel label, int x, int y){
		FontMetrics m = getFontMetrics(getFont());
		int width = m.stringWidth(label.getText());
		int height = m.getHeight();
		label.setBounds(x,y,width,height);
		add(label);
	}		
	
	public int getHeightOfJLabel(JLabel label){
		FontMetrics m = getFontMetrics(getFont());
		return m.getHeight();
	}

	public int getWidthOfJLabel(JLabel label){
		FontMetrics m = getFontMetrics(getFont());
		return m.stringWidth(label.getText());
	}
}
