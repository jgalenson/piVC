import javax.swing.*;

class PiGui extends JFrame {

    public PiGui() {
	
    }

    public static void main(String[] args) {
	javax.swing.SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    new PiGui();
		}
	    });
    }
    
}