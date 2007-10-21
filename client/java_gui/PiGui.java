import javax.swing.*;

public class PiGui extends JFrame {

	public PiGui() {
		super("PiVC");
	}

	public static void main(String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new PiGui();
			}
		});
	}

}