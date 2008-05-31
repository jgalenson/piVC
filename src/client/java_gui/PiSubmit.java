import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class PiSubmit extends Dialog {

	private static final Dimension FIELD_SIZE = new Dimension(250,28);
	private static final Dimension LABEL_SIZE_WITHOUT_SUBMIT_TO = new Dimension(130,28);
	private static final Dimension LABEL_SIZE_WITH_SUBMIT_TO = new Dimension(190,28);
	
	JTextField submitToField = null; //will remain null if "submit_to_email_address" key is in ENVIRONMENT file
	JTextField userNameField;
	JTextField userEmailField;
	JTextPane comments;
	PiGui gui;
	
	public PiSubmit(PiGui parent){		
		super(parent, "Submit Program", true);
		
		Dimension LABEL_SIZE;
		if(Config.enviornmentKeyExists("submit_to_email_address")){
			LABEL_SIZE = LABEL_SIZE_WITHOUT_SUBMIT_TO;	
		}else{
			LABEL_SIZE = LABEL_SIZE_WITH_SUBMIT_TO;					
		}
		
		this.gui = parent;
		setLocationRelativeTo(parent);
		addWindowListener(new WindowListener() {
			public void windowActivated(WindowEvent arg0) {
			}
			public void windowClosed(WindowEvent arg0) {
			}
			public void windowClosing(WindowEvent arg0) {
				setVisible(false);
			}
			public void windowDeactivated(WindowEvent arg0) {
			}
			public void windowDeiconified(WindowEvent arg0) {
			}
			public void windowIconified(WindowEvent arg0) {
			}
			public void windowOpened(WindowEvent arg0) {	
			}
		});	
		BoxLayout layout = new BoxLayout(this,BoxLayout.Y_AXIS);
		this.setLayout(layout);

		JPanel userInfo = new JPanel();
		userInfo.setLayout(new BoxLayout(userInfo,BoxLayout.Y_AXIS));
		userInfo.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Your Information"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));  
		
		JPanel nameRow = new JPanel();
		nameRow.setLayout(new BoxLayout(nameRow, BoxLayout.X_AXIS));		
        JLabel userNameLabel = new JLabel("Name: ");        
        userNameLabel.setPreferredSize(LABEL_SIZE);
        userNameField = new JTextField(Config.getValue("name"));
        userNameField.setPreferredSize(FIELD_SIZE);
        nameRow.add(userNameLabel);
        nameRow.add(userNameField);
        
		JPanel emailRow = new JPanel();
		emailRow.setLayout(new BoxLayout(emailRow, BoxLayout.X_AXIS));		        
        JLabel userEmailLabel = new JLabel("Email address: ");
        userEmailLabel.setPreferredSize(LABEL_SIZE);
        userEmailField = new JTextField(Config.getValue("email_address"));
        userEmailField.setPreferredSize(FIELD_SIZE);
        emailRow.add(userEmailLabel);
        emailRow.add(userEmailField);
        
        JLabel userInfoNote = new JLabel("A confirmation of your submission will be emailed to you.");
		userInfoNote.setAlignmentX(Component.CENTER_ALIGNMENT);
		userInfoNote.setPreferredSize(new Dimension(380,22));
		
		userInfo.add(nameRow);
		userInfo.add(emailRow);
		userInfo.add(userInfoNote);
        
		JPanel submissionInfo = null;		
		if(!Config.enviornmentKeyExists("submit_to_email_address")){
			submissionInfo = new JPanel();
	        submissionInfo.setBorder(BorderFactory.createCompoundBorder(
	                BorderFactory.createTitledBorder("Submission Information"),
	                BorderFactory.createEmptyBorder(5, 5, 5, 5)));		
	        submissionInfo.setLayout(new BoxLayout(submissionInfo,BoxLayout.X_AXIS));
	        JLabel submitToLabel = new JLabel("Email address to submit to: ");
	        submitToLabel.setPreferredSize(LABEL_SIZE);
	        submitToField = new JTextField(Config.getValue("submit_to_email_address"));
	        submitToField.setPreferredSize(FIELD_SIZE);
	        submissionInfo.add(submitToLabel);
	        submissionInfo.add(submitToField);
		}
        
        
        comments = new JTextPane();
        comments.setPreferredSize(new Dimension(300,100));
        comments.setBackground(Color.WHITE);
        JScrollPane commentsScroll = new JScrollPane(comments);
        commentsScroll.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Comments (optional)"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
        
        JPanel buttons = new JPanel();
        buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
        JButton submit = new JButton("Submit");
        
        JButton cancel = new JButton("Cancel");
        cancel.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				setVisible(false);		
			}
        });
        
        submit.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				goSubmit();
			}
        });
        
        buttons.add(submit);
        buttons.add(cancel);


        JLabel alsoCompileMessage1 = new JLabel("The submission procedure will also compile the program.");
        JLabel alsoCompileMessage2 = new JLabel("Please be patient while this occurs.");
        alsoCompileMessage1.setAlignmentX(Component.CENTER_ALIGNMENT);
        alsoCompileMessage2.setAlignmentX(Component.CENTER_ALIGNMENT);        
        
		add(userInfo);
		
		if(!Config.enviornmentKeyExists("submit_to_email_address")){
			add(submissionInfo);
		}
		add(commentsScroll);

		add(alsoCompileMessage1);
		add(alsoCompileMessage2);		
		
		add(buttons);
        
		pack();
		setLocation((parent.getLocation().x+parent.getSize().width)/2-getSize().width/2,(parent.getLocation().y+parent.getSize().height)/2 - getSize().height/2);
		setResizable(false);
		setVisible(true);
	}
	
	
	
	private void goSubmit(){
		
		//I assemble a regexp to recognize valid email addresses
		//Note that escape sequences must be double-escaped. We want to a '\' character fed to the reg-ex, but this
		//requires a '\\' in the string to escape the escape character.
		String validAddrSection = "[a-zA-Z0-9\\!\\#\\$\\%\\*\\/\\?\\|\\^\\{\\}\\`\\~\\&\\'\\+\\-\\=\\_]+";
		String validLHSOrRHS = "("+validAddrSection + "\\.)*"+validAddrSection;		
		String emailAddressRegExp = validLHSOrRHS + "\\@" + validLHSOrRHS;
		
		if(!userNameField.getText().matches("\\S+")){
			JOptionPane.showMessageDialog(gui,"The 'Name' field cannot be empty.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}
		if(!userEmailField.getText().matches(emailAddressRegExp)){
			JOptionPane.showMessageDialog(gui,"The 'Email address' appears malformatted.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}
		if(submitToField!=null && !submitToField.getText().matches(emailAddressRegExp)){
			JOptionPane.showMessageDialog(gui,"The 'Email address to submit to' field appears malformated.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}		

		Config.setValue("name", userNameField.getText());
		Config.setValue("email_address", userEmailField.getText());			
		if (submitToField!=null){
			Config.setValue("submit_to_email_address", submitToField.getText());
		}
		
		setVisible(false);
		String submissionComments = null;
		if(comments.getText().length()>0){
			submissionComments = comments.getText();
		}
		gui.doCompileAndMaybeSubmit(true, submissionComments);
	}
}