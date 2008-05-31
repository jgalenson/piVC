import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class PiSubmit extends Dialog {

	JTextField submitToField;
	JTextField userNameField;
	JTextField userEmailField;
	JTextPane comments;
	PiGui gui;
	
	public PiSubmit(PiGui parent){
		super(parent, "Submit Program", true);
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

		JPanel userInfoWrapper = new JPanel();
		userInfoWrapper.setLayout(new BoxLayout(userInfoWrapper,BoxLayout.Y_AXIS));
		userInfoWrapper.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Your Information"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));  
		
		JPanel userInfo = new JPanel();
        userInfo.setLayout(new GridLayout(2,2));
        JLabel userNameLabel = new JLabel("Name: ");
        userNameField = new JTextField(Config.getValue("name"));
        userInfo.add(userNameLabel);
        userInfo.add(userNameField);
        JLabel userEmailLabel = new JLabel("Email address: ");
        userEmailField = new JTextField(Config.getValue("email_address"));
        userInfo.add(userEmailLabel);
        userInfo.add(userEmailField);
        
        JLabel userInfoNote = new JLabel("A confirmation of your submission will be emailed to you.");
		userInfoNote.setAlignmentX(Component.CENTER_ALIGNMENT);
		userInfoNote.setPreferredSize(new Dimension(100,22));
		userInfoWrapper.add(userInfo);
		userInfoWrapper.add(userInfoNote);
        
		JPanel submissionInfo = new JPanel();
        submissionInfo.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Submission Information"),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));		
        submissionInfo.setLayout(new GridLayout(1,2));
        JLabel submitToLabel = new JLabel("Email address to submit to: ");
        submitToField = new JTextField(Config.getValue("submit_to_email_address"));
        submitToField.setPreferredSize(new Dimension(240,28));
        submissionInfo.add(submitToLabel);
        submissionInfo.add(submitToField);
        
        
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
        
		add(userInfoWrapper);
		add(submissionInfo);
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
		
		if(userNameField.getText().length()==0){
			JOptionPane.showMessageDialog(gui,"The 'Name' field cannot be empty.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}
		if(userEmailField.getText().length()==0){
			JOptionPane.showMessageDialog(gui,"The 'Email address' field cannot be empty.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}
		if(submitToField!=null && submitToField.getText().length()==0){
			JOptionPane.showMessageDialog(gui,"The 'Email address to submit to' field cannot be empty.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}		

		Config.setValue("name", userNameField.getText());
		Config.setValue("email_address", userEmailField.getText());			
		if (submitToField!=null){//TODO-J: field will be hidden if SUBMIT_ADDR file is present, although I haven't put in this functionality yet
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