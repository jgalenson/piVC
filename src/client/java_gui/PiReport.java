import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

public class PiReport extends PiDialog {

	public enum ReportType {bug, feedback};
	
	private static final Dimension FIELD_SIZE = new Dimension(250,28);
	private static final Dimension LABEL_SIZE = new Dimension(130,28);
	private static final Dimension COMMENTS_SIZE = new Dimension(400,150);
	
	JTextField userNameField;
	JTextField userEmailField;
	JTextPane comments;
	ReportType type;
	PiGui gui;
	JCheckBox includeSource;
	
	//this needs to be referred to in the super() call so we make it static
	private static String getWindowTitle(ReportType type){
		if(type==ReportType.bug){
			return "Report a Bug";
		}else{
			return "Give Feedback About PiVC";
		}
	}
	private String getCommentsLabel(){
		if(type==ReportType.bug){
			return "Description of Problem";
		}else{
			return "Feedback";
		}
	}	
	private String getSubmitText(){
		if(type==ReportType.bug){
			return "Send Bug Report";
		}else{
			return "Send Feedback";
		}
	}
	
	public PiReport(PiGui parent, ReportType type){
		super(parent, getWindowTitle(type));		
		
		this.gui = parent;
		this.type = type;
		
		JPanel all = new JPanel();
		all.setLayout(new BoxLayout(all, BoxLayout.Y_AXIS));

		JPanel userInfo = new JPanel();
		userInfo.setLayout(new BoxLayout(userInfo,BoxLayout.Y_AXIS));
		userInfo.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder("Your Information (optional)"),
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
        		
		userInfo.add(nameRow);
		userInfo.add(emailRow);        
        
		JPanel commentsPanel = new JPanel();
		commentsPanel.setLayout(new BoxLayout(commentsPanel, BoxLayout.Y_AXIS));	
        commentsPanel.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createTitledBorder(getCommentsLabel()),
                BorderFactory.createEmptyBorder(5, 5, 5, 5)));
		
		comments = new JTextPane();
        comments.setBackground(Color.WHITE);
        JScrollPane commentsScroll = new JScrollPane(comments);
        commentsPanel.add(commentsScroll);
        comments.setPreferredSize(COMMENTS_SIZE);
        
        if(type==ReportType.bug){
        	includeSource = new JCheckBox("Include currently open file with bug report", true);
        	//includeSource.setPreferredSize(new Dimension(10,10));
        	JPanel includeSourceRow = new JPanel();
    		includeSourceRow.setLayout(new BoxLayout(includeSourceRow, BoxLayout.X_AXIS));
    		includeSourceRow.add(includeSource);
//    		includeSourceRow.add(new JLabel("Include currently open file with bug report"));
    		commentsPanel.add(includeSourceRow);
        }
        
        JPanel buttons = new JPanel();
        buttons.setLayout(new FlowLayout(FlowLayout.RIGHT));
        JButton submit = new JButton(getSubmitText());
        
        JButton cancel = getCancelButton();
        
        submit.addActionListener(new ActionListener(){
			public void actionPerformed(ActionEvent arg0) {
				goSubmit();
			}
        });
        
        buttons.add(submit);
        buttons.add(cancel);
    
		all.add(userInfo);
        all.add(commentsPanel);		
		all.add(buttons);		
		add(all);        
		launch();	
	}
	
	
	
	private void goSubmit(){

		if(userNameField.getText().length()>0 && !Utils.nameFieldIsWellFormatted(userNameField.getText())){
			JOptionPane.showMessageDialog(this,"The 'Name' appears malformatted.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}				
		if(userEmailField.getText().length()>0 && !Utils.emailAddressIsWellFormatted(userEmailField.getText())){
			JOptionPane.showMessageDialog(this,"The 'Email address' appears malformatted.","Error",JOptionPane.ERROR_MESSAGE);
			return;
		}
	
		Config.setValue("name", userNameField.getText());
		Config.setValue("email_address", userEmailField.getText());
		
		close();

		String commentText = null;
		if(comments.getText().length()>0){
			commentText = comments.getText();
		}
		
		gui.doReport(type, commentText, includeSource!=null && includeSource.isSelected());
	}
	public static String stringOfReportType(ReportType reportType) {
		if(reportType==ReportType.bug){
			return "bug";
		}else{
			return "feedback";
		}
	}
}