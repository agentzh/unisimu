import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

public class TextFieldTest extends JFrame {
    private JTextField textField1, textField2, textField3;
    private JPasswordField passwordField;

    public TextFieldTest() {
        super("Testing JTextField and JPasswordField");
        Container container = getContentPane();
        container.setLayout(new FlowLayout());

        textField1 = new JTextField(10);
        container.add(textField1);

        textField2 = new JTextField("Enter text here");
        container.add(textField2);

        textField3 = new JTextField("Uneditable text field", 20);
        textField3.setEditable(false);
        container.add(textField3);

        passwordField = new JPasswordField("Hidden text");
        container.add(passwordField);

        TextFieldHandler handler = new TextFieldHandler();
        textField1.addActionListener(handler);
        textField2.addActionListener(handler);
        textField3.addActionListener(handler);
        passwordField.addActionListener(handler);

        setSize(325, 100);
        setVisible(true);
    }

    public static void main (String args[]) {
        TextFieldTest application = new TextFieldTest();
        application.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    }

    private class TextFieldHandler implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            String string = "";
            if (event.getSource() == textField1)
                string = "textField1: " + event.getActionCommand();
            else if (event.getSource() == textField2)
                string = "textField2: " + event.getActionCommand();
            else if (event.getSource() == textField3)
                string = "textField3: " + event.getActionCommand();
            else if (event.getSource() == passwordField) {
                JPasswordField pwd = (JPasswordField) event.getSource();
                string = "passwordField: " +
                    new String(passwordField.getPassword());
            }
            JOptionPane.showMessageDialog(null, string);
        }
    }
}
