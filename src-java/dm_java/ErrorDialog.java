package dm_java;

import java.awt.*;
import javax.swing.*;

public class ErrorDialog {
    private ErrorDialog() {}

    public static void ShowError(String title, String heading, String message, String longMessage) {
        ProgressManager.hide();

        // Title Label

        JLabel titleLabel = new JLabel(heading, SwingConstants.LEFT);

        titleLabel.setFont(titleLabel.getFont().deriveFont(16f));

        // Message Label

        JLabel msgLabel = new JLabel(message, SwingConstants.LEFT);

        // Text Area

        JTextArea textArea = new JTextArea(6, 30);
        textArea.setText(longMessage);
        textArea.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(textArea);

        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                scrollPane.getVerticalScrollBar().setValue(0);
            }
        });

        // Panel

        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        panel.add(leftAlignedInHorizontalBox(titleLabel));
        panel.add(Box.createRigidArea(new Dimension(0, 10)));
        panel.add(leftAlignedInHorizontalBox(msgLabel));
        panel.add(Box.createRigidArea(new Dimension(0, 10)));
        panel.add(scrollPane);

        JOptionPane.showMessageDialog(null, panel, title, JOptionPane.ERROR_MESSAGE);
    }

    private static Box leftAlignedInHorizontalBox(Component component) {
        Box box = Box.createHorizontalBox();
        box.add(component);
        box.add(Box.createHorizontalGlue());
        return box;
    }
}
