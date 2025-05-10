package dm_java;

import java.awt.*;
import javax.swing.*;

public final class LoadingError {
    private LoadingError() {}

    public static boolean ErrorHappened = false;

    public static void Error(boolean showUI, String message, String longMessage) {
        ErrorHappened = true;

        ProgressManager.hide();

        if (showUI) {
            ErrorDialog(message, longMessage);
        }
    }

    private static void ErrorDialog(String message, String longMessage) {

        // Title Label

        JLabel titleLabel = new JLabel("Failed loading files.", SwingConstants.LEFT);

        titleLabel.setFont(titleLabel.getFont().deriveFont(16f));

        // Message Label

        JLabel msgLabel = new JLabel(message, SwingConstants.LEFT);

        // Text Area

        JTextArea textArea = new JTextArea(6, 30);
        textArea.setText(longMessage);
        textArea.setEditable(false);

        JScrollPane scrollPane = new JScrollPane(textArea);

        // Panel

        JPanel panel = new JPanel();

        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));

        panel.add(leftAlignedInHorizontalBox(titleLabel));
        panel.add(Box.createRigidArea(new Dimension(0, 10)));
        panel.add(leftAlignedInHorizontalBox(msgLabel));
        panel.add(Box.createRigidArea(new Dimension(0, 10)));
        panel.add(scrollPane);

        JOptionPane.showMessageDialog(null, panel, "Loading Error!", JOptionPane.ERROR_MESSAGE);
    }

    private static Box leftAlignedInHorizontalBox(Component component) {
        Box box = Box.createHorizontalBox();
        box.add(component);
        box.add(Box.createHorizontalGlue());
        return box;
    }
}
