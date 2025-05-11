package dm_java;

import java.awt.*;
import javax.swing.*;

import java.awt.Dialog.ModalityType;
import java.awt.image.BufferedImage;
import javax.imageio.ImageIO;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.nio.file.Paths;

public class ProgressManager {
    private static BufferedImage backgroundImage;
    private static Dimension backgroundImageSize;
    private static ProgressDrawer progressDrawer;
    private static ProgressPanel progressPanel = null;

    private static SplashScreen splashScreen = null;
    private static Graphics splashGraphics = null;

    private static JDialog dialog = null;

    public static synchronized void show() {
        show(null);
    }

    public static synchronized void show(JFrame parent) {
        if (backgroundImage == null) {
            var path = Paths.get(".", "resources", "dm_splash.png");

            try {
                backgroundImage = ImageIO.read(path.toFile());
                backgroundImageSize = new Dimension(backgroundImage.getWidth(), backgroundImage.getHeight());
            } catch(IOException e) {
                System.err.println("Couldn't load image: " + path + "\nWill now exit.");
                System.exit(1);
            }
        }

        if (progressDrawer == null) {
            progressDrawer = new ProgressDrawer(backgroundImage);
        }

        if (splashScreen != null) return;
        if (dialog != null) return;

        splashScreen = SplashScreen.getSplashScreen();

        if (splashScreen != null) {
            splashGraphics = splashScreen.createGraphics();
            return;
        }

        dialog = new JDialog(parent);
        dialog.setModalityType(ModalityType.APPLICATION_MODAL);

        dialog.setUndecorated(true);
        dialog.getRootPane().setOpaque(false);
        dialog.getContentPane().setBackground(new Color (0, 0, 0, 0));
        dialog.setBackground(new Color (0, 0, 0, 0));

        progressPanel = new ProgressPanel(progressDrawer, backgroundImage);
        dialog.setContentPane(progressPanel);
        dialog.pack();
        dialog.setLocationRelativeTo(null);
        dialog.setAlwaysOnTop(true);

        SwingUtilities.invokeLater(() -> {
            dialog.setVisible(true);
        });
    }

    public static synchronized void hide() {
        progressPanel = null;

        if (splashScreen != null) {
            splashScreen.close();
            splashScreen = null;
            splashGraphics = null;
        }

        if (dialog != null) {
            dialog.setVisible(false);
            dialog = null;
        }
    }

    public static synchronized void setPercentage(String text, float progress) {
        show();
        progressDrawer.setPercentage(text, progress);
        redraw();
    }

    public static synchronized void redraw() {
        try {
            SwingUtilities.invokeAndWait(() -> {
                if (splashScreen != null) {
                    progressDrawer.draw(splashGraphics, backgroundImageSize.width, backgroundImageSize.height);
                    splashScreen.update();
                } else if (progressPanel != null) {
                    progressPanel.revalidate();
                    progressPanel.repaint();
                }
            });
        } catch(InterruptedException | InvocationTargetException e) {
            e.printStackTrace();
            System.out.println("Invoke interrupted.");
        }
    }
}
