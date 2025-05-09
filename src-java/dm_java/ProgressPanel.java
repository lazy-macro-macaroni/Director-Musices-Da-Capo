package dm_java;

import java.awt.*;
import java.awt.image.BufferedImage;

import javax.swing.*;

public class ProgressPanel extends JPanel {
    BufferedImage backgroundImage;
    ProgressDrawer progressDrawer;
    Dimension size;

    public ProgressPanel(ProgressDrawer progressDrawer, BufferedImage backgroundImage) {
        this.progressDrawer = progressDrawer;
        this.backgroundImage = backgroundImage;
        size = new Dimension(backgroundImage.getWidth(), backgroundImage.getHeight());
    }

    public Dimension getPreferredSize() {
        return size;
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);

        g.drawImage(backgroundImage, 0, 0, null);
        progressDrawer.draw(g, size.width, size.height);
    }
}
